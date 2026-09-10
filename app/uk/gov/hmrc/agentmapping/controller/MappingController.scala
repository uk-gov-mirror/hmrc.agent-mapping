/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.agentmapping.controller

import com.mongodb.MongoWriteException
import play.api.libs.json.Json.toJson
import play.api.libs.json._
import play.api.mvc._
import uk.gov.hmrc.agentmapping.audit.AuditService
import uk.gov.hmrc.agentmapping.auth.AuthActions
import uk.gov.hmrc.agentmapping.util.RequestAwareLogging
import uk.gov.hmrc.agentmapping.connector.EnrolmentStoreProxyConnector
import uk.gov.hmrc.agentmapping.model._
import uk.gov.hmrc.agentmapping.repository._
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class MappingController @Inject() (
  repositories: MappingRepositories,
  auditService: AuditService,
  espConnector: EnrolmentStoreProxyConnector,
  cc: ControllerComponents,
  val authActions: AuthActions
)(
  implicit val ec: ExecutionContext
)
extends BackendController(cc)
with RequestAwareLogging:

  def hasEligibleEnrolments: Action[AnyContent] = authActions.authorisedWithEnrolments { implicit request => hasEligibleEnrolments =>
    Future.successful(Ok(Json.obj("hasEligibleEnrolments" -> hasEligibleEnrolments)))
  }
  end hasEligibleEnrolments

  def createMapping(arn: Arn): Action[AnyContent] = authActions.authorisedWithAgentCode { implicit request => identifiers => implicit providerId =>
    createMapping(arn, identifiers)
  }
  end createMapping

  def autoMapEnrolments(arn: Arn): Action[AnyContent] = authActions.authorisedAsSubscribedAgentWithGroup:
    implicit request =>
      authArn =>
        groupId =>
          providerId =>
            if authArn != arn then
              Future.successful(Forbidden)
            else
              for
                es3Response <- espConnector.getPrincipalEnrolments(groupId)
                identifiers = extractActiveIdentifiers(es3Response.enrolments)

                result <-
                  if identifiers.isEmpty then
                    Future.successful(NoContent)
                  else
                    createAutoMappings(arn, identifiers)(
                      using
                      ec,
                      request,
                      providerId
                    )
              yield result
  end autoMapEnrolments

  def getClientCount: Action[AnyContent] = authActions.authorisedWithProviderId { implicit request => providerId =>
    espConnector
      .getClientCount(providerId)
      .map(clientCount => Ok(Json.obj("clientCount" -> clientCount)))
  }
  end getClientCount

  /** Add mapping for the passed identifier against an ARN. The identifier key (enrolment type) determines which data store to use Returns true IF the mapping
    * already exists, false otherwise
    */
  private def createMappingInRepository(
    arn: Arn,
    identifier: Identifier,
    ggCredId: String
  )(implicit
    request: Request[Any]
  ): Future[Boolean] = repositories
    .get(identifier.enrolmentType)
    .store(arn, identifier.value)
    .map { _ =>
      auditService.sendCreateMappingAuditEvent(
        arn,
        identifier,
        ggCredId
      )
      false
    }.recover:
      case e: MongoWriteException if e.getMessage.contains("E11000") =>
        auditService.sendCreateMappingAuditEvent(
          arn,
          identifier,
          ggCredId,
          duplicate = true
        )
        logger.warn(s"Duplicated mapping attempt for ${identifier.enrolmentType}")
        true
  end createMappingInRepository

  def findSaMappings(arn: Arn): Action[AnyContent] = Action.async { implicit request =>
    authActions.authorised():
      repositories.get(LegacyAgentEnrolmentType.IRAgentReference).findBy(arn) map { matches =>
        if matches.nonEmpty then
          Ok(toJson(AgentReferenceMappings(matches))(using AgentReferenceMappings.apiWrites("saAgentReference")))
        else
          NotFound
        end if

      }
  }
  end findSaMappings

  def findAgentCodeMappings(arn: Arn): Action[AnyContent] = Action.async { implicit request =>
    authActions.authorised():
      repositories.get(LegacyAgentEnrolmentType.AgentCode).findBy(arn) map { matches =>
        if matches.nonEmpty then
          Ok(toJson(AgentReferenceMappings(matches))(using AgentReferenceMappings.apiWrites("agentCode")))
        else
          NotFound
        end if

      }

  }

  end findAgentCodeMappings

  def findMappings(
    key: String,
    arn: Arn
  ): Action[AnyContent] = Action.async { implicit request =>
    authActions.authorised():

      LegacyAgentEnrolmentType.findByDataBaseKey(key) match
        case Some(service) =>
          repositories.get(service).findBy(arn) map { matches =>
            if matches.nonEmpty then
              Ok(toJson(AgentReferenceMappings(matches))(using AgentReferenceMappings.apiWrites()))
            else
              NotFound
            end if

          }
        case None => Future.successful(BadRequest(s"No service found for the key $key"))
      end match

  }
  end findMappings

  private def createMapping(
    arn: Arn,
    identifiers: Set[Identifier]
  )(implicit
    ec: ExecutionContext,
    request: Request[AnyContent],
    providerId: String
  ): Future[Result] =

    val mappingResults: Set[Future[Boolean]] = identifiers
      .map(identifier =>
        createMappingInRepository(
          arn,
          identifier,
          providerId
        )
      )

    Future
      .sequence(mappingResults)
      .map(resultSet =>
        if resultSet.contains(false) then
          Created
        else
          Conflict
      )
  end createMapping

  private def extractActiveIdentifiers(
    enrolments: Seq[Enrolment]
  ): Set[Identifier] =
    enrolments
      .filter(_.state.equalsIgnoreCase("Activated"))
      .flatMap { enrolment =>
        LegacyAgentEnrolmentType
          .findByName(enrolment.service)
          .fold(Seq.empty[Identifier]) { service =>
            enrolment.identifiers.map(id =>
              Identifier(service, id.value)
            )
          }
      }.toSet
  end extractActiveIdentifiers

  private def createAutoMappings(
    arn: Arn,
    identifiers: Set[Identifier]
  )(
    implicit
    ec: ExecutionContext,
    request: Request[AnyContent],
    providerId: String
  ): Future[Result] =

    val mappingResults: Set[Future[Boolean]] = identifiers.map(identifier =>
      createAutoMappingInRepository(
        arn,
        identifier,
        providerId
      )
    )

    Future
      .sequence(mappingResults)
      .map { resultSet =>
        if resultSet.contains(false) then
          Created
        else
          Conflict
        end if

      }
  end createAutoMappings

  private def createAutoMappingInRepository(
    arn: Arn,
    identifier: Identifier,
    ggCredId: String
  )(
    implicit request: Request[Any]
  ): Future[Boolean] =

    repositories
      .get(identifier.enrolmentType)
      .store(
        arn,
        identifier.value,
        automapped = true
      )
      .map { _ =>
        auditService.sendCreateMappingAuditEvent(
          arn,
          identifier,
          ggCredId,
          automapped = true
        )
        false
      }
      .recover:
        case e: MongoWriteException if e.getMessage.contains("E11000") => true // duplicate — silently ignore
  end createAutoMappingInRepository

end MappingController
