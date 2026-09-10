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

package uk.gov.hmrc.agentmapping.auth

import uk.gov.hmrc.agentmapping.util.RequestAwareLogging
import play.api.mvc.*
import uk.gov.hmrc.agentmapping.model.Arn
import uk.gov.hmrc.agentmapping.model.BasicAuthentication
import uk.gov.hmrc.agentmapping.model.Identifier
import uk.gov.hmrc.agentmapping.model.LegacyAgentEnrolmentType
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core.AuthProvider.PrivilegedApplication
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals.*
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.http.HeaderNames
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.matching.Regex

@Singleton
class AuthActions @Inject() (
  val authConnector: AuthConnector,
  cc: ControllerComponents
)
extends BackendController(cc)
with AuthorisedFunctions
with RequestAwareLogging:

  private type HasEligibleEnrolments = Boolean
  private type ProviderId = String

  def authorisedWithEnrolments(
    body: Request[AnyContent] => HasEligibleEnrolments => Future[Result]
  )(
    implicit ec: ExecutionContext
  ): Action[AnyContent] = Action.async { implicit request =>
    authorised(AuthProviders(GovernmentGateway) and AffinityGroup.Agent)
      .retrieve(allEnrolments)(allEnrolments => body(request)(captureIdentifiersFrom(allEnrolments).nonEmpty))
      .recover { case _: AuthorisationException => Unauthorized }
  }
  end authorisedWithEnrolments

  def authorisedWithAgentCode(
    body: Request[AnyContent] => Set[Identifier] => ProviderId => Future[Result]
  )(
    implicit ec: ExecutionContext
  ): Action[AnyContent] = Action.async { implicit request =>
    authorised(AuthProviders(GovernmentGateway) and AffinityGroup.Agent)
      .retrieve(credentials and agentCode and allEnrolments) {
        case Some(Credentials(providerId, _)) ~ agentCodeOpt ~ allEnrolments =>

          captureIdentifiersAndAgentCodeFrom(allEnrolments, agentCodeOpt) match
            case Some(identifiers) => body(request)(identifiers)(providerId)
            case _ => Future.successful(Forbidden)
          end match

        case _ =>
          logger.warn(s"error - missing credentials")
          Future.successful(Forbidden)
      }.recover:
        case _: NoActiveSession => Unauthorized
        case _: AuthorisationException => Forbidden
  }
  end authorisedWithAgentCode

  def authorisedWithProviderId(
    body: Request[AnyContent] => String => Future[Result]
  )(
    implicit ec: ExecutionContext
  ): Action[AnyContent] = Action.async { implicit request =>
    authorised(AuthProviders(GovernmentGateway) and AffinityGroup.Agent)
      .retrieve(credentials) {
        case Some(Credentials(providerId, _)) => body(request)(providerId)
        case _ => Future.successful(Forbidden)
      }.recover:
        case _: NoActiveSession => Unauthorized
        case _: AuthorisationException => Forbidden
  }
  end authorisedWithProviderId

  def authorisedAsSubscribedAgent(
    body: Request[AnyContent] => Arn => Future[Result]
  )(
    implicit ec: ExecutionContext
  ): Action[AnyContent] = Action.async { implicit request =>
    authorised(
      Enrolment("HMRC-AS-AGENT")
        and AuthProviders(GovernmentGateway)
    ).retrieve(authorisedEnrolments) { enrolments =>
      val arnOpt = getEnrolmentInfo(
        enrolments.enrolments,
        "HMRC-AS-AGENT",
        "AgentReferenceNumber"
      )

      arnOpt match
        case Some(arn) => body(request)(Arn(arn))
        case None => Future.successful(Forbidden)
      end match

    }.recover:
      case _: NoActiveSession => Unauthorized
      case _: AuthorisationException => Forbidden
  }
  end authorisedAsSubscribedAgent

  def authorisedAsSubscribedAgentWithGroup(
    body: Request[AnyContent] => Arn => String => String => Future[Result]
  )(
    implicit ec: ExecutionContext
  ): Action[AnyContent] = Action.async { implicit request =>
    authorised(
      Enrolment("HMRC-AS-AGENT") and AuthProviders(GovernmentGateway)
    ).retrieve(authorisedEnrolments and groupIdentifier and credentials) {
      case enrolments ~ maybeGroupId ~ maybeCreds =>

        val maybeArn = getEnrolmentInfo(
          enrolments.enrolments,
          "HMRC-AS-AGENT",
          "AgentReferenceNumber"
        )

        (maybeArn, maybeGroupId, maybeCreds) match
          case (Some(arn), Some(groupId), Some(creds)) => body(request)(Arn(arn))(groupId)(creds.providerId)
          case _ => Future.successful(Forbidden)
        end match

    }.recover:
      case _: NoActiveSession => Unauthorized
      case _: AuthorisationException => Forbidden
  }
  end authorisedAsSubscribedAgentWithGroup

  def basicAuth(
    body: Request[AnyContent] => Future[Result]
  )(
    implicit ec: ExecutionContext
  ): Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      body(request)
    } recover { case _: NoActiveSession => Unauthorized }
  }
  end basicAuth

  private val basicAuthHeader: Regex = "Basic (.+)".r
  private val decodedAuth: Regex = "(.+):(.+)".r

  private def decodeFromBase64(encodedString: String): String =

    try new String(Base64.getDecoder.decode(encodedString), UTF_8)
    catch
      case _: Throwable => ""
    end try

  end decodeFromBase64

  def withBasicAuth(
    expectedAuth: BasicAuthentication
  )(
    body: => Future[Result]
  )(
    implicit request: Request[?]
  ): Future[Result] =
    request.headers.get(HeaderNames.authorisation) match
      case Some(basicAuthHeader(encodedAuthHeader)) =>
        decodeFromBase64(encodedAuthHeader) match
          case decodedAuth(username, password) =>

            if BasicAuthentication(username, password) == expectedAuth then
              body
            else
              logger.warn("Authorization header found in the request but invalid username or password")
              Future successful Unauthorized
            end if

          case _ =>
            logger.warn("Authorization header found in the request but its not in the expected format")
            Future successful Unauthorized
        end match

      case _ =>
        logger.warn("No Authorization header found in the request for agent termination")
        Future successful Unauthorized
    end match

  end withBasicAuth

  def onlyStride(
    strideRole: String
  )(
    body: Request[AnyContent] => Future[Result]
  )(
    implicit ec: ExecutionContext
  ): Action[AnyContent] = Action.async { implicit request =>
    authorised(AuthProviders(PrivilegedApplication))
      .retrieve(allEnrolments):
        case allEnrols if allEnrols.enrolments.map(_.key).contains(strideRole) => body(request)
        case e =>
          logger.warn(
            s"Unauthorized Discovered during Stride Authentication: ${e.enrolments.map(enrol => enrol.key).mkString(",")}"
          )
          Future successful Unauthorized
      .recover { case e =>
        logger.warn(s"Error Discovered during Stride Authentication: ${e.getMessage}")
        Forbidden
      }
  }
  end onlyStride

  private def getEnrolmentInfo(
    enrolment: Set[Enrolment],
    enrolmentKey: String,
    identifier: String
  ): Option[String] = enrolment
    .find(_.key.equals(enrolmentKey))
    .flatMap(
      _.identifiers
        .find(_.key.equals(identifier))
        .map(_.value)
    )
  end getEnrolmentInfo

  private def captureIdentifiersAndAgentCodeFrom(
    enrolments: Enrolments,
    agentCodeOpt: Option[String]
  ): Option[Set[Identifier]] =
    val identifiers = captureIdentifiersFrom(enrolments)
    if identifiers.isEmpty then
      None
    else
      Some(
        agentCodeOpt match {
          case None => identifiers
          case Some(ac) => identifiers + Identifier(LegacyAgentEnrolmentType.AgentCode, ac)
        }
      )
    end if
  end captureIdentifiersAndAgentCodeFrom

  private def captureIdentifiersFrom(enrolments: Enrolments): Set[Identifier] =

    case class AgentEnrolment(
      legacyAgentEnrolmentType: LegacyAgentEnrolmentType,
      values: Seq[String]
    )

    enrolments.enrolments
      .map(enrolment =>
        LegacyAgentEnrolmentType
          .findByName(enrolment.key)
          .map(eType => AgentEnrolment(eType, enrolment.identifiers.map(_.value)))
      )
      .collect:
        // We only use the FIRST value; since all the enrolments we care about are single valued
        case Some(AgentEnrolment(enrolmentType, Seq(enrolmentIdentifierValue))) => Identifier(enrolmentType, enrolmentIdentifierValue)

  end captureIdentifiersFrom

end AuthActions
