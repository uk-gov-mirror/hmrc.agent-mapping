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

package uk.gov.hmrc.agentmapping.audit

import com.google.inject.Singleton
import play.api.mvc.Request
import uk.gov.hmrc.agentmapping.audit.AgentMappingEvent.CreateMapping
import uk.gov.hmrc.agentmapping.model.Arn
import uk.gov.hmrc.agentmapping.model.Identifier
import uk.gov.hmrc.agentmapping.util.RequestSupport
import uk.gov.hmrc.play.audit.AuditExtensions.auditHeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

@Singleton
class AuditService @Inject() (val auditConnector: AuditConnector)(implicit ec: ExecutionContext):

  def sendCreateMappingAuditEvent(
    arn: Arn,
    identifier: Identifier,
    authProviderId: String,
    duplicate: Boolean = false,
    automapped: Boolean = false
  )(implicit
    request: Request[Any]
  ): Unit =
    auditEvent(
      CreateMapping,
      "create-mapping",
      Seq(
        "authProviderId" -> authProviderId,
        "identifierType" -> identifier.enrolmentType.key,
        "identifier" -> identifier.value,
        "agentReferenceNumber" -> arn.value,
        "duplicate" -> duplicate.toString,
        "automapped" -> automapped.toString
      )
    )
    ()
  end sendCreateMappingAuditEvent

  private[audit] def auditEvent(
    event: AgentMappingEvent,
    transactionName: String,
    details: Seq[(String, String)] = Seq.empty
  )(implicit
    request: Request[Any]
  ): Future[Unit] = send(
    createEvent(
      event,
      transactionName,
      details*
    )
  )
  end auditEvent

  private def createEvent(
    event: AgentMappingEvent,
    transactionName: String,
    details: (String, String)*
  )(implicit
    request: Request[Any]
  ): DataEvent =
    val hc = RequestSupport.hc
    DataEvent(
      auditSource = "agent-mapping",
      auditType = event.toString,
      tags = hc.toAuditTags(transactionName, request.path),
      detail = hc.toAuditDetails(details.map(pair => pair._1 -> pair._2)*)
    )
  end createEvent

  private def send(events: DataEvent*): Future[Unit] = Future:
    events.foreach { event =>
      Try(auditConnector.sendEvent(event))
    }

  end send

end AuditService
