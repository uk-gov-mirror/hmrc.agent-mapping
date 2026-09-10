/*
 * Copyright 2026 HM Revenue & Customs
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

package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.libs.json.JsError
import play.api.libs.json.JsString
import play.api.libs.json.Json
import uk.gov.hmrc.agentmapping.model.LegacyAgentEnrolmentType

class LegacyAgentEnrolmentTypeSpec
extends AnyWordSpecLike
with Matchers:

  "LegacyAgentEnrolmentType" should:
    "map to the database key used by repository lookups" in:
      LegacyAgentEnrolmentType.IRAgentReference.getDataBaseKey shouldBe "sa"
      LegacyAgentEnrolmentType.HmrcMgdAgentRef.getDataBaseKey shouldBe "mgd"
      LegacyAgentEnrolmentType.AgentCode.getDataBaseKey shouldBe "agentcode"

    "find entries by name and database key" in:
      LegacyAgentEnrolmentType.findByName("IR-SA-AGENT") shouldBe Some(LegacyAgentEnrolmentType.IRAgentReference)
      LegacyAgentEnrolmentType.findByName("HMRC-MGD-AGNT") shouldBe Some(LegacyAgentEnrolmentType.HmrcMgdAgentRef)
      LegacyAgentEnrolmentType.findByName("unknown") shouldBe None

      LegacyAgentEnrolmentType.findByDataBaseKey("sa") shouldBe Some(LegacyAgentEnrolmentType.IRAgentReference)
      LegacyAgentEnrolmentType.findByDataBaseKey("mgd") shouldBe Some(LegacyAgentEnrolmentType.HmrcMgdAgentRef)
      LegacyAgentEnrolmentType.findByDataBaseKey("unknown") shouldBe None

    "serialise and deserialise JSON" in:
      Json.toJson(LegacyAgentEnrolmentType.IRAgentReference)(using LegacyAgentEnrolmentType.format) shouldBe JsString("IR-SA-AGENT")
      JsString("IR-SA-AGENT").as[LegacyAgentEnrolmentType](using LegacyAgentEnrolmentType.format) shouldBe LegacyAgentEnrolmentType.IRAgentReference
      JsString("unknown").validate[LegacyAgentEnrolmentType](using LegacyAgentEnrolmentType.format) shouldBe a[JsError]

end LegacyAgentEnrolmentTypeSpec
