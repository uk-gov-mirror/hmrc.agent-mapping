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

package uk.gov.hmrc.agentmapping.repository

import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Filters.equal
import org.mongodb.scala.model.Indexes.ascending
import org.mongodb.scala.model.IndexModel
import org.mongodb.scala.model.IndexOptions
import org.mongodb.scala.result.DeleteResult
import org.mongodb.scala.result.InsertOneResult
import uk.gov.hmrc.agentmapping.util.RequestAwareLogging
import play.api.libs.json.Format
import uk.gov.hmrc.agentmapping.model.AgentReferenceMapping
import uk.gov.hmrc.agentmapping.model.Arn
import uk.gov.hmrc.crypto.Decrypter
import uk.gov.hmrc.crypto.Encrypter
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.Codecs
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import java.util.concurrent.TimeUnit
import javax.inject.Named
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

//DO NOT DELETE (even if this microservice gets decommissioned)
abstract class MappingRepository(
  collectionName: String,
  mongo: MongoComponent
)(
  implicit
  ec: ExecutionContext,
  @Named("aes") crypto: Encrypter & Decrypter
)
extends PlayMongoRepository[AgentReferenceMapping](
  mongoComponent = mongo,
  collectionName = s"agent-mapping-${collectionName.toLowerCase}",
  domainFormat = AgentReferenceMapping.databaseFormat,
  indexes = List(
    IndexModel(
      ascending("arn", "identifier"),
      IndexOptions()
        .name("arnWithIdentifier")
        .unique(true)
    ),
    IndexModel(
      ascending("arn"),
      IndexOptions()
        .name("AgentReferenceNumber")
    ),
    IndexModel(
      ascending("preCreatedDate"),
      IndexOptions()
        .name("preCreatedDate")
        .expireAfter(86400L, TimeUnit.SECONDS)
    )
  ),
  replaceIndexes = true,
  extraCodecs = List(
    Codecs.playFormatCodec(Format(Arn.arnReads, Arn.arnWrites))
  )
)
with RequestAwareLogging:

  override lazy val requiresTtlIndex = false // keep data

  def findBy(arn: Arn): Future[Seq[AgentReferenceMapping]] =
    collection.find(equal("arn", arn.value)).toFuture()

  def findAll(): Future[Seq[AgentReferenceMapping]] =
    collection.find().toFuture()

  def store(
    arn: Arn,
    identifierValue: String,
    automapped: Boolean = false
  ): Future[InsertOneResult] =
    collection
      .insertOne(AgentReferenceMapping(
        None,
        arn,
        identifierValue,
        automapped
      ))
      .toFuture()

  def deleteByArn(arn: Arn): Future[DeleteResult] =
    collection.deleteOne(equal("arn", arn.value)).toFuture()

  // This is for testing purposes only
  def deleteAll(): Future[DeleteResult] =
    collection.deleteMany(BsonDocument()).toFuture()

end MappingRepository
