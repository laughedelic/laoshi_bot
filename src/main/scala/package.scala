package laughedelic.telegram.bot

import akka.http.scaladsl.model._, Uri._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._, Uri._, headers._, HttpMethods._
import akka.http.scaladsl.unmarshalling._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import org.json4s._, jackson.JsonMethods._

import scala.concurrent._, duration._
import scala.util._

package object laoshi {

  type EpochTime = Long // seconds

  def now(): EpochTime = java.time.Instant.now().getEpochSecond()

  type TelegramID = Long

  type ID = String
  type SkritterID = ID
  type Token = String

  type LangCode = String
  type Pinyin = String

  sealed trait ChineseStyle
  case object simp extends ChineseStyle
  case object trad extends ChineseStyle

  implicit class UriOps(val uri: Uri) extends AnyVal {

    // Adds parameters to the URI
    def ?(params: (String, String)*): Uri = uri.withQuery(
      Query(
        uri.query().toMap ++ params.toMap
      )
    )

    def withToken(token: Token): Uri = uri ? ("bearer_token" -> token)

    def withAuth(auth: SkritterAuth): Uri = withToken(auth.token)

    def run(implicit ec: ExecutionContext): Future[JValue] = {

      implicit val system = ActorSystem()
      implicit val materializer = ActorMaterializer()

      for {
        response <- Http().singleRequest(HttpRequest(GET, uri)) if response.status.isSuccess
        str <- Unmarshal(response).to[String]
      } yield parse(str)
    }
  }

}
