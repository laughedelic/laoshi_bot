package laughedelic.telegram.bot.laoshi

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._, Uri._, headers._, HttpMethods._
import akka.http.scaladsl.unmarshalling._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import org.json4s._, jackson.JsonMethods._

import scala.concurrent._, duration._
import scala.util._


case class SkritterAuth(
  val user: SkritterID,
  val token: Token,
  val expiration: EpochTime,
  val refresh: Token
) {

  def expired: Boolean = expiration - now() > 0
}

case object SkritterAuth {
  implicit val formats = DefaultFormats

  def fromJSON(json: JValue): SkritterAuth =
    SkritterAuth(
      user       = (json \ "user_id").extract[SkritterID],
      token      = (json \ "access_token").extract[Token],
      expiration = (json \ "expires_in").extract[Long] + now(),
      refresh    = (json \ "refresh_token").extract[Token]
    )

  implicit def unmarshaller: FromEntityUnmarshaller[SkritterAuth] = {
    Unmarshaller
      .stringUnmarshaller
      .forContentTypes(ContentTypes.`application/json`)
      .map { str =>
        SkritterAuth.fromJSON(parse(str))
      }
  }
}

case class UserInfo(
  id: String,
  name: String,
  created: Date,
  aboutMe: String,
  country: String
)

case object skritter {

  case object api {

    val host = Uri("http://beta.skritter.com")
    private val base = host.withPath(Path("/api/v0"))

    val authorize = base / "oauth2" / "authorize"
    val token     = base / "oauth2" / "token"

    val vocabs = base / "vocabs"

    val authorizeInit = skritter.api.authorize ? (
      "response_type" -> "code",
      "client_id"     -> credentials.skritterClientID
    )

    val users = base / "users"
    def user(id: String) = base / "users" / id

    val items = base / "items"

    val vocablists = base / "vocablists"
  }

  def getToken(code: String)(implicit ec: ExecutionContext): Future[SkritterAuth] = {

    val uri = skritter.api.token.withQuery(
      Query(
        "grant_type" -> "authorization_code",
        "client_id" -> credentials.skritterClientID,
        "redirect_uri" -> credentials.skritterClientRedirect,
        "code" -> code
      )
    )

    val httpRequest = HttpRequest(GET, uri, scala.collection.immutable.Seq[HttpHeader](
      Authorization(BasicHttpCredentials(credentials.skritterClientID, credentials.skritterClientSecret))
    ))

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    for {
      response <- Http().singleRequest(httpRequest) if response.status.isSuccess
      skritterAuth <- Unmarshal(response).to[SkritterAuth]
    } yield skritterAuth
    // TODO: save user token to a DB
  }

  def getUserInfo(skritterAuth: SkritterAuth)(implicit ec: ExecutionContext): Future[JValue] = {

    val uri = skritter.api.user(skritterAuth.user).withAuth(skritterAuth)
      // "fields" -> Seq("name", "created", "aboutMe", "country", "sourceLang").mkString(",")

    uri.get
  }

}
