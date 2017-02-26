package laughedelic.telegram.bot.laoshi

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._, Uri._, headers._, HttpMethods._
import akka.http.scaladsl.unmarshalling._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import org.json4s._, jackson.JsonMethods._

import scala.concurrent._, duration._
import scala.util._

import info.mukel.telegrambot4s._, api._, methods._, models._, Implicits._


case object skritter {

  val host = Uri("http://beta.skritter.com")
  private val api = Path("/api/v0")

  val authorize = host.withPath(api / "oauth2" / "authorize")
  val token     = host.withPath(api / "oauth2" / "token")

  val vocabs = host.withPath(api / "vocabs")
}

case class SkritterToken(val expiration: Deadline)(
  val user: String,
  val token: String,
  val refresh: String
) {

  def expired: Boolean = expiration.isOverdue()
}

case object SkritterToken {
  implicit val formats = DefaultFormats

  def fromJSON(json: JValue): SkritterToken =
    SkritterToken((json \ "expires_in").extract[Int].seconds.fromNow)(
      user    = (json \ "user_id").extract[String],
      token   = (json \ "access_token").extract[String],
      refresh = (json \ "refresh_token").extract[String]
    )

  implicit def unmarshaller: FromEntityUnmarshaller[SkritterToken] = {
    Unmarshaller
      .stringUnmarshaller
      .forContentTypes(ContentTypes.`application/json`)
      .map { str =>
        SkritterToken.fromJSON(parse(str))
      }
  }
}

case object LaoshiBot extends App with TelegramBot with Polling with Commands {

  def token = credentials.botToken

  on("/login") { implicit message => _ =>

    val uri = skritter.authorize.withQuery(
      Query(
        "response_type" -> "code",
        "client_id" -> credentials.skritterClientID
      )
    )

    request(SendMessage(
      message.sender,
      s"""
      |Click [here](${uri}) to sign in to Skritter and allow this bot access to your vocabulary.
      """.trim.stripMargin,
      parseMode = Some(ParseMode.Markdown),
      disableWebPagePreview = Some(true)
    ))
  }

  on("/start") { implicit message => args => args match {
      case Seq(code) => {

        val uri = skritter.token.withQuery(
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
          response <- Http().singleRequest(httpRequest) if response.status.isSuccess()
          skritterToken <- Unmarshal(response).to[SkritterToken]
        } yield {
          request(SendMessage(message.sender, s"Your new token is ${skritterToken.token} it expires in ${skritterToken.expiration.timeLeft.toDays} days"))
          // TODO: save user token to a DB
        }
      }
      case _ => request(SendMessage(message.sender, s"bad args: ${args}"))

    }
  }

  run()
}
