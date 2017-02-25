package laughedelic.telegram.bot.laoshi

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._, Uri._, headers._, HttpMethods._
import akka.http.scaladsl.unmarshalling._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import info.mukel.telegrambot4s._, api._, methods._, models._, Implicits._


case object skritter {

  val host = Uri("http://beta.skritter.com")
  private val api = Path("/api/v0")

  val authorize = host.withPath(api / "oauth2" / "authorize")
  val token     = host.withPath(api / "oauth2" / "token")

  val vocabs = host.withPath(api / "vocabs")
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

  run()
}
