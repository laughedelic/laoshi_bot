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


case object LaoshiBot extends App with TelegramBot with Polling with Commands {

  def token = credentials.botToken
  val db = DBMock

  on("/start") { implicit message => args => args match {

      case Seq(code) => {
        // FIXME: if the message.sender(==chat.id) is not a user, don't bother
        skritter.getToken(code).foreach { skritterAuth =>
          db.recordAuthInfo(message.sender, skritterAuth)

          // request(SendMessage(message.sender, s"Your new token is ${skritterAuth.token} it expires in ${skritterAuth.expiration.timeLeft.toDays} days"))

          skritter.getUserInfo(skritterAuth).foreach { json =>
            implicit val formats = DefaultFormats
            val username = (json \ "User" \ "name").extract[String]

            request(SendMessage(message.sender, s"You have successfully logged in on Skritter as ${username}"))
          }
        }
      }

      case _ => {
        val auth = db.authInfo(message.sender)

        if (auth.isEmpty) {
          request(SendMessage(
            message.sender,
            s"""
            |Click [here](${skritter.api.authorizeInit}) to sign in to Skritter and allow this bot access to your vocabulary.
            """.trim.stripMargin,
            parseMode = Some(ParseMode.Markdown),
            disableWebPagePreview = Some(true)
          ))
        } else {
          request(SendMessage(message.sender, s"You are already authorized on Skritter"))
          logger.debug(s"User ${message.sender} already is already authorized: ${db.authInfo(message.sender)}")
        }
      }

    }
  }

  run()
}
