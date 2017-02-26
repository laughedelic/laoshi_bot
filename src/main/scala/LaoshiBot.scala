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

  on("/start") { implicit message => args => args match {

      case Seq(code) => {
        skritter.getToken(code).foreach { skritterToken =>

          request(SendMessage(message.sender, s"Your new token is ${skritterToken.token} it expires in ${skritterToken.expiration.timeLeft.toDays} days"))
        }
      }

      case _ => {
        // TODO: check if user already has a token
        request(SendMessage(
          message.sender,
          s"""
          |Click [here](${skritter.api.authorizeInit}) to sign in to Skritter and allow this bot access to your vocabulary.
          """.trim.stripMargin,
          parseMode = Some(ParseMode.Markdown),
          disableWebPagePreview = Some(true)
        ))
      }

    }
  }

  run()
}
