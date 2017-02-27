package laughedelic.telegram.bot.laoshi

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._, Uri._, headers._, HttpMethods._
import akka.http.scaladsl.unmarshalling._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import org.json4s._, jackson.JsonMethods._, JsonDSL._

import scala.concurrent._, duration._
import scala.util._

import info.mukel.telegrambot4s._, api._, methods._, models._, Implicits._


case object LaoshiBot extends App with TelegramBot with Polling with Commands with ChatActions with Callbacks {

  implicit val formats = DefaultFormats

  def token = credentials.botToken
  val db = DBMock

  on("/start") { implicit message => args => args match {

      case Seq(code) => {
        // FIXME: if the message.sender(==chat.id) is not a user, don't bother
        skritter.getToken(code).foreach { skritterAuth =>
          db.recordAuthInfo(message.sender, skritterAuth)

          // request(SendMessage(message.sender, s"Your new token is ${skritterAuth.token} it expires in ${skritterAuth.expiration.timeLeft.toDays} days"))

          skritter.getUserInfo(skritterAuth).foreach { json =>
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

  def vocabLookup(text: String)(implicit message: Message) = if (text.trim.nonEmpty) {
    typing

    db.authInfo(message.sender).foreach { auth =>

      skritter.api.vocabs.withAuth(auth).?(
        "q" -> text.trim
      ).get.foreach { json =>
        (json \ "Vocabs").extract[List[Vocab]]
          .filter { v =>
            v.style != "trad"
            // && v.toughness <= 10 // kind of random choice to avoid too rare characters
          }
          .sortBy { _.toughness }
          .foreach { vocab =>

            request(SendMessage(
              message.sender,
              vocab.markdown,
              parseMode = Some(ParseMode.Markdown),
              disableWebPagePreview = Some(true),
              replyMarkup = InlineKeyboardMarkup(Seq(Seq(
                // TODO: show add button only whn this word is not studied yet
                InlineKeyboardButton("➕", callbackData = s"${callback.add}${vocab.id}"),
                InlineKeyboardButton("🔊", callbackData = s"${callback.add}${vocab.id}"),
                InlineKeyboardButton("⭐️", callbackData = s"${callback.add}${vocab.id}"),
                InlineKeyboardButton("🚫", callbackData = s"${callback.add}${vocab.id}")
                // TODO: more actions: correct word, components, get audio, start/ban
              )))
            ))
          }

        // TODO: implement pagination: output only ~5 top words, then send one more message with a "More" button
      }
    }
  }

  on("/vocab", "Look up Skritter vocabulary item") { implicit message => args =>
    vocabLookup(args.mkString(" "))
  }

  on(msg => msg.text.nonEmpty) { implicit message =>
    vocabLookup(message.text.getOrElse(""))
  }

  case object callback {
    val add = "add "
    val chooseList = "chooseList "
  }

  onCallback(_.data.map(_.startsWith(callback.add)).getOrElse(false)) { implicit cbq =>

    for {
      data    <- cbq.data.map(_.stripPrefix(callback.add))
      message <- cbq.message
      auth    <- db.authInfo(message.sender)
    } yield {

      data.split('|').toList match {
        case vocabID :: _ => {

          skritter.api.vocablists.withAuth(auth).?(
            "sort" -> "custom"
          ).get.foreach { json =>

            val lists = (json \ "VocabLists").extract[List[VocabList]].filter { list =>
              !list.disabled &&
              (list.studyingMode == "adding") &&
              list.currentSection.nonEmpty
            }
            logger.debug(lists.mkString("\n"))

            val buttons = lists.map { list =>
              val args = Seq(
                vocabID,
                list.id,
                list.currentSection.get
              ).mkString("|")
              logger.debug(args)

              Seq( InlineKeyboardButton(list.name, callbackData = s"${callback.chooseList}${args}") )
            }
            // TODO: offer to create a new list
            logger.debug(buttons.toString)

            request(EditMessageReplyMarkup(
              message.chat.id,
              message.messageId,
              replyMarkup = InlineKeyboardMarkup(buttons)
            ))

            ackCallback("Now choose a list")
          }
        }
        case _ => ackCallback("Something went wrong...")
      }
    }
  }

  onCallback(_.data.map(_.startsWith(callback.chooseList)).getOrElse(false)) { implicit cbq =>
    logger.debug("\nchoosing list\n")

    for {
      data    <- cbq.data.map(_.stripPrefix(callback.chooseList))
      message <- cbq.message
      auth    <- db.authInfo(message.sender)
    } yield {

      data.split('|').toList match {
        case vocabID :: listID :: sectionID :: _ => {
          // ackCallback("Adding it to the list...")

          // def newItem(part: String): JValue = {
          //   ("id"           -> s"${auth.user}-${vocabID}-${part}") ~
          //   ("lang"         -> "zh") ~
          //   ("part"         -> part) ~
          //   ("vocabIds"     -> Seq(vocabID)) ~
          //   ("vocabListIds" -> Seq(listID)) ~
          //   ("sectionIds"   -> Seq(sectionID))
          // }

          val newRow: JObject = ("vocabId" -> vocabID)
          val sectionUri = (skritter.api.vocablists / listID / "sections" / sectionID).withAuth(auth)

          for {
            sectionJson <- sectionUri.get
            response <- sectionUri.put(
              (sectionJson \ "VocabListSection") transformField { case JField("rows", JArray(rows)) =>
                "rows" -> JArray(rows :+ newRow)
              }
            )
          } yield {
            // logger.debug(pretty(render(response)))

            request(EditMessageReplyMarkup(
              message.chat.id,
              message.messageId,
              replyMarkup = None
            ))

            ackCallback("Done!")
          }
        }
        case _ => ackCallback("Something went wrong...")
      }

    }
  }

  run()
}
