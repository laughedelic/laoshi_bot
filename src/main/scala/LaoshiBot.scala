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

  on("/start") { implicit message => {
      // OAuth code from Skritter redirect
      case Seq(code) => {
        message.from.map { user =>

          skritter.getToken(code).foreach { skritterAuth =>
            db.recordAuthInfo(user, skritterAuth)

            // request(SendMessage(message.chat.id, s"Your new token is ${skritterAuth.token} it expires in ${skritterAuth.expiration.timeLeft.toDays} days"))

            skritter.getUserInfo(skritterAuth).foreach { json =>
              val username = (json \ "User" \ "name").extract[String]

              request(SendMessage(message.chat.id, s"You have successfully logged in on Skritter as ${username}"))
            }
          }
        }
      }
      // no args
      case _ => {
        message.from.flatMap(db.authInfo) match {
          case Some(auth) => {
            request(SendMessage(message.chat.id, s"You are already authorized on Skritter"))
            logger.debug(s"User ${auth.user} already is already authorized: ${auth}")
          }
          case _ => {
            request(SendMessage(
              message.chat.id,
              s"""
              |Click [here](${skritter.api.authorizeInit}) to sign in to Skritter and allow this bot access to your vocabulary.
              """.trim.stripMargin,
              parseMode = Some(ParseMode.Markdown),
              disableWebPagePreview = Some(true)
            ))
          }
        }
      }
    }
  }

  def vocabLookup(text: String)(implicit message: Message) = if (text.trim.nonEmpty) {

    message.from.flatMap(db.authInfo).foreach { auth =>
      typing

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
              message.chat.id,
              vocab.markdown,
              parseMode = Some(ParseMode.Markdown),
              disableWebPagePreview = Some(true),
              replyMarkup = InlineKeyboardMarkup(Seq(
                Seq(
                  // TODO: show add button only whn this word is not studied yet
                  Some(
                    InlineKeyboardButton("➕", callbackData = s"${callback.add}${vocab.id}")
                  ),
                  vocab.audio.map { url =>
                    InlineKeyboardButton("🔊", callbackData = s"${callback.audio}${vocab.id}")
                  }
                  // TODO: more actions: correct word, components, get audio, start/ban
                  // InlineKeyboardButton("⭐️", callbackData = s"${callback.add}${vocab.id}"),
                  // InlineKeyboardButton("🚫", callbackData = s"${callback.add}${vocab.id}")
                ).flatten
              ))
            ))
          }

        // TODO: implement pagination: output only ~5 top words, then send one more message with a "More" button
      }
    }
  }

  on("/vocab", "Look up Skritter vocabulary item") { implicit message => args =>
    vocabLookup(args.mkString(" "))
  }

  // This react on the text without any commands
  on(msg => msg.text.nonEmpty) { implicit message =>
    // TODO: finter out banned words
    vocabLookup(message.text.getOrElse(""))
  }

  case object callback {
    val add = "add "
    val audio = "audio "
    val chooseList = "chooseList "
  }

  onCallback(_.data.map(_.startsWith(callback.add)).getOrElse(false)) { implicit cbq =>

    for {
      data    <- cbq.data.map(_.stripPrefix(callback.add))
      message <- cbq.message
      auth    <- db.authInfo(cbq.from)
    } yield {

      data.split('|').toList match {
        case vocabId :: _ => {

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
                vocabId,
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
      auth    <- db.authInfo(cbq.from)
    } yield {

      data.split('|').toList match {
        case vocabId :: listID :: sectionID :: _ => {
          // ackCallback("Adding it to the list...")

          // def newItem(part: String): JValue = {
          //   ("id"           -> s"${auth.user}-${vocabId}-${part}") ~
          //   ("lang"         -> "zh") ~
          //   ("part"         -> part) ~
          //   ("vocabIds"     -> Seq(vocabId)) ~
          //   ("vocabListIds" -> Seq(listID)) ~
          //   ("sectionIds"   -> Seq(sectionID))
          // }

          val newRow: JObject = ("vocabId" -> vocabId)
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

  onCallback(_.data.map(_.startsWith(callback.audio)).getOrElse(false)) { implicit cbq =>

    for {
      data    <- cbq.data.map(_.stripPrefix(callback.audio))
      message <- cbq.message
      auth    <- db.authInfo(cbq.from)
    } yield {

      data.split('|').toList match {
        case vocabId :: _ => {

          uploadingAudio(message)

          skritter.api.vocabs.withAuth(auth).?(
            "ids" -> vocabId
          ).get.foreach { json =>

            (json \ "Vocabs").extract[List[Vocab]]
              .filter(_.audio.nonEmpty)
              .headOption
              .foreach { vocab =>

                // TODO: convert it to .ogg voice messages
                request(SendAudio(
                  message.chat.id,
                  vocab.audio.get, // FIXME
                  replyToMessageId = message.messageId
                ))

                ackCallback(s"Pronunciation for ${vocab.writing} 🔉${vocab.reading} is uploaded!")
              }
          }
        }
        case _ => ackCallback("Something went wrong...")
      }
    }
  }

  run()
}
