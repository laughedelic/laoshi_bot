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

            // reply(message.chat.id, s"Your new token is ${skritterAuth.token} it expires in ${skritterAuth.expiration.timeLeft.toDays} days")

            skritter.getUserInfo(skritterAuth).foreach { json =>
              val username = (json \ "User" \ "name").extract[String]

              reply(s"You have successfully logged in on Skritter as ${username}")
            }
          }
        }
      }
      // no args
      case Seq() => {
        message.from.flatMap(db.authInfo) match {
          case Some(auth) => {
            reply(s"You are already authorized on Skritter")
            logger.debug(s"User ${auth.user} already is already authorized: ${auth}")
          }
          case _ => {
            reply(
              s"Click [here](${skritter.api.authorizeInit}) to sign in to Skritter and allow this bot access to your vocabulary",
              parseMode = Some(ParseMode.Markdown),
              disableWebPagePreview = Some(true)
            )
          }
        }
      }
    }
  }

  def vocabLookup(word: String)(implicit auth: SkritterAuth, message: Message) = {
    typing

    skritter.api.vocabs.withAuth(auth).?("q" -> word).get.foreach { json =>

      (json \ "Vocabs").extract[List[Vocab]]
        .filter { v =>
          v.style != "trad"
          // && v.toughness <= 10 // kind of random choice to avoid too rare characters
        }
        .sortBy { _.toughness }
        .foreach { vocab =>

          reply(
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
          )
        }

      // TODO: implement pagination: output only ~5 top words, then send one more message with a "More" button
    }
  }

  on("/lookup", "Look up Skritter vocabulary item") { implicit message => args =>
    message.from.flatMap(db.authInfo).foreach { implicit auth =>
      args.foreach(vocabLookup)
    }
  }

  // This react on the text without any commands
  on(msg => msg.text.nonEmpty && !msg.text.map(_.startsWith("/")).getOrElse(false)) { implicit message =>

    // // messages that start with 🔍 are treated as the ones with /lookup
    // if (message.text.map(_.startsWith("🔍")).getOrElse(false)) {
    //   message.from.flatMap(db.authInfo).foreach { implicit auth =>
    //     message.text.foreach(vocabLookup)
    //   }
    // } else {
      // otherwise we segment the incoming message and offer to look up words
      val text = message.text.getOrElse("")

      if (text.isChineseEnough(minWords = 2, minPercentage = 0.5)) {
        typing

        reply(
          text.segmentedString(),
          parseMode = Some(ParseMode.Markdown)
          // replyToMessageId = message.messageId,
          // replyMarkup = InlineKeyboardMarkup(Seq(Seq(
          //   InlineKeyboardButton("Lookup a word", callbackData = s"${callback.lookup}")
          // )))
        )
      }
    // }
  }

  // CALLBACKS //

  // LOOKUP callback
  onCallback(_.data.map(_.startsWith(callback.lookup)).getOrElse(false)) { implicit cbq =>
    for {
      word    <- cbq.data.map(_.stripPrefix(callback.lookup))
      message <- cbq.message
      auth    <- db.authInfo(cbq.from)
    } yield {
      ackCallback("")
      // Initial call
      if (word.isEmpty) { // setup buttons for each word

        message.text.foreach { text =>

          val words = text.split(" / ").filter(_.isIdeographic)

          logger.debug(words.toString)

          val buttons = words.map { word =>
            Seq(KeyboardButton(s"🔍${word}"))
          }

          reply("Choose a word",
            replyMarkup = ReplyKeyboardMarkup(
              buttons,
              resizeKeyboard = true,
              oneTimeKeyboard = true
            )
          )(message)
        }
      } else {

        vocabLookup(word)(auth, message)

        // request(EditMessageReplyMarkup(
        //   message.chat.id,
        //   message.messageId,
        //   replyMarkup = InlineKeyboardMarkup(Seq(Seq(
        //     InlineKeyboardButton("Lookup a word", callbackData = s"${callback.lookup}")
        //   )))
        // ))
      }
    }
  }


  // ADD callback
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

  // CHOOSE LIST callback
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

  // AUDIO callback
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
