package laughedelic.telegram.bot.laoshi

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._, Uri._, headers._, HttpMethods._
import akka.http.scaladsl.unmarshalling._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import org.json4s._, jackson.JsonMethods._, JsonDSL._

import java.net.URLEncoder

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

          val inlineAction = if (vocab.writing.length == 1) "🔍 Words" else "⚛ Split"

          val basicButtons = Seq(
            InlineKeyboardButton("➕ Add", callbackData = s"${callback.add}${vocab.id}"),
            InlineKeyboardButton(inlineAction, switchInlineQueryCurrentChat = vocab.writing.toSeq.mkString(" "))
          )

          val audioButtons = vocab.reading.split(""",\s*""").map { reading =>
            InlineKeyboardButton(s"🔊 ${reading.toneNumbersToMarks}", callbackData = s"${callback.audio}${vocab.id}|${reading}")
          }

          val allButtons = basicButtons ++ audioButtons

          val buttonsLayout: Seq[Seq[InlineKeyboardButton]] =
            if (allButtons.length < 4) Seq(allButtons)
            else if (allButtons.length % 2 == 0) allButtons.grouped(allButtons.length / 2).toSeq
            else if (allButtons.length % 3 == 0) allButtons.grouped(allButtons.length / 3).toSeq
            else                                 allButtons.grouped(3).toSeq

          // TODO: more actions: correct word, components, get audio, start/ban
          // InlineKeyboardButton("⭐️", callbackData = s"${callback.add}${vocab.id}"),
          // InlineKeyboardButton("🚫", callbackData = s"${callback.add}${vocab.id}")

          reply(
            vocab.markdown,
            parseMode = Some(ParseMode.Markdown),
            disableWebPagePreview = Some(true),
            replyMarkup = InlineKeyboardMarkup(buttonsLayout)
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

  // This reacts on the text without any commands
  on(msg => !msg.text.map(_.startsWith("/")).getOrElse(false)) { implicit message =>

    // // messages that start with 🔍 are treated as the ones with /lookup
    // if (message.text.map(_.startsWith("🔍")).getOrElse(false)) {
    //   message.from.flatMap(db.authInfo).foreach { implicit auth =>
    //     message.text.foreach(vocabLookup)
    //   }
    // } else {

    // otherwise we segment the incoming message and offer to look up words
    val text = message.text.getOrElse("")

    val allWords = text.wordTerms.map(_.word)
    val cjkWords = allWords.filter(_.isIdeographic)

    if ( // only one word and it's Chinese
      allWords.length == 1 &&
      cjkWords.length == 1
    ) {
      typing

      message.from.flatMap(db.authInfo).foreach { implicit auth =>
        cjkWords.headOption.foreach(vocabLookup)
      }
    } else if ( // it's a text with
       (cjkWords.length > 1) &&                            // more than 1 Chinese word
      ((cjkWords.length: Double) / allWords.length) > 0.5  // more than a half of words are Chinese
    ) {
      typing

      reply(
        text.segmentedString(),
        parseMode = Some(ParseMode.Markdown),
        replyToMessageId = message.messageId,
        replyMarkup = InlineKeyboardMarkup(Seq(Seq(
          InlineKeyboardButton("Lookup a word", switchInlineQueryCurrentChat = text)
        )))
      )

    } else {

      reply("Not enough 中文 in your message!",
        replyMarkup = ReplyKeyboardRemove()
      )
    }
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
        case List(vocabId, readings) => readings.split(""",\s*""").toList match {
          case Nil => ackCallback("There is no recorded audio for this")
          // single reading:
          case List(reading) => {

            uploadingAudio(message)

            skritter.api.vocabs.withAuth(auth).?(
              "q" -> reading
            ).get.foreach { json =>

              val audioOpt = (json \ "Vocabs").extract[List[Vocab]]
                .flatMap(_.audios)
                .filter { _.reading == reading }
                .headOption
                // .map(_.mp3)

              audioOpt match {
                case None =>
                  ackCallback(s"There is no recorded audio for ${reading.toneNumbersToMarks} 😟")
                case Some(audio) => {

                  // TODO: convert it to .ogg voice messages
                  request(SendAudio(
                    message.chat.id,
                    audio.mp3,
                    // performer = audio.source,
                    // title = audio.reading.toneNumbersToMarks,
                    caption = audio.reading.toneNumbersToMarks,
                    replyToMessageId = message.messageId
                  ))

                  ackCallback(s"Audio for 🔉${reading.toneNumbersToMarks} is uploaded!")
                }
              }
            }
          }
          // // multiple readings:
          // case readings => {
          //   ackCallback(s"Choose pronunciation")
          //
          //   val buttons = readings.map { reading =>
          //     Seq(InlineKeyboardButton(
          //       s"🔉${reading.toneNumbersToMarks}",
          //       callbackData = s"${callback.audio}${vocabId}|${reading}"
          //     ))
          //   }
          //
          //   request(EditMessageReplyMarkup(
          //     message.chat.id,
          //     message.messageId,
          //     replyMarkup = InlineKeyboardMarkup(buttons)
          //   ))
          // }
        }
        case _ => ackCallback("Something went wrong...")
      }
    }
  }


  def inlineQueryResultLookup(word: String): InlineQueryResult = InlineQueryResultArticle(word,
    s"${word}",
    InputTextMessageContent(s"/lookup ${word}"),
    description = word.pinyinList.mkString.toneNumbersToMarks,
    thumbUrl = ""
    // word.headOption.map { c =>
    //   val character = URLEncoder.encode(c.toString, "UTF-8")
    //   s"""https://upload.wikimedia.org/wikipedia/commons/0/01/${character}-red.png"""
    // } //.getOrElse("")
  )

  override def onInlineQuery(iq: InlineQuery) = if (iq.query.trim.nonEmpty) {
    def answer(results: Seq[InlineQueryResult]) = request(AnswerInlineQuery(iq.id, results))

    val cjkWords = iq.query.wordTerms.map(_.word).filter(_.isIdeographic).distinct

    // val results: Seq[InlineQueryResult] =
    if (cjkWords.isEmpty) answer(Seq())
    else cjkWords match {
      // case List(singleWord) => {
        // TODO: links for this word
      case List(character) if character.length == 1 => {

        db.authInfo(iq.from).foreach { implicit auth =>

          skritter.api.vocabs.withAuth(auth).?(
            "q" -> character,
            "include_containing" -> true.toString,
            "fields" -> "writing"
          ).get.foreach { json =>
            val containingWords = (json \ "ContainingVocabs" \ "writing").extract[List[String]]

            answer( containingWords.map(inlineQueryResultLookup) )
          }
        }
      }
      case words => answer( words.map(inlineQueryResultLookup) )
    }
  }

  run()
}
