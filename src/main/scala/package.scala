package laughedelic.telegram.bot

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._, Uri._, headers._, HttpMethods._, ContentTypes._
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

  case object callback {
    val add = "add:"
    val audio = "audio:"
    val chooseList = "chooseList:"
    val lookup = "lookup:"
  }

  def startWith(param: String) =
    s"https://telegram.me/laoshi_bot?start=${param}"

  sealed trait ChineseStyle
  case object simp extends ChineseStyle
  case object trad extends ChineseStyle

  val parts = List("rune", "rdng", "defn", "tone")

  // TODO: add more or make a smarter check to filter out punctuation
  // val punctuation = List("。", "，", "、", "‧", "～")


  implicit class UriOps(val uri: Uri) extends AnyVal {

    def /(segment: String): Uri = uri.withPath(
      uri.path / segment
    )

    // Adds parameters to the URI
    def ?(params: (String, String)*): Uri = uri.withQuery(
      Query(
        uri.query().toMap ++ params.toMap
      )
    )

    def withToken(token: Token): Uri = uri ? ("bearer_token" -> token)

    def withAuth(auth: SkritterAuth): Uri = withToken(auth.token)

    def request(method: HttpMethod)(entity: RequestEntity)(implicit ec: ExecutionContext): Future[JValue] = {

      implicit val system = ActorSystem()
      implicit val materializer = ActorMaterializer()

      val req = HttpRequest(method, uri, entity = entity)

      for {
        response <- Http().singleRequest(req) // if response.status.isSuccess
        str <- Unmarshal(response).to[String]
      } yield parse(str)
    }


    def get(implicit ec: ExecutionContext): Future[JValue] = request(GET)(HttpEntity.Empty)

    def post(body: JValue)(implicit ec: ExecutionContext): Future[JValue] =
      request(POST)(HttpEntity(`application/json`, compact(render(body))))

    def  put(body: JValue)(implicit ec: ExecutionContext): Future[JValue] =
      request(PUT)(HttpEntity(`application/json`, compact(render(body))))
  }

  val langs = Map[String, String](
    "ar" -> "العربية",
    "da" -> "Dansk",
    "de" -> "Deutsch",
    "en" -> "English",
    "es" -> "Español",
    "fi" -> "Suomi",
    "fr" -> "Français",
    "hi" -> "हिन्दी",
    "hu" -> "Magyar",
    "it" -> "Italiano",
    "ja" -> "日本語",
    "ko" -> "한국어",
    "nl" -> "Nederlands",
    "no" -> "norsk",
    "pl" -> "Polski",
    "pt" -> "Português",
    "ro" -> "Română",
    "ru" -> "Русский",
    "sv" -> "Svenska",
    "vi" -> "Việt",
    "yue" -> "English/Cantonese",
    "zh-cn" -> "简体中文",
    "zh-tw" -> "繁體中文"
  )

  val flags = Map[String, String](
    "ar" -> "العربية",
    "da" -> "🇩🇰",
    "de" -> "🇩🇪",
    "en" -> "🇬🇧",
    "es" -> "🇪🇸",
    "fi" -> "🇫🇮",
    "fr" -> "🇫🇷",
    "hi" -> "🇮🇳",
    "hu" -> "🇭🇺",
    "it" -> "🇮🇹",
    "ja" -> "🇯🇵",
    "ko" -> "🇰🇷",
    "nl" -> "🇳🇱",
    "no" -> "🇳🇴",
    "pl" -> "🇵🇱",
    "pt" -> "🇵🇹",
    "ro" -> "🇷🇴",
    "ru" -> "🇷🇺",
    "sv" -> "🇸🇪",
    "vi" -> "🇻🇳",
    "yue" -> "🇬🇧/🇨🇳",
    "zh-tw" -> "🇹🇼",
    "zh-cn" -> "🇨🇳",
    "zh" ->    "🇨🇳"
  )



  implicit class HanStringOps(val str: String) extends AnyVal {
    import com.hankcs.hanlp._, seg.common.Term, dictionary.py.Pinyin
    import scala.collection.JavaConversions._

    def terms: List[Term] = HanLP.segment(str).toList

    def wordTerms: List[Term] = terms
      .filterNot { t =>              // No
        t.nature.name.startsWith("w") ||  // punctuation marks
        t.nature.name.startsWith("x")     // links, emails, etc.
      }

    def pinyinList: List[Pinyin] = HanLP.convertToPinyinList(str).toList

    def segments: List[(Term, List[Pinyin])] = {
      terms.map { term =>
        term -> term.word.pinyinList
      }
    }

    // def termWithPinyin(term: Term, ): String =
    //   s"${term.word} ${pys.map(_.getPinyinWithToneMark).mkString}"
    // }

    // Formats segments as strings with their pinyin (with tone marks)
    def segmentedString(delimiter: String = " / "): String =
      segments.foldLeft("") { case (acc, (term, pys)) =>

        val py = {
          val syllables = pys.filterNot(_ == Pinyin.none5)
          if (syllables.isEmpty) None
          else Some(syllables.map(_.getPinyinWithToneMark).mkString)
        }

        // if this term doesn't have pinyin it's just "word", otherwise "拼音 pīnyīn"
        val termWithPinyin = Seq(Some(term.word), py).flatten.mkString(" ")

        acc + Seq( // punctuation sticks to the previous word:
          if (term.nature.name.startsWith("w")) "" else delimiter,
          termWithPinyin
        ).mkString
      }

    // These are not related to HanLP:

    // Checking that each character is from the Unicode CJK block
    def isIdeographic: Boolean =
      str.codePoints.toArray.forall { Character.isIdeographic(_) }

    // Determines if the text contains enough Chinese to provide help for it
    def isChineseEnough(
      minWords: Int,         // minimum number Chinese words (not characters)
      minPercentage: Double  // minimum of chinese words in relation to all words
    ): Boolean = {
      val allWords = wordTerms.map(_.word)
      val cjkWords = allWords.filter(_.isIdeographic)

      (cjkWords.length >= minWords) &&
      ((cjkWords.length: Double) / allWords.length) >= minPercentage
    }
  }
}
