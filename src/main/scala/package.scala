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
  type Pinyin = String

  sealed trait ChineseStyle
  case object simp extends ChineseStyle
  case object trad extends ChineseStyle

  val parts = List("rune", "rdng", "defn", "tone")

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
}
