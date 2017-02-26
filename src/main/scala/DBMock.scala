package laughedelic.telegram.bot.laoshi

import better.files._
import org.json4s._, jackson.Serialization
import scala.util._

trait DB {

  def recordAuthInfo(user: TelegramID, info: SkritterAuth): Unit

  def authInfo(user: TelegramID): Option[SkritterAuth]
}

// This "DB" stores users auth tokens as JSON files
case object DBMock extends DB {

  private val db = file"db"

  implicit private val formats = Serialization.formats(NoTypeHints)

  def recordAuthInfo(user: TelegramID, info: SkritterAuth): Unit = {
    (db / user.toString).overwrite(
      Serialization.write(info)
    )
  }

  def authInfo(user: TelegramID): Option[SkritterAuth] = {
    Try {
      Serialization.read[SkritterAuth](
        (db / user.toString).contentAsString
      )
    }.toOption
  }
}
