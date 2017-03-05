package laughedelic.telegram.bot.laoshi

import better.files._
import org.json4s._, jackson.Serialization
import scala.util._
import info.mukel.telegrambot4s.models.User

trait DB {

  def recordAuthInfo(user: User, info: SkritterAuth): Unit

  def authInfo(user: User): Option[SkritterAuth]
}

// This "DB" stores users auth tokens as JSON files
case object DBMock extends DB {

  private val db = file"db"

  implicit private val formats = Serialization.formats(NoTypeHints)

  def recordAuthInfo(user: User, info: SkritterAuth): Unit = {
    (db / user.id.toString).overwrite(
      Serialization.write(info)
    )
  }

  def authInfo(user: User): Option[SkritterAuth] = {
    Try {
      Serialization.read[SkritterAuth](
        (db / user.id.toString).contentAsString
      )
    }.toOption
  }
}
