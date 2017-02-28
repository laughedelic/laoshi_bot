package laughedelic.telegram.bot.laoshi

import java.net.URL

case class Vocab(
  id: ID,
  lang: LangCode,
  style: String,
  writing: String,
  reading: Pinyin,
  toughness: Int,
  toughnessString: String,
  ilk: String,
  audio: Option[String],
  definitions: Map[LangCode, String],
  dictionaryLinks: Map[String, String]
) {

  val links: Map[String, String] = dictionaryLinks ++ Seq(
    Some("大БКРС" -> s"https://bkrs.info/slovo.php?ch=${writing}"),
    if (ilk != "char") None else
      Some("HanziDB" -> s"http://hanzidb.org/lookup?q=${writing}")
  ).flatten

  def markdown: String = {
    Seq(
      s"${writing}  🔉${reading}  💪${toughness} (${toughnessString})",

      definitions.map { case (lang, defn) =>
        s"""${flags.get(lang).getOrElse(lang + ":")} ${defn}"""
      }.mkString("\n"),

      links.collect { case (name, url) if url.nonEmpty =>
        s"[${name}](${url})"
      }.mkString(" | ")

    ).mkString("\n")
  }
}
