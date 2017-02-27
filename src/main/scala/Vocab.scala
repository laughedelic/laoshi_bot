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
  definitions: Map[LangCode, String],
  dictionaryLinks: Map[String, String]
) {

  def markdown: String = {
    Seq(
      s"✍️${writing}  🔊${reading}  💪${toughness} (${toughnessString})",

      definitions.map { case (lang, defn) =>
        s"""${flags.get(lang).getOrElse(lang + ":")} ${defn}"""
      }.mkString("\n"),

      ( dictionaryLinks +
        ("大БКРС" -> s"https://bkrs.info/slovo.php?ch=${writing}")
      ).collect { case (name, url) if url.nonEmpty =>
        s"[${name}](${url})"
      }.mkString(" | ")

    ).mkString("\n")
  }
}
