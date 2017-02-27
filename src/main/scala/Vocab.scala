package laughedelic.telegram.bot.laoshi

import java.net.URL

case class Vocab(
  id: ID,
  lang: LangCode,
  style: String,
  writing: String,
  reading: Pinyin,
  toughness: Double,
  definitions: Map[LangCode, String],
  dictionaryLinks: Map[String, String]
) {

  def markdown: String = {
    Seq(
      s"✍️${writing}  🔊${reading}  💪${toughness}",

      definitions.map { case (lang, defn) =>
        s"${lang}: ${defn}"
      }.mkString("\n"),

      ( dictionaryLinks +
        ("БКРС" -> s"https://bkrs.info/slovo.php?ch=${writing}")
      ).collect { case (name, url) if url.nonEmpty =>
        s"[${name}](${url})"
      }.mkString(" | ")

    ).mkString("\n")
  }
}
