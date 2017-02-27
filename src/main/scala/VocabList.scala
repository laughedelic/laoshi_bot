package laughedelic.telegram.bot.laoshi

case class VocabList(
  id: ID,
  name: String,
  disabled: Boolean,
  studyingMode: String,
  currentSection: Option[ID]
)
