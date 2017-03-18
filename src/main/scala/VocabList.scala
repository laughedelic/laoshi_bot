package laughedelic.telegram.bot.laoshi

case class VocabList(
  id: ID,
  name: String,
  disabled: Boolean,
  studyingMode: String,
  singleSect: Boolean,
  currentSection: Option[ID],
  sections: List[VocabListSection]
)

case class VocabListSection(
  id: ID,
  name: String,
  created: EpochTime,
  changed: EpochTime,
  // deleted: Option[Boolean],
  completed: Option[String],
  rows: List[VocabListRow]
)

case class VocabListRow(
  vocabId: ID,
  tradVocabId:  Option[ID],     // Chinese only
  studyWriting: Option[Boolean] // Japanese only
)
