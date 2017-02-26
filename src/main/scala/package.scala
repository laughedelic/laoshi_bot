package laughedelic.telegram.bot

package object laoshi {

  type EpochTime = Long // seconds

  def now(): EpochTime = java.time.Instant.now().getEpochSecond()

  type TelegramID = Long
  type SkritterID = String
  type Token = String
}
