package vliet.io

/**
  * Created by ruud on 11/12/2016.
  */
class BowlingGame {

  var frames: List[Frame] = List[Frame]()

  private var bonus1: Option[Int] = None
  private var bonus2: Option[Int] = None
  private def isPlayed(bonus: Option[Int]): Boolean = bonus.isDefined

  private def gamefinished = {
    frames.size == 10 &&
      frames.head.frameFinished &&
      ( ! frames.head.spare && ! frames.head.strike ||
        frames.head.spare && isPlayed(bonus1) ||
        frames.head.strike && isPlayed(bonus2) )
  }

  case class Frame(roll1: Int, var roll2: Option[Int] = None) {
    def sum = roll1 + roll2.getOrElse(0)
    val strike = roll1 == 10;
    def spare = roll2 match {
      case Some(pins) => roll1 + pins == 10
      case None => false
    }
    def frameFinished = roll2 match {
      case Some(pins) => true
      case None => strike
    }
  }

  def roll(pins: Int) {
    require(! gamefinished, "Rolls for a finished game are not allowed")
    require(pins >= 0 && pins <= 10, "Number of pins must be between 0 and 10")
    if (frames.size > 0 && !frames.head.frameFinished) {
      require(pins <= 10 - frames.head.roll1, "Number of pins within 1 frame must not exceed 10")
      frames.head.roll2 = Some(pins)
    } else if (frames.size < 10) {
      frames = Frame(pins) :: frames
    } else if (!isPlayed(bonus1)) {
      bonus1 = Some(pins)
    } else {
      bonus2 = Some(pins)
    }
  }

  def score: Int = {
    def sumScore(frames: List[Frame], bonus: (Int, Int)): Int = frames match {
      case Nil => 0
      case x :: xs => x.sum +
        { if (x.spare || x.strike) bonus._1 else 0 } +
        { if (x.strike) bonus._2 else 0 } +
        sumScore(xs, (x.roll1,  { if (x.strike) bonus._1 else x.roll2.getOrElse(0) } ))
    }
    sumScore(frames, (bonus1.getOrElse(0), bonus2.getOrElse(0)))
  }
}