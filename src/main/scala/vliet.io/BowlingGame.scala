package vliet.io

import scala.annotation.tailrec

/**
  * Created by ruud on 11/12/2016.
  */
class BowlingGame {
  type Roll = Int

  private var frames: List[Frame] = List[Frame]()

  private var bonus1: Option[Roll] = None
  private var bonus2: Option[Roll] = None

  private def isPlayed(bonus: Option[Roll]): Boolean = bonus.isDefined

  private def gamefinished = {
    frames.size == 10 &&
      frames.head.frameFinished &&
      (!frames.head.spare && !frames.head.strike ||
        frames.head.spare && isPlayed(bonus1) ||
        frames.head.strike && isPlayed(bonus2))
  }

  private case class Frame(roll1: Roll, var roll2: Option[Roll] = None) {
    def sum = roll1 + roll2.getOrElse(0)

    val strike = roll1 == 10;

    def spare = !strike && sum == 10

    def frameFinished = strike || roll2.isDefined

    def validateRoll = sum <= 10
  }

  def roll(pins: Roll) {
    require(!gamefinished, "Rolls for a finished game are not allowed")
    require(pins >= 0 && pins <= 10, "Number of pins must be between 0 and 10")
    if (frames.size > 0 && !frames.head.frameFinished) {
      frames.head.roll2 = Some(pins)
      require(frames.head.validateRoll, "Number of pins within 1 frame must not exceed 10")
    } else if (frames.size < 10) {
      frames = Frame(pins) :: frames
    } else if (!isPlayed(bonus1)) {
      bonus1 = Some(pins)
    } else {
      bonus2 = Some(pins)
    }
  }

  def score: Int = {
    @tailrec
    def sumScore(frames: List[Frame], acc: Int, bonus: (Roll, Roll)): Int = frames match {
      case Nil => acc
      case x :: xs =>
        sumScore(xs,
          acc + x.sum + {
            if (x.spare || x.strike) bonus._1 else 0
          } + {
            if (x.strike) bonus._2 else 0
          },
          (x.roll1, {
            if (x.strike) bonus._1 else x.roll2.getOrElse(0)
          }))
    }

    sumScore(frames, 0, (bonus1.getOrElse(0), bonus2.getOrElse(0)))
  }
}