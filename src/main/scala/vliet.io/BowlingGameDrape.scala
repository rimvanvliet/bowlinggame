package vliet.io

import scala.annotation.tailrec

object BowlingGameDrape {
  type Roll = Int

  case object Strike {
    def unapply(frame: Frame): Boolean = frame.isStrike
  }

  case object Spare {
    def unapply(frame: Frame): Option[(Int, Int)] = if (frame.isSpare) Some((frame.pins1, frame.pins2.get)) else None
  }

  case class Frame(pins1: Int, pins2: Option[Int] = None) {
    require(pins1 + pins2.getOrElse(0) <= 10)
    def score: Int = pins1 + pins2.getOrElse(0)
    def isStrike: Boolean = pins1 == 10
    def isSpare: Boolean = !isStrike && score == 10
  }

  def score(rolls: List[Roll]): Int = {
    require(rolls.forall(r => r >= 0 && r <= 10), "Number of pins must be between 0 and 10")

    val turns = rolls.foldLeft((Vector.empty[Frame], Option.empty[Frame])) { case ((rolls, lr), roll) =>
      lr match {
        case None if roll == 10 => (rolls :+ Frame(roll), None)
        case None => (rolls, Some(Frame(roll)))
        case Some(rl) => (rolls :+ rl.copy(pins2 = Some(roll)), None)
      }
    } match {
      case (rolls, None) => rolls
      case (rolls, Some(rl)) => rolls :+ rl
    }

    @tailrec
    def acc(frames: Seq[Frame], score: Int, frameCount: Int): Int = {
      println(f"frameCount: $frameCount score: ${score} Frames: $frames, ")
      frames match {
        case Nil => score
        case Strike() +: y +: z +: Nil if frameCount == 9 => score + 10 + y.score + z.score
        case Spare(_, _) +: y +: Nil if frameCount == 9 => score + 10 + y.score
        case Strike() +: Strike() +: y +: xs => acc(frames.tail, score + 10 + 10 + y.pins1, frameCount + 1)
        case Strike() +: Spare(_, _) +: xs => acc(frames.tail, score + 10 + 10, frameCount + 1)
        case Strike() +: x +: xs => acc(frames.tail, score + 10 + x.score, frameCount + 1)
        case Spare(_, _) +: x +: xs => acc(frames.tail, score + 10 + x.pins1, frameCount + 1)
        case x +: xs => acc(frames.tail, score + x.score, frameCount + 1)
      }
    }

    acc(turns, 0, 0)
  }
}