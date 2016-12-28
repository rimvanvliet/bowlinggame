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
    require(pins1 + pins2.getOrElse(0) <= 10, "Number of pins within 1 frame must not exceed 10")

    def score: Int = pins1 + pins2.getOrElse(0)

    def isStrike: Boolean = pins1 == 10

    def isSpare: Boolean = !isStrike && score == 10
  }

  def score(rolls: List[Roll]): Int = {
    require(rolls.forall(r => r >= 0 && r <= 10), "Number of pins must be between 0 and 10")

    val frames: Seq[Frame] = rolls.foldLeft((Vector.empty[Frame], Option.empty[Roll])) {
      case ((framesAcc, unassignedRoll), roll) =>
        unassignedRoll match {
          case None if roll == 10 => (framesAcc :+ Frame(roll), None)
          case None => (framesAcc, Some(roll))
          case Some(previousRoll) => (framesAcc :+ Frame(previousRoll, Some(roll)), None)
        }
    } match {
      case (framesAcc, None) => framesAcc
      case (framesAcc, Some(rl)) => framesAcc :+ Frame(rl)
    }

    @tailrec
    def acc(frames: Seq[Frame], score: Int, frameCount: Int): Int = {
      println(f"frameCount: $frameCount score: ${score} Frames: $frames, ")
      frames match {
        case Nil =>
          require(frameCount <= 10, "Rolls for a finished game are not allowed")
          score
        case Strike() +: y +: z +: Nil if frameCount == 9 => score + 10 + y.score + z.score
        case Strike() +: y +: Nil if frameCount == 9 => score + 10 + y.score
        case Spare(_, _) +: y +: Nil if frameCount == 9 => score + 10 + y.score
        case Strike() +: Strike() +: y +: xs => acc(frames.tail, score + 10 + 10 + y.pins1, frameCount + 1)
        case Strike() +: x +: xs => acc(frames.tail, score + 10 + x.score, frameCount + 1)
        case Spare(_, _) +: x +: xs => acc(frames.tail, score + 10 + x.pins1, frameCount + 1)
        case x +: xs => acc(frames.tail, score + x.score, frameCount + 1)
      }
    }

    acc(frames, 0, 0)
  }
}