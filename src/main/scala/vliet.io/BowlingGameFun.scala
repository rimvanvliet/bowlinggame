package vliet.io

/**
  * Created by ruud on 21/12/2016.
  *
  * Pure functional implementation:
  */
object BowlingGameFun {

  type Roll = Int

  def score(rolls: List[Roll]): Int = {
    require(rolls.forall(r => r >= 0 && r <= 10), "Number of pins must be between 0 and 10")
    scoreAcc(rolls, 0, 10)
  }

  private def scoreAcc(rolls: List[Roll], acc: Int, frames: Int): Int = {
    if (frames == 0) acc
    else rolls match {
      case Nil => acc
      case 10 :: b1 :: b2 :: xs =>
        require(xs == Nil || frames > 1, "Rolls for a finished game are not allowed")
        scoreAcc(rolls.tail, acc + 10 + b1 + b2, frames - 1)
      case 10 :: b1 :: Nil =>
        scoreAcc(rolls.tail, acc + 10 + b1, frames - 1)
      case r1 :: r2 :: b1 :: xs if r1 + r2 == 10 =>
        require(xs == Nil || frames > 1, "Rolls for a finished game are not allowed")
        scoreAcc(rolls.tail.tail, acc + r1 + r2 + b1, frames - 1)
      case r1 :: r2 :: xs =>
        require(r1 + r2 <= 10, "Number of pins within 1 frame must not exceed 10")
        require(xs == Nil || frames > 1, "Rolls for a finished game are not allowed")
        scoreAcc(xs, acc + r1 + r2, frames - 1)
      case r1 :: Nil =>
        acc + r1
    }
  }
}
