package vliet.io

/**
  * Created by ruud on 21/12/2016.
  *
  */
class BowlingGameFun(rolls: List[Int] = List[Int]()) {


  def roll(pins: Int): BowlingGameFun = {
    require(pins >= 0 && pins <= 10, "Number of pins must be between 0 and 10")
    new BowlingGameFun(rolls :+ pins)
  }

  def score(): Int = {
    println(rolls)
    scoreAcc(rolls, 0, 10)
  }

  private def scoreAcc(rolls: List[Int], acc: Int, framesLeft: Int): Int = {
    if (framesLeft == 0) acc
    else rolls match {
      case Nil => acc
      case 10 :: b1 :: b2 :: xs =>
        require(xs == Nil || framesLeft > 1, "Rolls for a finished game are not allowed")
        scoreAcc(rolls.tail, acc + 10 + b1 + b2, framesLeft - 1)
      case 10 :: b1 :: Nil =>
        scoreAcc(rolls.tail, acc + 10 + b1, framesLeft - 1)
      case r1 :: r2 :: b1 :: xs if r1 + r2 == 10 =>
        require(xs == Nil || framesLeft > 1, "Rolls for a finished game are not allowed")
        scoreAcc(rolls.tail.tail, acc + r1 + r2 + b1, framesLeft - 1)
      case r1 :: r2 :: xs =>
        require(r1 + r2 <= 10, "Number of pins within 1 frame must not exceed 10")
        require(xs == Nil || framesLeft > 1, "Rolls for a finished game are not allowed")
        scoreAcc(xs, acc + r1 + r2, framesLeft - 1)
      case r1 :: Nil =>
        acc + r1
    }
  }
}
