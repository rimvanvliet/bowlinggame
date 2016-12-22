import vliet.io.BowlingGameFun

object bowling {

  type Roll = Int

  def scoreFun(rolls: List[Roll]): Int = scoreAcc(rolls, 0, 10)

  def scoreAcc(rolls: List[Roll], acc: Int, frames: Int): Int = (rolls, frames) match {
    case (_, 0) => acc
    case (Nil, _) => acc
    case (10 :: b1 :: b2 :: xs, _) => scoreAcc(rolls.tail, acc + 10 + b1 + b2, frames -1)
    case (r1 :: r2 :: b1 :: xs, _) if r1 + r2 == 10 => scoreAcc(rolls.tail.tail, acc + r1 + r2 + b1, frames -1)
    case (r1 :: r2 :: xs, _)  => scoreAcc(xs, acc + r1 + r2, frames -1)
    case (r1 :: nil, _) => acc + r1
    case _ => -1
  }

  if(true && 5 - 5) "waar" else "onwaar"

  val l = List(10,10,10,10,10,10,10,10,10,10)
  l.length

  scoreFun(l)

}