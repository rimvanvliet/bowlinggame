package test.scala.io.vliet.van

/**
  * Created by ruud on 11/12/2016.
  */


import io.vliet.van.BowlingGame
import org.scalatest.{Matchers, WordSpec}

import scala.Some

class BowlingGameSpec extends WordSpec with Matchers {

  "First roll" should {
    "store pins as roll1 and () as roll2" in {
      val game = new BowlingGame
      game.roll(4)
      game.frames.head.roll1 should be (4)
    }
  }
  "Second roll" should {
    "set pins as roll2 to the last rolls element" in {
      val game = new BowlingGame
      game.roll(4)
      game.roll(2)
      game.frames.head.roll1 should be(4)
      game.frames.head.roll2.getOrElse(0) should be (2)
    }
  }

  "Four roles" should {
    "be stored as two frame elements" in {
      val game = new BowlingGame
      game.roll(1)
      game.roll(2)
      game.roll(3)
      game.roll(4)
      game.frames.tail.head.roll1 should be(1)
      game.frames.tail.head.roll2.getOrElse(0) should be(2)
      game.frames.head.roll1 should be(3)
      game.frames.head.roll2.getOrElse(0) should be(4)
    }
  }

  "Score" should {
    "be sum of all roles" in {
      val game = new BowlingGame
      game.roll(1)
      game.roll(2)
      game.roll(3)
      game.roll(4)
      game.roll(5)
      game.roll(4)
      game.score should be(1 + 2 + 3 + 4 + 5 + 4)
    }
  }

  "Strike" should {
    "set 0 as roll2" in {
      val game = new BowlingGame
      game.roll(10)
      game.frames.head.roll1 should be(10)
    }
  }

  it should {
    "add next two rolls (not strikes) to the score" in {
      val game = new BowlingGame
      game.roll(10)
      game.roll(1)
      game.roll(2)
      game.score should be((10 + 1 + 2) + (1 + 2))
    }
  }

  it should {
    "add next two rolls (strike and not strike) to the score" in {
      val game = new BowlingGame
      game.roll(10)
      game.roll(10)
      game.roll(1)
      game.roll(2)
      game.roll(3)
      game.roll(4)
      game.score should be((10 + 10 + 1) + (10 + 1 + 2) + (1 + 2) + (3 + 4))
    }
  }

  it should {
    "add next two rolls (both strikes) to the score" in {
      val game = new BowlingGame
      game.roll(10)
      game.roll(10)
      game.roll(10)
      game.roll(2)
      game.roll(3)
      game.score should be((10 + 10 + 10) + (10 + 10 + 2) + (10 + 2 + 3) + (2 + 3))
    }
  }

  it should {
    "not add additional rolls if they were not played" in {
      val game = new BowlingGame
      game.roll(1)
      game.roll(1)
      game.roll(10)
      game.score should be((1 + 1) + 10)
    }
  }

  "Spare" should
    {"add next roll to the score" in {
      val game = new BowlingGame
      game.roll(9)
      game.roll(1)
      game.roll(7)
      game.roll(3)
      game.roll(1)
      game.roll(2)
      game.score should be((9 + 1 + 7) + (7 + 3 + 1) + (1 + 2))
    }
  }

  it should {
    "not add additional roll if it was not played" in {
      val game = new BowlingGame
      game.roll(1)
      game.roll(1)
      game.roll(8)
      game.roll(2)
      game.score should be((1 + 1) + (8 + 2))
    }
  }

  "Tenth frame" should {
    "give 30 points for three strikes" in {
      val game = new BowlingGame
      (1 to 18).foreach(i => game.roll(0))
      game.roll(10)
      game.roll(10)
      game.roll(10)
      game.score should be(30)
    }
  }

  it should {
    "add one roll for spare" in {
      val game = new BowlingGame
      (1 to 18).foreach(i => game.roll(0))
      game.roll(9)
      game.roll(1)
      game.roll(10)
      game.score should be(20)
    }
  }

  "Perfect game" should {
    "score 300 for 12 strikes (12 regular and 2 bonus)" in {
      val game = new BowlingGame
      (1 to 12).foreach(i => game.roll(10))
      game.score should be(300)
    }
  }
}

