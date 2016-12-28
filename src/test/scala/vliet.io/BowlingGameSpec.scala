package vliet.io

/**
  * Created by ruud on 11/12/2016.
  */


import org.scalatest.{Matchers, WordSpec}

class BowlingGameSpec extends WordSpec with Matchers {


  "Number of pins in a roll" should {
    "be between 0 and 10" in {
      val game = new BowlingGame
      intercept[IllegalArgumentException] { game.roll(11) }
    }
  }

  "Second roll" should {
    "not pass 10 pins for a single Frame" in {
      val game = new BowlingGame
      game.roll(8)
      intercept[IllegalArgumentException] { game.roll(3) }

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
    "add next two rolls (not strikes) to the score" in {
      val game = new BowlingGame
      game.roll(10)
      game.roll(1)
      game.roll(2)
      game.score should be((10 + 1 + 2) + (1 + 2))
    }
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
   "add next two rolls (both strikes) to the score" in {
      val game = new BowlingGame
      game.roll(10)
      game.roll(10)
      game.roll(10)
      game.roll(2)
      game.roll(3)
      game.score should be((10 + 10 + 10) + (10 + 10 + 2) + (10 + 2 + 3) + (2 + 3))
    }
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
    "score 300 for 12 strikes (10 regular and 2 bonus)" in {
      val game = new BowlingGame
      (1 to 12).foreach(i => game.roll(10))
      game.score should be(300)
    }
  }

  "Unfinished perfect game" should {
    "score 290 for 11 strikes (10 regular and 1 bonus, 1 bonus to go)" in {
      val game = new BowlingGame
      (1 to 11).foreach(i => game.roll(10))
      game.score should be (290)
    }
  }

  "A roll in finished game" should {
    "not be aloowed" in {
      val game = new BowlingGame
      (1 to 12).foreach(i => game.roll(10))
      intercept[IllegalArgumentException] { game.roll(1) }
    }
  }
}

