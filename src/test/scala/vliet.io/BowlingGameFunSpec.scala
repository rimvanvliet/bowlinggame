package vliet.io

/**
  * Created by ruud on 11/12/2016.
  */


import org.scalatest.{Matchers, WordSpec}

class BowlingGameFunSpec extends WordSpec with Matchers {

  "Number of pins in a roll" should {
    "be between 0 and 10" in {
      intercept[IllegalArgumentException] {
        new BowlingGameFun().roll((11))
      }
    }
  }

  "Second roll" should {
    "not pass 10 pins for a single Frame" in {
      val game = new BowlingGameFun()
        .roll(8)
        .roll(3)
      intercept[IllegalArgumentException] {
        game.score()
      }
    }
  }

  "Score" should {
    "be sum of all roles" in {
      val game = new BowlingGameFun().roll(1)
        .roll(2)
        .roll(3)
        .roll(4)
        .roll(5)
        .roll(4)
      game.score should be(1 + 2 + 3 + 4 + 5 + 4)
    }
  }

  "Strike" should {
    "add next two rolls (not strikes) to the score" in {
      val game = new BowlingGameFun()
        .roll(10)
        .roll(1)
        .roll(2)
      game.score should be((10 + 1 + 2) + (1 + 2))
    }
    "add next two rolls (strike and not strike) to the score" in {
      val game = new BowlingGameFun()
        .roll(10)
        .roll(10)
        .roll(1)
        .roll(2)
        .roll(3)
        .roll(4)
      game.score should be((10 + 10 + 1) + (10 + 1 + 2) + (1 + 2) + (3 + 4))
    }
    "add next two rolls (both strikes) to the score" in {
      val game = new BowlingGameFun()
        .roll(10)
        .roll(10)
        .roll(10)
        .roll(2)
        .roll(3)
      game.score should be((10 + 10 + 10) + (10 + 10 + 2) + (10 + 2 + 3) + (2 + 3))
    }
    "not add additional rolls if they were not played" in {
      val game = new BowlingGameFun()
        .roll(1)
        .roll(1)
        .roll(10)
      game.score should be((1 + 1) + 10)
    }
  }

  "Spare" should {
    "add next roll to the score" in {
      val game = new BowlingGameFun()
        .roll(9)
        .roll(1)
        .roll(7)
        .roll(3)
        .roll(1)
        .roll(2)
      game.score should be((9 + 1 + 7) + (7 + 3 + 1) + (1 + 2))
    }
    "not add additional roll if it was not played" in {
      val game = new BowlingGameFun()
        .roll(1)
        .roll(1)
        .roll(8)
        .roll(2)
      game.score should be((1 + 1) + (8 + 2))
    }
  }

  "Tenth frame" should {
    "give 30 points for three strikes" in {
      val game = (1 to 18).foldLeft(new BowlingGameFun())((bg, i) => bg.roll(0))
        .roll(10)
        .roll(10)
        .roll(10)
      game.score should be(30)
    }


    "add one roll for spare" in {
      val game = (1 to 18).foldLeft(new BowlingGameFun())((bg, i) => bg.roll(0))
        .roll(9)
        .roll(1)
        .roll(10)
      game.score should be(20)
    }
  }

  "Perfect game" should {
    "score 300 for 12 strikes (10 regular and 2 bonus)" in {
      val game = (1 to 12).foldLeft(new BowlingGameFun())((bg, i) => bg.roll(10))
      game.score should be(300)
    }
  }

  "Unfinished perfect game" should {
    "score 290 for 11 strikes (10 regular and 1 bonus, 1 bonus to go)" in {
      val game = (1 to 11).foldLeft(new BowlingGameFun())((bg, i) => bg.roll(10))
      game.score should be(290)
    }
  }

  "A roll in finished game" should {
    "not be aloowed" in {
      val game = (1 to 12).foldLeft(new BowlingGameFun())((bg, i) => bg.roll(10))
        .roll(1)
      intercept[IllegalArgumentException] {
        game.score
      }
    }
  }
}

