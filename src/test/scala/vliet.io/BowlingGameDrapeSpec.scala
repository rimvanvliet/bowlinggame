package vliet.io

/**
  * Created by ruud on 11/12/2016.
  */

import org.scalatest.{Matchers, WordSpec}
import vliet.io._

class BowlingGameDrapeSpec extends WordSpec with Matchers {

  "Number of pins in a roll" should {
    "be less or equal to 10" in {
      intercept[IllegalArgumentException] {
        BowlingGameDrape.score(List(11))
      }
    }
    "be more or equal to 0" in {
      intercept[IllegalArgumentException] {
        BowlingGameDrape.score(List(-1))
      }
    }
  }
  "Number of pins in a frame" should {
    "be less or equal to 10" in {
      intercept[IllegalArgumentException] {
        BowlingGameDrape.score(List(8, 3))
      }
    }
  }

  "Score" should {
    "be sum of all roles" in {
      BowlingGameDrape.score(List(1, 2, 3, 4, 5, 4)) should be(1 + 2 + 3 + 4 + 5 + 4)
    }
  }

  "Strike" should {
    "add next two rolls (not strikes) to the score" in {
      BowlingGameDrape.score(List(10, 1, 2)) should be((10 + 1 + 2) + (1 + 2))
    }
    "add next two rolls (strike and not strike) to the score" in {
      BowlingGameDrape.score(List(10, 10, 1, 2, 3, 4)) should be((10 + 10 + 1) + (10 + 1 + 2) + (1 + 2) + (3 + 4))
    }
    "add next two rolls (both strikes) to the score" in {
      BowlingGameDrape.score(List(10, 10, 10, 2, 3)) should be((10 + 10 + 10) + (10 + 10 + 2) + (10 + 2 + 3) + (2 + 3))
    }
    "not add additional rolls if they were not played" in {
      BowlingGameDrape.score(List(1, 1, 10)) should be((1 + 1) + 10)
    }
  }

  "Spare" should {
    "add next roll to the score" in {
      BowlingGameDrape.score(List(9, 1, 7, 3, 1, 2)) should be((9 + 1 + 7) + (7 + 3 + 1) + (1 + 2))
    }
    "not add additional roll if it was not played" in {
      BowlingGameDrape.score(List(1, 1, 8, 2)) should be((1 + 1) + (8 + 2))
    }
  }

  "Tenth frame" should {
    "for strike, give 2 bonus rolls" in {
      BowlingGameDrape.score(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10)) should be(30)
    }
    "for spare, add one bonus roll" in {
      BowlingGameDrape.score(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 1, 10)) should be(20)
    }
  }

  "Perfect game" should {
    "score 300 for 12 strikes (10 regular and 2 bonus)" in {
      BowlingGameDrape.score(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)) should be(300)
    }
  }

  "11 strikes" should {
    "score 290 for 11 strikes (10 regular and 1 bonus,1 bonus to go)" in {
      BowlingGameDrape.score(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)) should be(290)
    }
  }

  "10 strikes" should {
    "score 270 for 10 strikes (10 regular and 2 bonus to go)" in {
      BowlingGameDrape.score(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)) should be(270)
    }
  }

  "A roll in finished game" should {
    "not be allowed in a perfect game" in {
      intercept[IllegalArgumentException] {
        BowlingGameDrape.score(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 1))
      }
    }

    "not be allowed in a spare last frame" in {
      intercept[IllegalArgumentException] {
        BowlingGameDrape.score(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 9, 1, 10, 1))
      }
    }
    "not be allowed in a standard last frame" in {
      intercept[IllegalArgumentException] {
        BowlingGameDrape.score(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 1, 1))
      }
    }
  }
}

