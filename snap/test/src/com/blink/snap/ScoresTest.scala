package com.blink.snap

import com.blink.snap.Player._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ScoresTest extends AnyFlatSpec with Matchers {

  "A score" should "know who is the winner when the scores are not equal" in {
    Scores(0,1).winner must be(Player2)
    Scores(1,0).winner must be(Player1)
  }

  "A score" should "mark as winner Player1 in case of tie" in {
    Scores(0,0).winner must be(Player1)
    Scores(1,1).winner must be(Player1)
  }

}
