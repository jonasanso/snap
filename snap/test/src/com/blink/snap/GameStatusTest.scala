package com.blink.snap

import com.blink.snap.Card._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class GameStatusTest extends AnyFlatSpec with Matchers with TestImplicits {

  "A game" should "deal a card and add it to the dealt deck of cards" in {

    GameStatus(Table(Deck(List(D_A, D_2)), DealtDeck.empty), Scores.initial).play must be(
      GameStatus(Table(Deck(List(D_2)), DealtDeck(List(D_A))), Scores.initial)
    )

    GameStatus(Table(Deck(List(D_2)), DealtDeck(List(D_A))), Scores.initial).play must be(
      GameStatus(Table(Deck.empty, DealtDeck.empty), Scores(2, 0))
    )
  }

  "A game" should "finish when there are no more cards to deal" in {
    GameStatus(Table(Deck(List(D_A, D_2)), DealtDeck.empty), Scores.initial).isNotFinished must be(true)

    GameStatus(Table(Deck.empty, DealtDeck.empty), Scores.initial).isNotFinished must be(false)
  }

}

trait TestImplicits {
  implicit val matcher: SnapMatcher = SnapMatcher.Suits
  implicit val winner: () =>  Player = () => Player.Player1
}
