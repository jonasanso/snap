package com.blink.snap

import com.blink.snap.Card._
import com.blink.snap.SnapMatcher._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class TableTest extends AnyFlatSpec with Matchers {

  "A table" should "deal a card and add it to the dealt deck of cards" in {
    Table(Deck(List(D_A, D_2)), DealtDeck.empty).deal must be(Table(Deck(List(D_2)), DealtDeck(List(D_A))))
    Table(Deck(List(D_2)), DealtDeck(List(D_A))).deal must be(Table(Deck.empty, DealtDeck(List(D_2, D_A))))
  }

  "A table" should "collect the dealt cards" in {
    Table(Deck(List(D_2)), DealtDeck(List(D_A))).collectCards must be(Table(Deck(List(D_2)), DealtDeck.empty))
  }

  "A table" should "validate snap comparing the 2 most recent values using the snap matcher" in {
    Table(Deck.empty, DealtDeck(List(D_2, D_A))).isSnapValid(Suits) must be(true)
    Table(Deck.empty, DealtDeck(List(S_A, D_A))).isSnapValid(Suits) must be(false)

    Table(Deck.empty, DealtDeck(List(D_2, D_A))).isSnapValid(Values) must be(false)
    Table(Deck.empty, DealtDeck(List(S_A, D_A))).isSnapValid(Values) must be(true)

    Table(Deck.empty, DealtDeck(List(D_2, D_A))).isSnapValid(Both) must be(true)
    Table(Deck.empty, DealtDeck(List(S_A, D_A))).isSnapValid(Both) must be(true)
    Table(Deck.empty, DealtDeck(List(S_2, D_A))).isSnapValid(Both) must be(false)
  }
}
