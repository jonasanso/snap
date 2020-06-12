package com.blink.snap

import Suit._
import Value._
import Card._
import SnapMatcher._

import scala.util.Random

abstract class Suit extends Product with Serializable
object Suit {
  case object Hearts extends Suit
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Spades extends Suit
}

abstract class Value extends Product with Serializable
object Value {
  case object _2 extends Value
  case object _3 extends Value
  case object _4 extends Value
  case object _5 extends Value
  case object _6 extends Value
  case object _7 extends Value
  case object _8 extends Value
  case object _9 extends Value
  case object _10 extends Value
  case object J extends Value
  case object Q extends Value
  case object K extends Value
  case object A extends Value
}

abstract class Card(val suit: Suit, val value: Value) extends Product with Serializable {
  def snaps(other: Card)(implicit matcher: SnapMatcher): Boolean = matcher match {
    case Values => this.value == other.value
    case Suits => this.suit == other.suit
    case Both => this.suit == other.suit || this.value == other.value
  }
}

object Card {
  case object H_A extends  Card(Hearts, A)
  case object H_2 extends  Card(Hearts, _2)
  case object H_3 extends  Card(Hearts, _3)
  case object H_4 extends  Card(Hearts, _4)
  case object H_5 extends  Card(Hearts, _5)
  case object H_6 extends  Card(Hearts, _6)
  case object H_7 extends  Card(Hearts, _7)
  case object H_8 extends  Card(Hearts, _8)
  case object H_9 extends  Card(Hearts, _9)
  case object H_10 extends Card(Hearts, _10)
  case object H_J extends  Card(Hearts, J)
  case object H_Q extends  Card(Hearts, Q)
  case object H_K extends  Card(Hearts, K)

  case object C_A extends  Card(Clubs, A)
  case object C_2 extends  Card(Clubs, _2)
  case object C_3 extends  Card(Clubs, _3)
  case object C_4 extends  Card(Clubs, _4)
  case object C_5 extends  Card(Clubs, _5)
  case object C_6 extends  Card(Clubs, _6)
  case object C_7 extends  Card(Clubs, _7)
  case object C_8 extends  Card(Clubs, _8)
  case object C_9 extends  Card(Clubs, _9)
  case object C_10 extends Card(Clubs, _10)
  case object C_J extends  Card(Clubs, J)
  case object C_Q extends  Card(Clubs, Q)
  case object C_K extends  Card(Clubs, K)

  case object D_A extends  Card(Diamonds, A)
  case object D_2 extends  Card(Diamonds, _2)
  case object D_3 extends  Card(Diamonds, _3)
  case object D_4 extends  Card(Diamonds, _4)
  case object D_5 extends  Card(Diamonds, _5)
  case object D_6 extends  Card(Diamonds, _6)
  case object D_7 extends  Card(Diamonds, _7)
  case object D_8 extends  Card(Diamonds, _8)
  case object D_9 extends  Card(Diamonds, _9)
  case object D_10 extends Card(Diamonds, _10)
  case object D_J extends  Card(Diamonds, J)
  case object D_Q extends  Card(Diamonds, Q)
  case object D_K extends  Card(Diamonds, K)

  case object S_A extends  Card(Spades, A)
  case object S_2 extends  Card(Spades, _2)
  case object S_3 extends  Card(Spades, _3)
  case object S_4 extends  Card(Spades, _4)
  case object S_5 extends  Card(Spades, _5)
  case object S_6 extends  Card(Spades, _6)
  case object S_7 extends  Card(Spades, _7)
  case object S_8 extends  Card(Spades, _8)
  case object S_9 extends  Card(Spades, _9)
  case object S_10 extends Card(Spades, _10)
  case object S_J extends  Card(Spades, J)
  case object S_Q extends  Card(Spades, Q)
  case object S_K extends  Card(Spades, K)


  val all =
    List(H_A, H_2, H_3, H_4, H_5, H_6, H_7, H_8, H_9, H_10, H_J, H_Q, H_K) ++
      List(C_A, C_2, C_3, C_4, C_5, C_6, C_7, C_8, C_9, C_10, C_J, C_Q, C_K) ++
      List(D_A, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9, D_10, D_J, D_Q, D_K) ++
      List(S_A, S_2, S_3, S_4, S_5, S_6, S_7, S_8, S_9, S_10, S_J, S_Q, S_K)
}

final case class Deck(cards: List[Card]){
  def isEmpty: Boolean = cards.isEmpty
  def take: Option[(Card, Deck)] = cards match {
    case c :: n => Some((c, Deck(n)))
    case _ => None
  }
}

object Deck {

  private val deck =
    List(H_A, H_2, H_3, H_4, H_5, H_6, H_7, H_8, H_9, H_10, H_J, H_Q, H_K) ++
      List(C_A, C_2, C_3, C_4, C_5, C_6, C_7, C_8, C_9, C_10, C_J, C_Q, C_K) ++
      List(D_A, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9, D_10, D_J, D_Q, D_K) ++
      List(S_A, S_2, S_3, S_4, S_5, S_6, S_7, S_8, S_9, S_10, S_J, S_Q, S_K)

  def cards(numDecks: Int): Deck = Deck(Random.shuffle((1 to numDecks).flatMap(_ => deck)).toList)

  def empty: Deck = Deck(List.empty)
}

final case class DealtDeck(cards: List[Card]) {
  def deal(card: Card): DealtDeck = new DealtDeck(card :: cards)

  def isSnapValid(implicit matcher: SnapMatcher): Boolean = cards match {
    case a1 :: a2 :: _ => a1.snaps(a2)
    case _ => false
  }

  def countCards: Int = cards.length
}

object DealtDeck {
  def empty: DealtDeck = DealtDeck(List.empty)
}


final case class Table(deck: Deck, dealtDeck: DealtDeck) {

  def canDeal: Boolean = !deck.isEmpty
  def isSnapValid(implicit matcher: SnapMatcher): Boolean = dealtDeck.isSnapValid
  def countDealtCards: Int = dealtDeck.countCards

  def deal: Table = deck.take match {
    case Some((c, n)) => Table(n, dealtDeck.deal(c))
    case _ => this
  }

  def collectCards: Table = Table(deck, DealtDeck.empty)
}

