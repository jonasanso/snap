package com.blink.snap

import scala.util.Random

abstract class Player(name: String) extends Product with Serializable {
  override def toString: String = name
}

object Player {
  case object Player1 extends Player("Player 1")
  case object Player2 extends Player("Player 2")

  val all = List(Player1, Player2)

  val initial: Player = Player1

  def pickFasterSnap(): Player = Random.shuffle(all).head
}




