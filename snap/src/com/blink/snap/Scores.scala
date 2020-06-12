package com.blink.snap

import com.blink.snap.Player.{Player1, Player2}

final case class Scores(p1Score: Int, p2Score: Int) {
  def update(player: Player, score: Int): Scores = player match {
    case Player1 => new Scores(p1Score + score, p2Score)
    case Player2 => new Scores(p1Score, p2Score  + score)
  }

  def winner: Player = if (p1Score > p2Score) Player1 else Player2

  override def toString: String = {
    s"""|The winner is $winner. Scores =>
        |$Player1: $p1Score points
        |$Player2: $p2Score points
        |""".stripMargin
  }
}

object Scores {
  val initial: Scores = Scores(0, 0)
}