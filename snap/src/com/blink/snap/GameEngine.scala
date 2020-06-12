package com.blink.snap

object GameEngine {

  def play(opts: Opts): Scores = {
    var g = GameStatus.initial(opts)
    while (g.isNotFinished) g = g.play
    g.scores
  }
}

final case class GameStatus(table: Table, scores: Scores)(implicit matcher: SnapMatcher, snapWinner: () => Player) {
  def isNotFinished: Boolean = table.canDeal

  def play: GameStatus= {
    val next = table.deal
    if (next.isSnapValid) {
      val updatedScores = scores.update(snapWinner(), next.countDealtCards)
      GameStatus(next.collectCards, updatedScores)
    } else {
      GameStatus(next, scores)
    }
  }
}

object GameStatus {
  def initial(opts: Opts): GameStatus =
    GameStatus(Table(Deck.cards(opts.numDecks), DealtDeck.empty), Scores.initial)(opts.matcher, Player.pickFasterSnap)
}