package com.blink.snap

import SnapMatcher._

object Main {

  def main(args: Array[String]): Unit = {
    // TODO: Ask user for Opts
    val opts = Opts(numDecks = 1, matcher = Suits)
    println(GameEngine.play(opts))
  }
}
