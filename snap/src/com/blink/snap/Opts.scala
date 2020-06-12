package com.blink.snap

abstract class SnapMatcher extends Product with Serializable

object SnapMatcher {
  case object Values extends SnapMatcher
  case object Suits extends SnapMatcher
  case object Both extends SnapMatcher
}

final case class Opts(numDecks: Int, matcher: SnapMatcher)






