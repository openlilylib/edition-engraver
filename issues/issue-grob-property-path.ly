\version "2.19.82"

\include "oll-core/package.ily"
\loadPackage edition-engraver

\consistToContexts #edition-engraver Score.Staff.Voice

base = the.composition

\layout {
  \context {
    \Score
    \editionID \base
  }
}

\addEdition test

\editionMod test 2 0/4 \base.Voice {
  \override TextSpanner.bound-details.left.text = "A"
  <>\startTextSpan
}
\editionMod test 2 3/4 \base.Voice \stopTextSpan
\editionMod test 3 0/4 \base.Voice \once \override TextSpanner.color = #green

\new Voice \relative {
  c''4 c c c | d d d d | \once \override TextSpanner.color = #blue \override TextSpanner.bound-details.left.text = "B" b\startTextSpan b b b\stopTextSpan |
}
