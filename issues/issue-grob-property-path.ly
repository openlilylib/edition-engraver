\version "2.19.82"

\include "oll-core/package.ily"
\loadPackage edition-engraver

\consistToContexts #edition-engraver Score.Staff.Voice

\addEdition test

\editionMod test 2 0/4 Voice {
  \override TextSpanner.bound-details.left.text = "A"
  <>\startTextSpan
}
\editionMod test 2 3/4 Voice \stopTextSpan

\new Voice \relative {
  c''4 c c c | d d d d | \override TextSpanner.bound-details.left.text = "B" b\startTextSpan b b b\stopTextSpan |
}

