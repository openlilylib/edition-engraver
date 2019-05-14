\version "2.19.82"

\include "oll-core/package.ily"
\loadPackage edition-engraver

\consistToContexts #edition-engraver Score.Staff.Voice

\addEdition test

\editionMod test 2 0/4 Staff { \override NoteHead.color = #red \ottava #1 }

\new Voice \relative {
  \time 3/4
  c''4 e g | c e g |
}

