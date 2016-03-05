\version "2.19.37"
\include "edition-engraver.ily"

\addEdition test
\editionMod test 1 2/4 hallo.welt.Voice \override NoteHead.color = #green
\editionMod test 1 2/4 hallo.welt.Staff \clef "alto"

\layout {
  \context {
    \Score
    \consists \edition-engraver
  }
  \context {
    \Staff
    \consists \edition-engraver
  }
  \context {
    \Voice
    \consists \edition-engraver
  }
}

\new Staff = "BACH" \with {
  edition-id = #'(hallo welt)
} \new Voice = "SING" \relative c'' { bes4 a c b }
