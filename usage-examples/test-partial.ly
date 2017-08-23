\version "2.21.0"
\include "oll-core/package.ily"
\loadPackage edition-engraver

\consistToContexts #edition-engraver Score.Staff.Voice
\layout {
  \context {
    \Voice
    edition-engraver-log = ##t
  }
}

\addEdition test
\editionMod test 1 #(ly:make-moment -1/4) lala.la.Voice.A \once \override NoteHead.color = #green
\editionMod test 1 0/4 lala.la.Voice.C \once \override NoteHead.color = #red

\new Staff \with {
  edition-id = lala.la
} \relative {
  \time 3/4 \partial 4
  c'4 \voices 2,1 << e \\ { a8[ as] } >> g4 fis
}
