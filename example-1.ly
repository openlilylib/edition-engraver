\version "2.19.37"
\include "edition-engraver.ily"

#(add-edition 'test)
#(edition-mod 'test 1 (ly:make-moment 2/4) '(hallo welt Voice) (list #{ \override NoteHead.color = #red #}))

\new Staff = "BACH" \with {
  edition-id = #'(hallo welt)
  \consists #edition-engraver
} \new Voice = "SING" \with {
  \consists #edition-engraver
} \relative c'' { bes4 a c b }
