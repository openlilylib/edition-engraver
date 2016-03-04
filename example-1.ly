\version "2.19.37"
\include "edition-engraver.ily"

\new Staff = "BACH" \with {
  edition-id = #'(hallo welt)
  \consists #edition-engraver
} \relative c'' { bes4 a c b }
