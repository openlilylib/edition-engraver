\version "2.19.37"

\include "oll-core/package.ily"
\loadPackage edition-engraver

\setEditions changes
\consistToContexts #edition-engraver Score.PianoStaff.Staff.Voice

base = music.piano

\editionMod changes 1 3/4 \base.left.Voice.A \change Staff = right

meta = { s1*4 }
right = {}
left = \relative { c4 f bes es as d }

\new PianoStaff \with {
  \editionID \base
} <<
  \new Staff = right \with {
    \editionID right
  } \new Voice << \clef G \meta \right >>
  \new Staff = left \with {
    \editionID left
  } \new Voice << \clef bass \meta \left >>
>>
