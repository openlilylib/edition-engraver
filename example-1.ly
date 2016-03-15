\version "2.19.37"
\include "edition-engraver.ily"

\addEdition test
\editionMod test 1 2/4 sing.with.bach.along.Voice.A \override NoteHead.color = #green
\editionMod test 1 3/4 sing.with.bach.along.Voice.A \override NoteHead.color = #blue
\editionMod test 2 1/4 sing.with.bach.along.Voice.A \revert NoteHead.color
\editionMod test 4 0/4 sing.with.bach.along.Voice.A \revert NoteHead.color
\editionModList test sing.with.bach.Score \break #'(4 8 12 16)

\editionMod test 1 2/4 sing.with.bach.along.Staff \clef "alto"
\editionMod test 2 2/4 sing.with.bach.along.Staff \clef "G"
\editionMod test 4 0/4 sing.with.bach.along.Staff \bar ".|:-||"

% "Install" the edition-engraver in a number of contexts.
% The order is not relevant,
% Dynamics is not used in this example, Foo triggers an oll:warn
\consistEE Score.Staff.Voice

\layout {
  \context {
    \Score
    \editionID ##f sing.with.bach
    %edition-engraver-log = ##t
  }
}

\new Staff = "BACH" \with {
  \editionID along
} <<
  \new Voice \with { \voiceOne } { \repeat unfold 20 \relative c'' { bes4 a c b } }
  \new Voice = "accom" \with { \voiceTwo } \repeat unfold 20 \relative c' { d4. e4 f8 g4 }
>>
