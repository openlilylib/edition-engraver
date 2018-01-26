%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib)                                            %
%              -----------                                                    %
%                                                                             %
% Library: edition-engraver                                                   %
%          ================                                                   %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% edition-engraver is maintained by Jan-Peter Voigt, jp.voigt@gmx.de          %
% and others.                                                                 %
%       Copyright Jan-Peter Voigt, Urs Liska, 2016-2018                       %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.37"

\include "oll-core/package.ily"
\loadPackage edition-engraver

\addEdition test

\editionMod test 2 0/4 sing.with.bach.along.Voice.B \once \override NoteHead.color = #red

\editionMod test 2 2/4 sing.with.bach.along.Voice.B \override NoteHead.color = #green
\editionMod test 2 3/4 sing.with.bach.along.Voice.B \override NoteHead.color = #blue
\editionMod test 3 1/4 sing.with.bach.along.Voice.B \revert NoteHead.color
% the following refers to the Voice named "1" which is the upper voice inside << {} \\ {} >>
\editionMod test 5 0/4 sing.with.bach.along.Voice."1" \revert NoteHead.color

% you can reduce redundancy with variables:
BACH = sing.with.bach
STAFF = along
ba = sing.with.bach.\STAFF % (TODO we need a helper for \BACH.\STAFF)
\editionMod test 13 3/8 \ba.Voice."2" \once \override NoteHead.color = #red

% insert a list of modifications
\editionModList test \BACH.Score \break 5,9,13,17

\editionMod test 2 2/4 \ba.Staff \clef "alto"
\editionMod test 3 2/4 \ba.Staff \clef "G"
\editionMod test 5 0/4 \ba.Staff \bar ".|:-||"
\editionMod test 5 0/4 \ba.Staff {
  \once \override KeySignature.color = #red
  \key f \major
}
\editionMod test 10 0/4 \ba.Staff \key g \major

\editionMod test 5 0/4 \BACH.Score \set proportionalNotationDuration = #(ly:make-moment 1/24)
\editionMod test 7 0/4 \BACH.Score \unset proportionalNotationDuration

\editionMod test 5 1/4 \ba.Staff ^\tweak self-alignment-X #0 -"Hallo"
\editionMod test 9 0/4 \ba.Staff \mark \default
\editionMod test 10 0/4 \ba.Staff \mark \default


\editionMod test 10 0/4 \ba.Voice.B {
  \once \override NoteHead.extra-offset = #'(2 . -1)
  \once \override NoteHead.color = #green
}

% "Install" the edition-engraver in a number of contexts.
% The order is not relevant,
% Dynamics is not used in this example, Foo triggers an oll:warn
\consistToContexts #edition-engraver Score.Staff.Voice

\layout {
  \context {
    \Score
    \editionID ##f \BACH
    %edition-engraver-log = ##t
  }
  \context {
    \Voice
    %edition-engraver-log = ##t
  }
}

\new Staff = "BACH" \with {
  \editionID \STAFF
} {
  R1
  <<
    \repeat unfold 10 \relative c'' { bes4 ( a c b ) } \\
    \repeat unfold 10 \relative c' { d4. e4 f8 g4 }
  >>
  <<
    \repeat unfold 10 \relative c'' { bes4 a c b } \\
    \repeat unfold 10 \relative c' { d4. e4 f8 g4 } \\
    \repeat unfold 10 \relative c' { f2 a }
  >>
}
