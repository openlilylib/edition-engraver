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
%       Copyright Jan-Peter Voigt, Urs Liska, 2017                            %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.37"

\include "oll-core/package.ily"
\loadPackage edition-engraver

\consistToContexts #edition-engraver Score.Staff.Voice.Lyrics

#(define (my-wild-card v) (eq? #\l (string-ref (format "~A" v) 0)))

\addEdition test

% path-elements surrounded by '<' and '>' denote a procedure
\editionMod test 1 5/4 "<my-wild-card>".Voice \once \override NoteHead.color = #green
% path elements enclosed in '/' are regular expressions
\editionMod test 2 2/4 "/^.[eu].*$/".Voice \once \override NoteHead.color = #red
\editionMod test 2 3/4 "/^[fl][au]$/".Voice \once \override NoteHead.color = #'(0.8 0.5 0.8)
% path elements enclosed in curly brackets are wildcards
\editionMod test 3 2/4 "{l*}".Voice \once \override NoteHead.color = #blue
\editionMod test 3 3/4 "{*a}".Voice \once \override NoteHead.color = #'(0.5 0.5 1)
\editionMod test 3 3/4 "{*u}".Voice \once \override NoteHead.color = #'(1 0.5 0.5)

<<
  \new Staff \with {
    \editionID la
  } \repeat unfold 3 \relative { c''4 b bes a }
  \new Staff \with {
    \editionID le
  } \repeat unfold 3 \relative { c''4 b bes a }
  \new Staff \with {
    \editionID fu
  } \repeat unfold 3 \relative { c''4 b bes a }
>>

