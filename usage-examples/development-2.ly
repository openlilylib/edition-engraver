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

\paper {
  ragged-right = ##f
}
\layout {
  \context {
    \Score
    \override BarNumber.break-visibility = #'#(#t #t #t)
    barNumberVisibility = #all-bar-numbers-visible
  }
}

\consistToContexts #edition-engraver Score.Staff.Voice.Lyrics

% TODO add mod-marker function, do automatically track and color all modded elements?
% display primary time-address
editionMod =
#(let ((short-mom? (@@ (edition-engraver engine) short-mom?)))
   (define-void-function
    (edition-target measure moment context-edition-id mods)
    (symbol? integer? short-mom? list? music-or-context-mod?)
    ((@ (edition-engraver engine) editionMod) edition-target measure moment context-edition-id
      #{ ^\markup \box \fontsize #-4 $(format "~A ~A" measure moment) #})
    ((@ (edition-engraver engine) editionMod) edition-target measure moment context-edition-id mods)
    ))

\addEdition test
\editionMod test 1 #(ly:make-moment -1/4) Staff \once \override NoteHead.color = #red
\editionMod test 1 4/4 Staff \once \override NoteHead.color = #red
\editionMod test 1 9/4 Staff { \once \override NoteHead.color = #red <>^X }
\editionMod test 2 0/4 Staff \once \override Stem.color = #green

\editionMod test 3 8/4 Staff \once \override NoteHead.color = #green
\editionMod test 5 4/4 Staff \once \override NoteHead.color = #green

\editionMod test 6 3/4 Staff \bar "|."

\relative {
  \partial 4 c'' | b bes b c | d e fis c | b bes b c |
  \time 3/4 b4 bes a | as g ges | f a a |
}
