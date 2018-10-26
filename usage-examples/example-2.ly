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
%       Copyright Jan-Peter Voigt, Urs Liska, 2016                            %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.37"

\include "oll-core/package.ily"
\loadPackage \with {
  % THE NAME OF THIS OPTION AND THE FILENAME WILL CHANGE IN UPCOMING VERSIONS!
  % don't write 'example-2.edition.log'
  write-log = ##f
} edition-engraver

\paper {
  ragged-right = ##f
}

% The edition-engraver is now able to produce extenders and hyphens.
% This might not be necessary very often with autoextenders, but still there are use-cases.


\consistToContexts #edition-engraver Score.Staff.Voice.Lyrics

\addEdition test
\editionMod test 1 0/4 Voice \(
\editionMod test 1 1/4 Lyrics #(make-music 'ExtenderEvent)
\editionMod test 1 2/8 Voice [
\editionMod test 1 3/8 Voice ]
\editionMod test 1 6/8 Voice (
\editionMod test 1 7/8 Voice )
\editionMod test 1 7/8 Voice \)
\editionMod test 2 0/4 Score \time 3/4
\editionMod test 2 0/4 Score \tempo "Allegro" 4=135
\editionMod test 2 0/4 Voice e''4 % articulations crash right now!
\editionMod test 2 0/4 Voice ^\p
\editionMod test 2 0/4 Voice ^\<
\editionMod test 2 1/4 Lyrics #(make-music 'HyphenEvent)
% due to the melisma mechanics, we have to set the properties one step later
\editionMod test 2 3/8 Voice \melisma
\editionMod test 2 4/8 Voice \melismaEnd
\editionMod test 3 0/8 Voice \!

\relative {
  \autoBeamOff
  c''4 a8 bes8 c4 d8 cis | c4 a8 b c4 c
} \addlyrics { Say what you think, say what you need. }

