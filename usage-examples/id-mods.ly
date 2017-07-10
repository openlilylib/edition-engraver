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
\include "edition-engraver/edition-engraver.ily"

\activateIdMods Tie.NoteHead.Beam

% Apply a single property override to a single grob
\idMod #'b color #red

% Apply multiple property overrides to a single grob
\idMods A
#`((color . ,blue)
   (extra-offset . (-1.5 . -0.5)))

\relative c' {
  <c -\tweak Tie.eid #'a ~
  \tweak eid #'A
  e
  -\tweak Tie.eid #'b ~
  g -\tweak Tie.eid #'c ~
  c -\tweak Tie.eid #'d ~
  >1
  <c e g c>
}

% Apply the same property override to multiple grobs
\idModList positions #'(4 . 4) #'(Ba Bb Bc Bd)
\relative {
  \stemUp
  \once \override Beam.eid = #'Ba
  c'8 d e f
  \once \override Beam.eid = #'Bb
  g e d b'
  \once \override Beam.eid = #'Bc
  c e a, c
  \once \override Beam.eid = #'Bd
  fis, a dis, fis
}

% Apply different values to the same property of multiple grobs
% Not specifying the value for a grob uses the default
\idVarModList color #red
#`((Be . ,blue)
   (Bf . ,magenta)
   Bg
   (Bh . ,green))

\relative {
  \stemUp
  \once \override Beam.eid = #'Be
  c'8 d e f
  \once \override Beam.eid = #'Bf
  g e d b'
  \once \override Beam.eid = #'Bg
  c e a, c
  \once \override Beam.eid = #'Bh
  fis, a dis, fis
}