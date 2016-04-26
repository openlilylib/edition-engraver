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

\version "2.19.36"
\include "oll-core.ily"

% activate edition-engraver module
#(use-modules (edition-engraver engine))
\include "util/consist-to-contexts.ily"

% Install the edition-engraver in the contexts
% specified by the argument list

% This function is deprecated,
% Please remove ASAP!
consistEE =
#(define-scheme-function (contexts)(symbol-list?)
   (oll:warn "\\consistEE is deprecated. Please use \\consistToContexts #edition-engraver <context-list>")
   #{
     \layout {
       #(map
         (lambda (ctx)
           (if (and (defined? ctx)
                    (ly:context-def? (module-ref (current-module) ctx)))
               #{
                 \context {
                   #(module-ref (current-module) ctx)
                   \consists \edition-engraver
                 }
               #}
               ; TODO: Make the input location point to the location of the *caller*
               (oll:warn (format "Trying to install edition-engraver to non-existent context ~a" ctx))))
         contexts)
     }
   #})
