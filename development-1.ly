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

\version "2.19.43"
\include "edition-engraver.ily"
\include "../oll-core/util/consist-to-contexts.ily"

% In this example we introduce a method to easily enter lists of mods
% (the function provided here shall be moved to be available at once,
% but right now they are copy-and-paste-fellows)

% start a new list of edition-mods:
% lid: the name of the variable to use
% edition-target: the target the mods are tagged with, like 'fullScore'
% edition-context-id: the context to apply the mods to
% proc: a procedure which takes one parameter and returns the actual mod
% - e.g. a music expression with overrides, a contextMod or anything else applicable to \editionMod
startModList =
#(define-void-function (lid edition-target edition-context-id proc)(symbol? symbol? list? procedure?)
   (newAtree lid) ; clear the variable
   (ly:parser-define! 'current-modlist lid) ; store list/var name
   (ly:parser-define! 'current-modproc `((proc . ,proc) ; store alist with procedure,
                                          (edition-target . ,edition-target) ; edition-target and
                                          (edition-context-id . ,edition-context-id))) ; edition-context-id
   )

% add one *mod* to the formerly initiated list at measure *takt*, postion *pos*
addModList =
#(let ((short-mom? (@@ (edition-engraver engine) short-mom?))) ; TODO short-mom? should be public ... naming??
   (define-void-function (takt pos mod)
     (integer? short-mom? scheme?)
     (if (and (defined? 'current-modlist)(symbol? current-modlist)) ; if ModList is started
         (addAtree current-modlist (list (cons takt pos)) mod) ; add mod to the list
         (ly:input-warning (*location*) "no modlist started?") ; else give warning
         )))

% actually enter all mods
finishModList =
#(define-void-function ()()
   (if (and (defined? 'current-modlist) ; check, if we have everything we need
            (symbol? current-modlist)
            (defined? 'current-modproc)
            (list? current-modproc)
            (symbol? (assoc-get 'edition-target current-modproc))
            (list? (assoc-get 'edition-context-id current-modproc))
            (procedure? (assoc-get 'proc current-modproc))
            )
       (let ((lst (ly:parser-lookup current-modlist))
             (edition-target (assoc-get 'edition-target current-modproc))
             (edition-id (assoc-get 'edition-context-id current-modproc))
             (proc (assoc-get 'proc current-modproc)))
         (for-each
          (lambda (p)
            (if (pair? p)
                (let ((mod (proc (cdr p))))
                   ; \editionMod edition-target measure position edition-context-id mod
                  (editionMod edition-target (caar p) (cdar p) edition-id mod)
                  )))
          lst)
         ; TODO should list be cleared?
         )
       (ly:input-warning (*location*) "no modlist started?")
       ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% example

% create mod-function
modproc = \once \override NoteHead.color = \etc
% for more advanced mods you'll use #(lambda (val) #{ ... #})

% initialize ModList with:
% lid: modlist
% edition-target: partitur
% edition-context-id: Score
% proc: ... \once \override NoteHead.color = #val
\startModList modlist partitur Score #modproc
% add mods in measure 1 with values red and blue
\addModList 1 3/8 #red
\addModList 1 5/8 #blue
\addModList 2 5/8 #yellow % order of input doesn't matter, but is recommended
\addModList 2 0/8 #green

% display modlist:
#(display modlist)

% actually add mods
\finishModList

% consist edition-engraver to Score contexts
\consistToContexts #edition-engraver Score
% activate edition-target 'partitur'
\addEdition partitur

% new Score 
\repeat unfold 4 \relative c'' { bes8 a c b }


