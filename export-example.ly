%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib)                                            %
%              -----------                                                    %
%                                                                             %
% Library: lilypond-export                                                    %
%          ===============                                                    %
%                                                                             %
% export foreign file formats with LilyPond                                   %
%                                                                             %
% lilypond-export is free software: you can redistribute it and/or modify     %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% lilypond-export is distributed in the hope that it will be useful,          %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% lilypond-export is maintained by Jan-Peter Voigt, jp.voigt@gmx.de           %
%                                                                             %
%       Copyright Jan-Peter Voigt, Urs Liska, 2017                            %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.58"
\include "export-base.ly"

music = <<
  \new Staff {
    \time 3/4
    \relative <<
      { c''4. a8 g4 | g( bes) <g b> | \tuplet 3/2 { a c a~ } a | } \\
      { e8 f g fis e4 | es2 d4 | <c e>2 <c f>4 }
    >>
  }
  \new Staff {
    \time 3/4 \clef bass
    \new Voice = "mel" \relative { c'2 c4 | c g b | a2. | }
  }
  \new Lyrics \lyricsto "mel" { \lyricmode { la le li lu la lo } }
>>

% TODO wrap run-translator in function
% exporter can run without actually typesetting
\runTranslator \music
\FileExport #`((exporter . ,exportHumdrum)) % TODO more than one output format in one run?

opts.exporter = #exportMusicXML

% or as a layout extension that is added to the layout
\score {
  \music
  \layout {
    \FileExport #opts
  }
  \midi {}
}
