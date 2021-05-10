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
%       Copyright Jan-Peter Voigt, Urs Liska, 2017, 2018                      %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.82"
\include "oll-core/package.ily"
\loadPackage lilypond-export

music = \new PianoStaff <<
  \new Staff <<
    { \time 3/4 \key es \major \set Timing.tempoWholesPerMinute = #(ly:make-moment 30) \partial 4 s4 | \repeat volta 2 { s2.*3 } }
    \relative <<
      { b'4 | c4. a8 g4 | g( bes) <g b> | \tuplet 3/2 { a c a~ } a | } \\
      { r4 | e8 f g fis e4 | es2 d4 | <c e>8[ <b dis> <bes d>] <a cis>  <c f>4 }
    >>
  >>
  \new Staff {
    \time 3/4 \clef bass \key es \major
    \new Voice = "mel" \relative { g4 | c2 c4 | c g b | a2. | }
  }
  \new Lyrics \lyricsto "mel" { \lyricmode { la la le li lu la lo } }
  \new Lyrics \lyricsto "mel" { \lyricmode { ku ka ke ki ku ka ko } }
>>

% exporter can run without actually typesetting
\exportMusic #'Humdrum \music

%opts.exporter = #exportMusicXML
% or as a layout extension that is added to the layout
%\score {
%  \music
%  \layout {
%    \FileExport #opts
%  }
%  \midi {}
%}
