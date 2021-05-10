; -*- master: export-example.ly;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib)                                            %
;%              -----------                                                    %
;%                                                                             %
;% Library: lilypond-export                                                    %
;%          ===============                                                    %
;%                                                                             %
;% export foreign file formats with LilyPond                                   %
;%                                                                             %
;% lilypond-export is free software: you can redistribute it and/or modify     %
;% it under the terms of the GNU General Public License as published by        %
;% the Free Software Foundation, either version 3 of the License, or           %
;% (at your option) any later version.                                         %
;%                                                                             %
;% lilypond-export is distributed in the hope that it will be useful,          %
;% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
;% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
;% GNU General Public License for more details.                                %
;%                                                                             %
;% You should have received a copy of the GNU General Public License           %
;% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
;%                                                                             %
;% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
;% lilypond-export is maintained by Jan-Peter Voigt, jp.voigt@gmx.de           %
;%                                                                             %
;%       Copyright Jan-Peter Voigt, Urs Liska, 2017                            %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;% TODO ties, slurs, grace notes, bar lines

(define-module (lilypond-export api))

(use-modules
 (oll-core tree)
 (oll-core internal music-tools)
 (lilypond-export lily)
 (lilypond-export MusicXML)
 ;(lilypond-export Humdrum)
 (lily))

(re-export exportLilyPond)
(re-export exportMusicXML)
;(re-export exportHumdrum)

(define-public (load-exporter target)
  (let*
   ; We don't have access to os-path from a Scheme module
   ((oll-core-root #{ \getOption oll-core.root #})
    (oll-root (list-head oll-core-root (- (length oll-core-root) 1)))
    (here (string-join (append oll-root '("lilypond-export")) "/"))
    (module-file (format "~a/~a.scm" here target)))
   (ly:message "Going to load ~a\n" module-file)
   (load-from-path module-file)
   ; FIXME
   ; I don't know why export-lilypond is not visible here.
   ; In my MWE this approach worked perfectly.
   ; It's clear that load-from-path does succeed
   ; (because it fails if you change module-file by a character).
   export-lilypond))

; create duration from moment
(define-public (moment->duration mom)
  (ly:make-duration
   (ly:intlog2 (ly:moment-main-denominator mom)) 0
   (ly:moment-main-numerator mom)
   ))

; names of used context-properties
(define ctprop::voice-context-count 'voice-context-count)
(define ctprop::staff-context-count 'staff-context-count)
(define ctprop::lyric-context-count 'lyric-context-count)
(define ctprop::voice-id 'voice-id)
(define ctprop::staff-id 'staff-id)
(define ctprop::lyric-id 'lyric-id)
(define ctprop::music-export 'music-export)
(define ctprop::export-step 'music-export-step)
(define ctprop::lyrics 'lyric-events)

; The use of '@@' indicates bad code style! But how else can we add context properties in external code?
; add used context-properties
((@@ (lily) translator-property-description) ctprop::voice-context-count integer? "Count voice contexts")
((@@ (lily) translator-property-description) ctprop::staff-context-count integer? "Count staff contexts")
((@@ (lily) translator-property-description) ctprop::lyric-context-count integer? "Count staff contexts")
((@@ (lily) translator-property-description) ctprop::voice-id integer? "Voice ID")
((@@ (lily) translator-property-description) ctprop::staff-id integer? "Staff ID")
((@@ (lily) translator-property-description) ctprop::lyric-id integer? "Staff ID")
((@@ (lily) translator-property-description) ctprop::music-export tree? "Music export store")
((@@ (lily) translator-property-description) ctprop::export-step tree? "Music export step store")
((@@ (lily) translator-property-description) ctprop::lyrics list? "current lyric events")

; combine note-events to event-chord
(define (combine-notes current music)
  (define (artic-type music) (ly:music-property music 'articulation-type))
  (if (not (equal? (ly:music-length current)(ly:music-length music)))
      (ly:warning "durations differ: ~A ~A" current music))
  (cond
   ; only take rests, if there is no music
   ((music-is? music 'RestEvent) ; (and (ly:music? current)(eq? 'RestEvent (ly:music-property music 'name)))
     (if (ly:music? current) current music))
   ; if we already have an event-chord, add note-event
   ((music-is? current 'EventChord) ;(and (ly:music? current)(eq? 'EventChord (ly:music-property current 'name)))
     (let* ((elements (ly:music-property current 'elements))
            (artics (ly:music-property music 'articulations))
            (an1 (filter symbol? (map artic-type elements))))
       ; reset articulations and place them in the event-chord
       (ly:music-set-property! music 'articulations '())
       ; add element and alement articulations to event-chord
       (ly:music-set-property! current 'elements `(,@elements ,music ,@artics))
       current
       ))
   ; if current is rest, override it with music
   ((music-is? current 'RestEvent) ; (and (ly:music? current)(eq? 'RestEvent (ly:music-property current 'name)))
     music)
   ; create event-chord with two note-events
   ((ly:music? current)
    (let* ((artics1 (ly:music-property current 'articulations))
           (artics2 (ly:music-property music 'articulations))
           (an1 (filter symbol? (map artic-type artics1))))
      (ly:music-set-property! current 'articulations '())
      (ly:music-set-property! music 'articulations '())
      (make-music 'EventChord 'elements
        `(,current ,music ,@artics1
           ,@(filter (lambda (v) (not (memv (ly:music-property v 'articulation-type) an1))) artics2))
        'duration (ly:music-property music 'duration)
        )))
   (else music))
  )


; engraver to collect all note- and rest-events
(define-public collectVoice
  (lambda (context)
    (let ((id 0)
          ; beam- and tuplet-closing is always a step late ...
          (beam-time '(#f . #f))
          (tuplet-time '(#f . #f)))

      ; search for music-cause of grob
      (define (grob-cause grob)
        (cond
         ((ly:grob? grob) (grob-cause (ly:grob-property grob 'cause)))
         ((ly:music? grob) grob)
         ((ly:stream-event? grob) (grob-cause (ly:event-property grob 'music-cause)))
         (else #f)
         ))


      (make-engraver
       ((initialize trans)
        (let* ((staff-context (ly:context-find context 'Staff))
               (stvc (ly:context-property staff-context ctprop::voice-context-count 0))) ; How many voices are in the staff

          (set! stvc (1+ stvc))
          (ly:context-set-property! staff-context ctprop::voice-context-count stvc)
          (set! id stvc)
          (ly:context-set-property! context ctprop::voice-id id)

          (ly:message "init Voice ~A/~A (~A)" (ly:context-property context ctprop::staff-id) (ly:context-id context) id)

          ; export tree for one timestep
          (ly:context-set-property! context ctprop::export-step (tree-create 'timestep))
          ))

       ((start-translation-timestep trans)
        ; clear lyrics
        (ly:context-unset-property context ctprop::lyrics)
        ; export tree for this timestep
        (if (not (tree? (ly:context-property context ctprop::export-step)))
            (ly:context-set-property! context ctprop::export-step (tree-create 'timestep))))

       ((stop-translation-timestep trans)
        (let ((step (ly:context-property context ctprop::export-step)) ; export tree for this timestep
               (musicexport (ly:context-property context ctprop::music-export)) ; global export tree
               (bar (ly:context-property context 'currentBarNumber 1)) ; current bar number
               (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))) ; current measure position

          (ly:context-unset-property context ctprop::export-step)

          ; detect upbeat (partial)
          (if (ly:moment<? moment (ly:make-moment 0))
              (set! bar (1- bar)))

          ; if we have a tree for this timestep (why should'nt we?)
          (if (tree? step)
              ; copy to global export tree
              (tree-walk step '()
                (lambda (path xkey value) ; path in tree, key of current node, value of current node
                  (tree-set! musicexport `(,bar ,moment ,@path) value)
                  ) '(empty . #f)))
          ))

       (listeners
        ((StreamEvent engraver event) ; listen to any event
          (let ((musicexport (ly:context-property context ctprop::music-export))
                (musicstep (ly:context-property context ctprop::export-step))
                (music (ly:event-property event 'music-cause))
                (bar (ly:context-property context 'currentBarNumber 1))
                (moment (ly:context-property context 'measurePosition (ly:make-moment 0))))
            ; notes and rests are stored in the tree under measeure/moment/staff/voice
            ; TODO MultiMeasureRests, Upbeats
            (if (and (ly:music? music) (= 0 (ly:moment-grace moment))) ; Drop grace notes!
                (let* ((path (list bar moment
                               (ly:context-property context ctprop::staff-id)
                               (ly:context-property context ctprop::voice-id)))
                       (steppath (cddr path))
                       (notes (tree-get musicstep steppath)))
                  (ly:music-set-property! music 'timestamp (cons bar moment))
                  (cond
                   ((memq (ly:music-property music 'name) '(NoteEvent RestEvent))
                    (let ((dur (ly:event-property event 'duration)))

                      ; track shortest duration (musicXML/MEI divisions)
                      (let ((shortdur (tree-get musicexport '(division-dur))))
                        (if (and (ly:duration? dur)(or (not shortdur) (ly:duration<? dur shortdur)))
                            (tree-set! musicexport '(division-dur) dur))
                        )

                      ; if we already have a note, combine it to a eventchord
                      (if (ly:music? notes) (set! music (combine-notes notes music)))

                      ; tuplets
                      (let ((scale (ly:duration-scale dur)))
                        (if (not (integer? scale))
                            (let ((num (numerator scale))
                                  (den (denominator scale)))
                              ;(ly:message "scale ~A/~A" num den)
                              (tree-set! musicstep `(,@steppath scale) scale)
                              )))

                      ; remember current time
                      (ly:event-set-property! event 'timestamp (cons bar moment))

                      ; track time for beams
                      (if (not (and
                                (pair? (cdr beam-time))
                                (equal? (cadr beam-time) bar)
                                (equal? (cddr beam-time) moment)))
                          (set! beam-time (cons (cdr beam-time) (cons bar moment))))

                      ; track time for tuplets
                      (if (not (and
                                (pair? (cdr tuplet-time))
                                (equal? (cadr tuplet-time) bar)
                                (equal? (cddr tuplet-time) moment)))
                          (set! tuplet-time (cons (cdr tuplet-time) (cons bar moment))))

                      ; store music
                      (tree-set! musicstep steppath music)))

                   ((eq? (ly:music-property music 'name) 'TupletSpanEvent)
                    (let ((timestamp (ly:music-property music 'timestamp))
                          (num (ly:music-property music 'numerator))
                          (den (ly:music-property music 'denominator))
                          (dir (ly:music-property music 'span-direction)))
                      ;(ly:message "tuplet ~A:~A ~A ~A ~A" num den timestamp (cons bar moment) dir)

                      (cond
                       ((and (= -1 dir)(integer? num)(integer? den))
                        (tree-set! musicstep `(,@steppath tuplet) `(start . ,(/ num den))))

                       ((= 1 dir)
                        (let ((tup-time (cdr tuplet-time)))
                          ;(ly:message "tuplet time ~A ~A" tup-time (cons bar moment))

                          (tree-set! musicexport `(,(car tup-time) ,(cdr tup-time) ,@steppath tuplet) `(stop . #f))
                          ))
                       )))
                   )))
            ))
        )

       (acknowledgers
        ; store accidental
        ((accidental-interface engraver grob source-engraver)
         (let ((musicexport (ly:context-property context ctprop::music-export))
              (bar (ly:context-property context 'currentBarNumber 1))
              (staff-id (ly:context-property context ctprop::staff-id))
              (voice-id (ly:context-property context ctprop::voice-id))
              (moment (ly:context-property context 'measurePosition (ly:make-moment 0))))
           (tree-set! musicexport (list bar moment staff-id voice-id 'accidental) (accidental-interface::calc-alteration grob))
           ))

        ; store stem direction
        ((stem-interface engraver grob source-engraver)
         (let ((musicstep (ly:context-property context ctprop::export-step))
               (staff-id (ly:context-property context ctprop::staff-id))
               (voice-id (ly:context-property context ctprop::voice-id)))
           (ly:message "stem info ~A" (map car (ly:grob-properties grob)))
           (tree-set! musicstep `(,staff-id ,voice-id stem dir) (ly:grob-properties grob)) ;(ly:stem::calc-direction grob))
           ))
        )

       ; store beam span
       (end-acknowledgers
        ((beam-interface engraver grob source-engraver)
         (let ((musicexport (ly:context-property context ctprop::music-export))
               ;(musicstep (ly:context-property context ctprop::export-step))
               (staff-id (ly:context-property context ctprop::staff-id))
               (voice-id (ly:context-property context ctprop::voice-id))
               (bar (ly:context-property context 'currentBarNumber 1))
               (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))
               (cause (grob-cause grob)))
           ; we stored the start-time inside the causing music-event
           ; we stored the end-time in beam-time one timestep before this one
           (cond
            ((music-is? cause 'NoteEvent)
             (let ((start-timestamp (ly:music-property cause 'timestamp))
                   (end-timestamp (car beam-time)))
               (if (not (null? start-timestamp)) ; this skips grace notes
                   (tree-set! musicexport (list (car start-timestamp) (cdr start-timestamp) staff-id voice-id 'beam) 'start) )
               (if (not (null? end-timestamp)) ; this skips grace notes
                   (tree-set! musicexport (list (car end-timestamp) (cdr end-timestamp) staff-id voice-id 'beam) 'end))
               ;(ly:message "beam ~A ~A" start-timestamp end-timestamp)
               ))
            ((music-is? cause 'BeamEvent)
             (let ((start-timestamp (ly:music-property cause 'timestamp))
                   (end-timestamp (cons bar moment)))
               (tree-set! musicexport (list (car start-timestamp) (cdr start-timestamp) staff-id voice-id 'beam) 'start)
               (tree-set! musicexport (list (car end-timestamp) (cdr end-timestamp) staff-id voice-id 'beam) 'end)
               ;(ly:message "beam ~A ~A" start-timestamp end-timestamp)
               ))
            ; this should'nt appear!
            (else (ly:message "Beam? ~A" cause))
            )
           ))
        )
       ))))

; collect lyrics
(define-public collectLyrics
  (let ((id 0))
    (lambda (context)
      (make-engraver
       ((initialize trans)
        (let* ((score-context (ly:context-find context 'Score))
               (lyric-count (ly:context-property score-context ctprop::lyric-context-count)))

          (if (integer? lyric-count)
              (set! lyric-count (1+ lyric-count))
              (set! lyric-count 1))
          (set! id lyric-count)
          ; export tree for the current timestamp
          (ly:context-set-property! context ctprop::export-step (tree-create 'timestep))
          ))

       ((start-translation-timestep trans)
        (if (not (tree? (ly:context-property context ctprop::export-step)))
            (ly:context-set-property! context ctprop::export-step (tree-create 'timestep))))

       ((stop-translation-timestep trans)
        (let ((step (ly:context-property context ctprop::export-step))
              (musicexport (ly:context-property context ctprop::music-export))
              (bar (ly:context-property context 'currentBarNumber 1))
              (moment (ly:context-property context 'measurePosition (ly:make-moment 0))))
          (ly:context-set-property! context ctprop::export-step #f)
          (if (ly:moment<? moment (ly:make-moment 0))
              (set! bar (1- bar)))
          (if (tree? step)
              (tree-walk step '()
                (lambda (path xkey value)
                  (tree-set! musicexport `(,bar ,moment ,@path) value)
                  ) '(empty . #f)))
          ))

       (listeners
        ((lyric-event engraver event)
         (let ((musicexport (ly:context-property context ctprop::music-export))
               (musicstep (ly:context-property context ctprop::export-step))
               (text (ly:event-property event 'text))
               ; we need the associated voice context
               (voice (ly:context-property context 'associatedVoiceContext)))

           (if (ly:context? voice)
               (let* ((staff-id (ly:context-property voice ctprop::staff-id))
                      (voice-id (ly:context-property voice ctprop::voice-id))
                      (bar (ly:context-property context 'currentBarNumber 1))
                      (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))
                      (lpath (list staff-id voice-id 'lyrics))
                      (lyrics (ly:context-property voice ctprop::lyrics)))

                 ; create/extend list of lyric events found for the associated voice context
                 (set! lyrics (if (list? lyrics) `(,@lyrics ,text) (list text)))
                 (tree-set! musicstep lpath lyrics)
                 (ly:context-set-property! voice ctprop::lyrics lyrics))

               ; TODO if we have no associated voice context, what shall we do?
               (ly:message "syl ~A" text))
           ))
        )

       ))))


; engraver to group voices in one staff
(define-public collectStaff
  (lambda (context)
    (let ((id 0))
      (make-engraver
       ((initialize trans)
        ; TODO StaffGroup hierarchy! -> ly:context-property-where-defined !
        (let* ((parent-context (ly:context-find context 'Score)) ;(ly:context-parent context)) ; look in parent context for current staff count
                (psc (ly:context-property parent-context ctprop::staff-context-count 0)))
          (set! psc (1+ psc))
          (ly:context-set-property! parent-context ctprop::staff-context-count psc)
          (set! id psc)
          (ly:context-set-property! context ctprop::staff-id id)
          (ly:message "init Staff ~A" id)
          (ly:context-set-property! context ctprop::export-step (tree-create 'timestep))
          ))

       ((start-translation-timestep trans)
        (if (not (tree? (ly:context-property context ctprop::export-step)))
            (ly:context-set-property! context ctprop::export-step (tree-create 'timestep))))

       ((stop-translation-timestep trans)
        (let ((step (ly:context-property context ctprop::export-step))
              (musicexport (ly:context-property context ctprop::music-export))
              (bar (ly:context-property context 'currentBarNumber 1))
              (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))
              (mlen (ly:context-property context 'measureLength))
              (barline (ly:context-property context 'whichBar)))
          (ly:context-set-property! context ctprop::export-step #f) ; reset step store

          (if (ly:moment<? moment (ly:make-moment 0))
              (begin
               (set! bar (1- bar)) ; upbeat / partial
               ;(set! moment (ly:moment-add mlen moment))
               ))
          (tree-set! musicexport (list bar moment 'mlength) mlen)
          (tree-set! musicexport (list bar moment id 'mlength) mlen)

          (if (tree? step)
              (tree-walk step '()
                (lambda (path xkey value)
                  (tree-set! musicexport `(,bar ,moment ,@path) value)
                  ) '(empty . #f)))

          (if (and (string? barline)(not (equal? "" barline))(not (equal? "|" barline)))
              (begin
               (tree-set! musicexport (list bar moment 'barline) barline)
               (tree-set! musicexport (list bar moment id 'barline) barline)
               ))
          ))

       (listeners
        ((SetProperty engraver event)
         (let ((musicstep (ly:context-property context ctprop::export-step))
               (sym (ly:event-property event 'symbol))
               (val (ly:event-property event 'value))
               (staff-id (ly:context-property context ctprop::staff-id)))
           ; detect clefs
           (if (memq sym '(clefGlyph clefPosition clefTransposition))
               (tree-set! musicstep (list staff-id sym) val))
           ))

        ; detect key changes
        ((key-change-event engraver event)
         (let ((musicstep (ly:context-property context ctprop::export-step))
               (music (ly:event-property event 'music-cause))
               (staff-id (ly:context-property context ctprop::staff-id)))
           (tree-set! musicstep (list staff-id 'keysig) music)
           ))

        ; detect time signatures
        ((time-signature-event engraver event)
         (let ((musicstep (ly:context-property context ctprop::export-step))
               (staff-id (ly:context-property context ctprop::staff-id)))
           (tree-set! musicstep (list staff-id 'timesig)
             (cons (ly:event-property event 'numerator)(ly:event-property event 'denominator)))
           (tree-set! musicstep `(timesig)
             (cons (ly:event-property event 'numerator)(ly:event-property event 'denominator)))
           ))
        )
       ))))

; create score-export-engraver
(define-public scoreExporter
  ; engraver to export tree in foreign format
  (define-scheme-function (options)(list?)
    (let* ((target (ly:assoc-get 'target options 'Humdrum #f))
           (exporter (load-exporter target))
           (suffix (ly:assoc-get 'filesuffix options (object-property exporter 'file-suffix) #f))
           (filename (ly:assoc-get 'filename options
                       (format "~A.~A"
                         (ly:assoc-get 'filebase options (ly:parser-output-name) #f)
                         (if (string? suffix) suffix
                             (begin
                              (ly:input-warning (*location*) "no file suffix given!")
                              "dat"))) #f)))
      (lambda (context)
        (make-engraver
         ((initialize trans)
          (ly:message "init ~A: \"~A\"" (procedure-name exporter) filename)
          (ly:context-set-property! context ctprop::music-export (tree-create ctprop::music-export))
          )
         ((finalize trans)
          (let ((musicexport (ly:context-property context ctprop::music-export)))
            ; when score is finished, score is exported
            (tree-set! musicexport '(finaltime)
              (cons (ly:context-property context 'currentBarNumber) (ly:context-property context 'measurePosition)))
            ;(for-each (lambda (sym) (ly:message "~A: ~A" sym (tree-get musicexport (list sym))))
            ;  (filter symbol? (tree-get-keys musicexport '())))
            ;(tree-display musicexport)
            (exporter musicexport filename)
            ))
         ))
      )))

; create output-definition (layout) with file exporter
(define-public FileExport
  (define-scheme-function (options)(list?)
    #{
      \layout {
        \context {
          \Voice
          \consists #collectVoice
        }
        \context {
          \Staff
          \consists #collectStaff
        }
        \context {
          \Lyrics
          \consists #collectLyrics
        }
        \context {
          \Score
          \consists \scoreExporter #options
        }
      }
    #}))

