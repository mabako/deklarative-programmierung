; nur mit Bigloo 4.0 getestet, vector-map gibt es z.B. nicht in älteren Versionen.
(module str8ts3)

; ausführliche Ausgabe für 'print?
(define verbose #f)

; Nur Backtracking verwenden?
(define backtrack-only #f)

; allgemeiner unbekannter Wert
(define ? '?)

; Schwarzer Text (auf weißer Konsole getestet)
(define schwarz (lambda (z) (string-append "\033[37;1;40m" z "\033[0m")))

; Wurzel zu ganzer Zahl - falls eine Zahl übergeben wurde,
; deren Wurzel keine Ganzzahl ist, tritt ein Fehler auf.
(define sqrt'
  (lambda (zahl)
    (let((wurzel (flonum->fixnum (sqrt zahl))))
      (if (= (* wurzel wurzel) zahl)
        wurzel
        (error 'sqrt' "Wurzel ist keine ganze Zahl" zahl)))))

; n*n-Grid
(define make-grid
   (lambda (n)
    (let (
      (the-grid
        ; leeres Grid mit neuen Vektoren anlegen
        (vector-map
          (lambda (e)
            (vector-map (lambda (_) (iota n 1)) (make-vector n)))
          (make-vector n)))
      (size n))
      (letrec(
        ; Gibt einen einzelnen Wert zurück.
        (get (lambda (x y)
          (vector-ref (vector-ref the-grid x) y)))

        ; Auswahl eines Intervalls, entweder als Teil einer Spalte oder als
        ; Teil einer Reihe.
        ; Veränderungen am Rückgabewert müssen über set erfolgen.
        (select (lambda (x y)
          (if (pair? x)
            ; [(car x) ... (cdr x)] als Intervall
            (take (drop (vector->list
              (vector-map (lambda (e) (vector-ref e y)) the-grid)) (car x)) (- (cdr x) (car x)))
            ; [(car y) ... (cdr y)] als Intervall
            (take (drop (vector->list (vector-ref the-grid x)) (car y)) (- (cdr y) (car y)))
          )
        ))

        ; Gibt eine gesamte Spalte oder Reihe zurück, abhängig
        ; vom Parameter x/y. Für Spalten ist dies beispielsweise
        ; (get-column x #f).
        (get-column (lambda (x y)
          (cond ((number? y) (select (cons 0 size) y))
                ((number? x) (select x (cons 0 size))))))

        ; Setzt einen Wert ohne Rücksicht auf eventuelle Einschränkung
        ; vorhandener Möglichkeiten in allen anderen Werten.
        (raw-set (lambda (x y value)
          (vector-set! (vector-ref the-grid x) y value)))

        ; Setzt alle Werte in einer Zeile und Spalte.
        ; Entweder x oder y sollte #f sein, analog wie zu (select)
        (set-column (lambda (x y values)
          (let((index 0))
            (map
              (lambda (e)
                (if (number? x) (raw-set x index e) (raw-set index y e))
                (set! index (+ 1 index))
              )
              values
            )
          )
        ))

        ; Setzt einen Wert und entfernt diesen als Möglichkeit zu allen
        ; anderen Werten in dieser Zeile/Spalte.
        (set (lambda (x y value)
          ; falls Zahl: gültiger Wert? [-n .. 0 .. n] sind gültig, da <= 0 schwarze Felder sind.
          (if (and (number? value) (> (abs value) n))
            (error "make-grid/set" "invalid value" value))
          (raw-set x y value)

          ; falls Zahl: Möglichkeiten aus anderen Spalten/Zeilen entfernen
          (if (number? value)
            (begin
              (set-column x #f (map (lambda (e) (if (list? e) (remq (abs value) e) e)) (get-column x #f)))
              (set-column #f y (map (lambda (e) (if (list? e) (remq (abs value) e) e)) (get-column #f y)))
            ))))

        ; relatives Setzen eines Wertes - pos enthält ein pair aus (x y)
        ; und addiert - relativ gesehen - den Wert von index dazu.
        ; Damit entfällt das Ausrechnen der Position für Compartments (welche in einer
        ; Liste unabhängig von der Ausrichtung gespeichert sind).
        (set2 (lambda (pos index value)
          (if (pair? (car pos))
            (set (+ (caar pos) index) (cdr pos) value)
            (set (car pos) (+ (cadr pos) index) value)
        ))))

        (lambda (message x y . params)
          (cond
            ; Bounds-Prüfung nur begrenzt für booleans sinnvoll.
            ((eqv? message 'get-column) (get-column x y))
            ((eqv? message 'select) (select x y))
            ((eqv? message 'set2) (set2 x y (car params)))

            ; Speichern und Wiederherstellen des Zustands.
            ((eqv? message 'serialize) (obj->string the-grid))
            ((eqv? message 'deserialize) (set! the-grid (string->obj x)))

            ((or (< x 0) (>= x size))
              (error "make-grid" "x out of bounds" x))
            ((or (< y 0) (>= y size))
              (error "make-grid" "y out of bounds" y))
            (else
              (case message
                ((get) (get x y))
                ((set) (if (null? params)
                  (error "make-grid" "No value to set" params)
                  (set x y (car params))))
                (else
                  (error "make-grid" "Unknown message" message))))))))))

; Hilfsfunktion zum aufteilen von Compartments an den 'schwarzen' Feldern
(define split-compartments
  (lambda (liste)
    (let this ((L liste) (index 0) (start #f) (compartments '()))
      (let((new-list (if (boolean? start) compartments (cons (cons start index) compartments))))
        (cond
          ; Ende der Liste
          ((null? L) new-list)

          ; Ende des Compartments
          ((and (not (list? (car L))) (<= (car L) 0))
            (this (cdr L) (+ index 1) #f new-list))
          (else
            ; eventuell fängt hier das Compartment an
            (this (cdr L) (+ index 1) (if (boolean? start) index start) compartments)
          ))))))

(define make-str8ts
  (lambda (n)
    (let ((grid (make-grid n))
      (compartments '())
      (size n))
      (letrec*
        (
          ; Folding: übergibt der Funktion fn (x y value previous-value), mit wert als
          ; Startwert für previous-value. Nach jedem Schritt wird der Wert, der zurückgegeben
          ; wird, als previous-value verwendet.
          ; Das letzte Ergebnis wird zurückgegeben.
          (fold (lambda (fn wert)
            (let this ((x 0) (y 0) (w wert))
              (if (< y n)
                (let((x' (+ x 1)) (w' (fn x y (grid 'get x y) w)))
                  (this
                    (modulo x' n)
                    (+ y (if (= x' n) 1 0))
                    w'
                  ))
                w
              ))))

          ; Variante von fold, die die Funktion fn ohne Wissen des vorherigen Wertes ausführt und
          ; unspecified zurückgibt.
          (each (lambda (fn)
            (fold (lambda (x y value prev) (fn x y value)) (unspecified))
            (unspecified)
          ))

          ; Einzelne Werte einsetzen - einfach in jedem Feld prüfen,
          ; ob dort nur ein Element enthalten ist, und den ggf. setzen
          (fill-singles (lambda ()
            (each (lambda (x y e)
              (if (and (list? e) (= 1 (length e)))
                (begin
                  (if verbose (print "setze x=" x ", y=" y " auf " (car e)))
                  (grid 'set x y (car e))
                  ; (print-all)
                ))))))

          ; Werte, die nicht möglich sind, entfernen.
          ; Dies ist der Fall, wenn in einem Compartment bereits mindestens ein Wert
          ; steht und dadurch garantiert wird, dass bestimmte Werte nicht mehr eingesetzt werden
          ; können.
          (compartment-check (lambda (c)
            (if (null? c) #f
              (letrec*(
                  (values (grid 'select (caar c) (cdar c)))
                  (len' (length values))
                  (numvalues (filter number? values))
                )
                (if (not (null? numvalues)) ; Sind im Compartment Werte enthalten?
                  (map ; alle Elemente im Compartment durchgehen
                    (lambda (e index)
                      (if (list? e)
                        (begin
                          (if #f
                            (print "Feld hat Werte " e ", Werte max. im Bereich von ["
                              (- (apply max numvalues) len' -1) " ... "
                              (+ (apply min numvalues) len' -1) "]"))

                          ; alle Möglichkeiten entfernen, die außerhalb des noch verfügbaren
                          ; Rahmens liegen (ausgehend von Minimum und Maximum im Bereich)
                          (grid 'set2
                            (car c) ; Position als pair
                            index ; Offset, der zur Position addiert werden muss
                            (filter (lambda (x) (> x (- (apply max numvalues) len')))
                              (filter (lambda (x) (< x (+ (apply min numvalues) len'))) e)))
                        )
                      )
                    ) values (iota len')))
                (compartment-check (cdr c))
              )
          )))

          ; entfernt einzelne Ziffern, die keinen direkten Vorgänger oder nachfolger
          ; in diesem Compartment haben. z.B. (1 2) (1 2 4) - gibt keine 3, damit entfällt die 4.
          ; Jetzt neu auch mit Sequenzen. Löst damit gewisse Pair/Triple-Situationen auf, wenn zB.
          ; in einem Compartment (1 2) (1 2 3) (1 2) steht, prüft es nachfolgend, ob
          ;    (1 2 3) -> wenn entweder 1 oder 2 aus dem ganzen restlichen rausgestrichen werden,
          ;       sind nicht mehr genug Zahlen zur Lösung vorhanden. Diese werden somit entfernt.
          (stranded (lambda (c)
            (if (null? c) #f
              (letrec*(
                  (values (grid 'select (caar c) (cdar c)))
                  (len' (length values))
                )
                (if (> (length values) 1) ; für eine einzelne Zelle sind Vorgänger/Nachfolger egal.
                  (map
                    (lambda (e index)
                      (if (list? e)
                        (letrec*
                          (
                            (all-values
                              (delete-duplicates
                                (let this ((i index) (v values))
                                  (cond ((null? v) '())
                                        ((= i 0) (this -1 (cdr v)))
                                        (else (append (this (- i 1) (cdr v)) (if (number? (car v)) (list (car v)) (car v)))))
                                )
                              )
                            )
                            (possible? (lambda (value start)
                              ; von im Intervall von [-len' ... len'] suchen, ob eine einzige durchgängige
                              ; Zahlenfolge der Länge len' existiert.
                              (if (> start 0) #f
                                (let ((erwartet (remq value (iota len' (+ value start)))))
                                  (if (find (lambda (e) (not (memq e all-values))) erwartet)
                                    (possible? value (+ start 1))
                                    #t
                                  )
                                )
                              )
                            ))
                          )
                          ; alle Möglichkeiten entfernen, die außerhalb des noch verfügbaren
                          ; Rahmens liegen (ausgehend von Minimum und Maximum im Bereich)
                          (grid 'set2
                            (car c) ; Position als pair
                            index ; Offset, der zur Position addiert werden muss
                            (filter (lambda (x) (possible? x (- 1 len'))) e)
                          )
                        )
                      )
                    ) values (iota len')))
                (stranded (cdr c))
              )
          )))

          ; Prüft, ob alle Compartments mit logisch gültigen Werten besetzt sind.
          ; Tritt (aktuell?) nur als Möglichkeit bei Weekly Extreme Str8ts auf.
          (valid? (lambda ()
            (letrec*((same? (lambda (a b)
                ; haben 2 Listen die gleichen Elemente, egal, in welcher Reihenfolge
                (if (null? a) (null? b)
                  (and
                    (= (length a) (length b))
                    (memq (car a) b)
                    (same? (cdr a) (remq (car a) b)))))))
              (let this ((c compartments))
                (if (null? c)
                  #t
                  (letrec*(
                      (values (grid 'select (caar c) (cdar c)))
                      (len' (length values))
                      (numvalues (filter number? values))
                    )
                    (if (eq? len' (length numvalues))
                      (and (same? (iota len' (apply min numvalues)) numvalues) (this (cdr c)))
                      ; sonst, nicht alle Compartments gefüllt.
                      (this (cdr c))
                    )
                  )
                )
              )
          )))

          ; Hash ist der Wert, der ursprünglich (vor der versuchten Lösung) erstellt wurde.
          ; Kommt hier ein anderer Wert raus, wurde durch den Lösungsschritt (called) eine
          ; Änderung vorgenommen.
          (to-string (lambda ()
            (grid 'serialize 0 0)))
          (diff (lambda (hash called extra)
            (if (not (equal? (to-string) hash)) (print "Änderung durch " extra))
            (not (equal? (to-string) hash))
          ))

          ; Backtracking, besteht aus den nachfolgenden beiden Funktionen.
          ; Hier: Einzelnen Wert einsetzen und versuchen, ob eine Lösung möglich ist.
          (do-backtrack (lambda (state x y e)
            (print "Backtrack [" x "," y "] -> " e)
            (if (null? e) 'failed-backtrack
              (begin
                (grid 'deserialize state #f) ; Ausgangszustand wiederherstellen.
                (grid 'set x y (car e)) ; Wert experimentell setzen.
                (print-all)
                (solve) ; Lösen... vielleicht?
                (if (and (solvable?) (solved?))
                  (print "Lösung scheint OK.")
                  (do-backtrack state x y (cdr e)))))))
          ; Freies Feld suchen, d.h. Feld mit ohne Werten.
          (backtrack (lambda ()
            (fold (lambda (x y e w)
              (if w #t
                (if (list? e)
                  (let ((state (grid 'serialize #f #f)))
                    (do-backtrack state x y e)
                    #t)
                  #f)))
              #f)))

          ; alle Lösungsmöglichkeiten probieren
          (solve (lambda ()
            (if (solvable?)
              (let((hash (to-string)))
                (if
                  (and (not backtrack-only)
                    (or ; alle der drei "normalen" Lösungsmöglichkeiten probieren.
                    ; insbesondere gibt (diff) true zurück, wenn sich mindestens ein
                    ; Feld (wenn auch nur eine Möglichkeit) geändert hat.
                    (diff hash (fill-singles) "einsetzen")
                    (diff hash (compartment-check compartments) "compartment")
                    (diff hash (stranded compartments) "stranded")
                  ))
                  (begin
                    (if verbose (print-all))
                    (solve)
                  )
                  (backtrack)))
              #f)
          ))

          ; Gibt true zurück, falls das Grid gelöst ist.
          ; Gelöst ist es, falls es kein Feld mehr gibt, welches keinen Wert enthält.
          ; (d.h. enthält Liste von Möglichkeiten)
          (solved? (lambda ()
            (fold (lambda (x y value prev)
              (and prev (not (list? value)))) #t)))

          ; (schlechte) Abschätzung, ob das Str8ts noch gelöst werden kann:
          ; dies ist nur dann der Fall, wenn in jedem Feld mindestens eine Möglichkeit existiert
          ; oder dort bereits eine Zahl eingetragen wurde.

          ; Leere Felder komplett ohne Möglichkeiten sind nicht lösbar, weil ja nichts
          ; eingetragen werden kann.
          (solvable? (lambda ()
            (and
              (fold (lambda (x y value prev)
                (and prev
                  (or
                    (not (list? value))
                    (> (length value) 0))
                )) #t)
            ; außerdem sollen alle Compartments gültig sein
              (if (not (valid?))
                (begin (print "Ungültige Lösung...") #f)
                #t))))


          ; Ausgabe
          (print-all (lambda ()
            (each (lambda (x y value)
              (if (= x 0) (display "  "))
              (display
                (cond
                  ((and verbose (list? value)) value)
                  ((list? value) ".")
                  ((< value 0) (schwarz (number->string (- value))))
                  ((= value 0) (schwarz " "))
                  (else value)))
              (if (= x (- n 1)) (display "\n"))))))
        )
        (lambda (message . params)
          (case message
            ; übernimmt alle initialen Werte. Im speziellen wird hier set aufgerufen, welches sich
            ; um die Entfernung der Möglichkeiten kümmert. Dies wäre bei "normaler" Initialisierung
            ; (einfaches Setzen der Werte) nicht so leicht
            ((set-all)
              (let((L (list-split params n)))
                (each (lambda (x y _)
                  (let((value (list-ref (list-ref L y) x)))
                    (if (number? value)
                      (grid 'set x y value)))))))

            ; Compartments herausfinden. Damit kann dann später leichter gesucht werden,
            ; wenn es z.B. um range-checks geht.
            ((find-compartments)
              (let this ((e 0))
                (if (< e size)
                  (begin
                    (map (lambda (x) (set! compartments (cons (cons e x) compartments)))
                      (split-compartments (grid 'get-column e #f)))
                    (map (lambda (y) (set! compartments (cons (cons y e) compartments)))
                      (split-compartments (grid 'get-column #f e)))
                    (this (+ e 1))))) (print compartments))

            ; Gibt das Grid auf der Konsole aus.
            ((print) (print-all))

            ((solved?) (solved?))

            ; Lösung versuchen.
            ((solve) (solve))

            (else
              (if (>= (length params) 2)
                (apply grid message params)
                (error "make-str8ts" "Unknown message" message)))))))))

; Initialisiert ein str8ts, belegt die Werte vor und ruft danach 'lösen auf
(define str8ts
  (lambda (values)
    (let ((s (make-str8ts (sqrt' (length values)))))
      (apply s 'set-all values)
      (s 'find-compartments)
      (s 'print)

      (s 'solve)

      (s 'print)
      (print (if (s 'solved?) "Gelöst" "Nicht lösbar"))
)))

; http://www.str8ts.com/str8ts_6x6_sample_pack.pdf
(define easy '( ; Easy No. 1
  -5 -0  ?  ?  2 -0
   ?  6  ?  ?  ? -0
   ?  ? -0 -0  5  ?
   2  ? -0 -6  ?  ?
  -0  ?  ?  ?  ?  ?
  -0  ?  5  ? -1 -2
))

(define moderate '( ; Moderate No. 1
  -0  ?  ? -3 -0 -0
  -1  ?  ?  ?  ?  ?
   ?  ? -0  6  ?  ?
   ?  ?  3 -0  ?  ?
   ?  ?  ?  ?  4 -0
  -5 -6 -0  ?  ? -0
))

(define first '( ; "My First Str8ts"
  -4  ?  ? -7 -0  ?  ?  ? -9
   ?  ?  7 -0  ?  ? -6  ?  ?
   ?  ? -8 -0  2  ? -0  3  ?
   9  7  ?  ?  ? -0  ?  ? -3
  -1  ?  ?  9  ?  ?  ?  7 -0
  -0  ?  3 -0  ?  ?  4  ?  ?
   3  ? -0  ?  ? -0 -0  ?  8
   2  ? -0  ?  ? -0  ?  ?  ?
  -0  ?  ?  ? -0 -5  9  ? -0
))

; Solver-Beispiele; Easy 1 Str8ts
(define easy2 '(
  -6  ?  3 -0 -0  ?  ? -0 -0
   4  1  ?  3 -0  ?  8  ?  6
   ?  ? -0  ?  ? -0  ?  ?  ?
   ?  ?  ? -2  ?  ?  ?  7 -0
  -0 -9  ?  ?  6  ?  ? -0 -0
  -0  ?  ?  ?  ? -0  ?  ?  ?
   ?  ?  8 -0  4  ? -0  6  ?
   ?  ?  9  ? -1  2  ?  ?  ?
  -0 -0  5  6 -0 -4  2  ? -0
))
; Solver-Beispiele; Moderate 1 Str8ts
; Stranded Digits sind hier von Bedeutung, da dadurch ohne Backtracking eine Lösung gefunden wird.
(define moderate2 '(
  -0  ?  ?  ?  7 -0 -0  ?  1
  -5  ?  ?  7  ? -0  ?  ?  ?
   ?  3 -1  ?  ?  ?  ?  ? -0
   4  2  ? -0 -8  ?  ?  ? -0
   ?  ?  ?  ? -0  ?  5  ?  ?
  -0  4  ?  ? -0 -0  ?  ?  6
  -0  7  ?  ?  ?  ? -0  ?  ?
   ?  ?  ? -9  ?  ?  ?  3 -0
   ?  9 -7 -0  1  ?  ?  ? -0
))

; Solver-Beispiele; Tough 1 Str8ts
(define tough '(
  -0 -0  ?  6 -0  ?  ? -0 -3
  -0  ?  ?  2 -0  ?  ?  ? -0
   2  ? -0  ?  3  ? -0  ?  ?
   ?  ?  ?  5  ?  ?  ?  7  ?
  -0 -0  ?  ?  ?  1  ? -6 -0
   ?  7  ?  ?  ?  ?  ?  ?  ?
   ?  6 -0  ?  ?  ? -9  ?  ?
  -5  ?  ?  ? -1  ?  ?  ? -0
  -0 -0  ?  ? -7  ?  ? -0 -0
))

; Solver-Beispiele; Diabolical 1 Str8ts
(define diab '(
  -4  9  ?  ? -3 -6  ?  ? -5
   ?  ?  ?  ? -0  ?  ?  ?  ?
   ?  ? -0 -2  ?  ? -0  ?  ?
   ?  ?  ?  ?  ?  ?  ?  ?  ?
  -0 -0  3  ?  ?  4  ? -1 -0
   ?  ?  ?  ?  ?  ?  ?  ?  6
   ?  ? -0  ?  ? -0 -0  ?  8
   1  ?  ?  ? -9  ?  ?  ?  ?
  -0  ?  ? -0 -0  ?  ?  ? -0
))

; Daily Str8ts 1890
(define daily '(
   8  ? -0 -0  ?  ?  ? -0 -0
   7  ? -0  ?  ? -0  ?  ? -6
  -0  ?  ?  ? -0 -9  ?  ?  1
   ?  ?  ?  ?  6  ? -0  ?  ?
   ?  ? -1  ?  ?  ? -0  ?  ?
   ?  ? -0  ?  ?  8  ?  4  ?
   3  ?  ? -0 -0  7  ?  ? -0
  -0  ?  ? -2  ?  ? -0  ?  ?
  -0 -0  ?  ?  ? -0 -4  ?  ?
))

; The Weekly Extreme Str8ts Puzzle
; #187, January 19 - January 25
(define weekly '(
   ?  7  6  ?  ? -0  ?  ?  3
   ?  ?  ?  ?  ?  ? -0  ?  ?
   ?  ?  ?  ? -9  ?  ?  4  ?
   ?  ?  ?  ?  ?  ?  ? -0 -0
   ?  ? -0  8  ?  ?  ?  ?  7
   ?  ? -1  ?  ? -0  ?  ?  ?
   5  ?  ?  ? -0  ?  ? -0  ?
   2  ?  ?  ?  ?  ?  ?  ?  ?
  -0 -0 -3  ?  ?  ?  ?  7  ?
))

(repl)