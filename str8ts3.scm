; TODO get-column und select mit Funktion, die mindestens die Parameter (x y value) hat, ersetzen

(module str8ts3)

; ausführliche Ausgabe für 'print?
(define verbose #t)

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

; Konvertiert eine Liste aus Zahlen zu einem String
(define list->string' (lambda (c)
  (if (null? c)
    ""
    (let((text (number->string (car c))) (rest (list->string' (cdr c))))
      (if (equal? "" rest)
        text
        (string-append text "+" rest)
    )))))

; n*n-Grid
; TODO eventuell mit bei make-str8ts einschließen?
(define make-grid
   (lambda (n)
    (let (
      (the-grid
        (vector-map
          (lambda (e)
            (vector-map (lambda (_) (iota n 1)) (make-vector n)))
          (make-vector n)))
      (size n))
      (letrec(

        (get (lambda (x y)
          (vector-ref (vector-ref the-grid x) y)))

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
        (get-column (lambda (x y)
          (cond ((number? y) (select (cons 0 size) y))
                ((number? x) (select x (cons 0 size))))))

        (raw-set (lambda (x y value)
          (vector-set! (vector-ref the-grid x) y value)))
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
            ((eqv? message 'serialize) (obj->string the-grid))
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
          ; ende der Liste
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

          ; Einzelne Werte einsetzen
          (fill-singles (lambda ()
            (each (lambda (x y e)
              (if (and (list? e) (= 1 (length e)))
                (begin
                  (print "setze x=" x ", y=" y " auf " e)
                  (grid 'set x y (car e))
                  (print-all)
                ))))))

          ; Werte, die nicht möglich sind, entfernen
          (compartment-check (lambda (c)
            (if (null? c) #f
              (letrec*(
                  (values (grid 'select (caar c) (cdar c)))
                  (len' (length values))
                  (numvalues (filter number? values))
                )
                (if (not (null? numvalues))
                  (map
                    (lambda (e index)
                      (if (list? e)
                        (begin
                          (print "Feld hat Werte " e ", Werte max. im Bereich von ["
                            (- (apply max numvalues) len' -1) " ... "
                            (+ (apply min numvalues) len' -1) "]")

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

          ; Hash ist der Wert, der ursprünglich (vor der versuchten Lösung) erstellt wurde.
          ; Kommt hier ein anderer Wert raus, wurde durch den Lösungsschritt (called) eine
          ; Änderung vorgenommen.
          (to-string (lambda ()
            (grid 'serialize 0 0)))
;            (let((s ""))
;              (each (lambda (x y value)
;                (set! s (string-append s ","
;                  (if (list? value)
;                    (string-append "(" (list->string' value) ")")
;                    (number->string value))))))
;              s)))

          (diff (lambda (hash called extra)
            (print "Änderung durch " extra "? " (not (equal? (to-string) hash))); "; " (to-string))
            (not (equal? (to-string) hash))
          ))

          ; alle Lösungsmöglichkeiten probieren
          (solve (lambda ()
            (let((hash (to-string)))
              (print "Neuer Versuch..."); hash)
              (if
                (or
                  (diff hash (fill-singles) "fill-singles")
                  (diff hash (compartment-check compartments) "compartment-check")
                  ; als letztes: allgemeine Lösungsstrategie mit Backtracking
                  #f
                )
                (begin
                  (print-all)
                  (solve)
                )
                #f))))

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

            ; Gibt true zurück, falls das Grid gelöst ist.
            ; Gelöst ist es, falls es kein Feld mehr gibt, welches keinen Wert enthält.
            ; (d.h. enthält Liste von Möglichkeiten)
            ((solved?)
              (fold (lambda (x y value prev)
                (and prev (not (list? value)))) #t))


            ; (schlechte) Abschätzung, ob das Str8ts noch gelöst werden kann:
            ; dies ist nur dann der Fall, wenn in jedem Feld mindestens eine Möglichkeit existiert
            ; oder dort bereits eine Zahl eingetragen wurde.

            ; Leere Felder komplett ohne Möglichkeiten sind nicht lösbar, weil ja nichts
            ; eingetragen werden kann.
            ((solvable?)
              (fold (lambda (x y value prev)
                (and prev
                  (or
                    (not (list? value))
                    (> (length value) 0))
                )) #t))

            ; Lösung versuchen.
            ((solve) (solve))

            (else
              (if (>= (length params) 2)
                (apply grid message params)
                (error "make-str8ts" "Unknown message" message)))))))))

; Initialisiert ein str8ts, belegt die Werte vor und ruft danach 'lösen auf
(define str8ts
  (lambda values
    (let ((s (make-str8ts (sqrt' (length values)))))
      (apply s 'set-all values)
      (s 'find-compartments)
      (print "Lösbar: " (s 'solvable?))
      (print "Gelöst? " (s 'solved?))
      (s 'solve)
      (s 'print)
      (print (if (s 'solved?) "Gelöst" "Nicht lösbar"))
)))

; http://www.str8ts.com/str8ts_6x6_sample_pack.pdf
#|
(str8ts ; Easy No. 1
  -5 -0  ?  ?  2 -0
   ?  6  ?  ?  ? -0
   ?  ? -0 -0  5  ?
   2  ? -0 -6  ?  ?
  -0  ?  ?  ?  ?  ?
  -0  ?  5  ? -1 -2
)
|#

(str8ts
  -0  ?  ? -3 -0 -0
  -1  ?  ?  ?  ?  ?
   ?  ? -0  6  ?  ?
   ?  ?  3 -0  ?  ?
   ?  ?  ?  ?  4 -0
  -5 -6 -0  ?  ? -0)