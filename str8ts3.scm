(module str8ts3)

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
; TODO eventuell mit bei make-str8ts einschließen?
(define make-grid
   (lambda (n)
    (let (
      (the-grid
        (vector-map
          (lambda (e)
            (vector-map (lambda (_) (iota n 1)) (make-vector n))
          )
          (make-vector n)
        ))
      (size n))
      (letrec(

        (get (lambda (x y)
          (vector-ref (vector-ref the-grid x) y)))

        (set (lambda (x y value)
          ; falls Zahl: gültiger Wert?
          (if (and (number? value) (or (< value (- n)) (> value n)))
            (error "make-grid" "invalid value" value))
          (vector-set! (vector-ref the-grid x) y value)

          ; falls Zahl: Möglichkeiten aus anderen Spalten/Zeilen entfernen
          (if (number? value)
            (begin
              (let this ((e 0))
                (if (< e size)
                  (let((value' (get e y)) (value'' (get x e)))
                    (if (list? value') (remq! (abs value) value'))
                    (if (list? value'') (remq! (abs value) value''))
                    (this (+ e 1))))))))))
        (lambda (message x y . params)
          (cond
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

(define make-str8ts
  (lambda (n)
    (let ((grid (make-grid n))
      (size n))
      (letrec*
        (
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
          (each (lambda (fn)
            (fold (lambda (x y value prev) (fn x y value)) #f)
            #f
          ))
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
            ; Gibt das Grid auf der Konsole aus.
            ((print)
              (each (lambda (x y value)
                (display
                  (cond
                    ; ((list? value) ".")
                    ((list? value) value)
                    ((< value 0) (schwarz (number->string (- value))))
                    ((= value 0) (schwarz " "))
                    (else value)
                  )
                )
                (if (= x (- n 1)) (display "\n"))
              ))
            )
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
            (else
              (if (>= (length params) 2)
                (apply grid message params)
                (error "make-str8ts" "Unknown message" message)))
          )
        )
      )
    )
  )
)

; Initialisiert ein str8ts, belegt die Werte vor und ruft danach 'lösen auf
(define str8ts
  (lambda values
    (let ((s (make-str8ts (sqrt' (length values)))))
      (apply s 'set-all values)
      (s 'print)
      (print "Lösbar: " (s 'solvable?))
      (print "Gelöst? " (s 'solved?))
    )
  )
)

; http://www.str8ts.com/str8ts_6x6_sample_pack.pdf
(str8ts ; Easy No. 1
  -5 -0  ?  ?  2 -0
   ?  6  ?  ?  ? -0
   ?  ? -0 -0  5  ?
   2  ? -0 -6  ?  ?
  -0  ?  ?  ?  ?  ?
  -0  ?  5  ? -1 -2
)
