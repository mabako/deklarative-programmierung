; Möglichkeiten mit ausgeben
(define verbose #t)

; Hilfsfunktion: sqrt auf ganze Zahlen
(define sqrt'
  (lambda (zahl)
    (let((wurzel (flonum->fixnum (sqrt zahl))))
      (if (= (* wurzel wurzel) zahl)
        wurzel
        (error 'sqrt' "Wurzel ist keine ganze Zahl" zahl)))))

; ? als Wert noch unbekannt
(define ? '?)

; Zeichen ausgeben: für negative Zahlen wird dies schwarz
(define print-char
  (lambda (c)
    (let
      ((schwarz (lambda (z) (display (string-append "\033[37;1;40m" z "\033[0m")))))
      (cond
        ((or (not (number? c)) (> c 0)) (display c))
        ((< c 0) (schwarz (number->string (- c))))
        ((= c 0) (schwarz " "))))))


; s * s-Grid: str8ts sind immer symetrisch
(define make-str8ts
  (lambda (s)
    (let
      (
        (grid (make-vector s (make-vector s (iota s 1)))) ; mit (1 ... s) vorbelegen
        (size s)

        (each (lambda (fn . v)
          (apply vector-map!
            (lambda v'
              (apply vector-map fn v')
            )
            v)))
      )
      (lambda (message . params)
        (cond
          ; Initialisiert die Werte mit den Anfangswerten
          ((eqv? message 'init)
            (let*((werte (car params))
              ; für vector-map in ein s*s-Vektor konvertieren
              (alsvektor (list->vector (map list->vector (list-split werte s)))))
              ; jeden Wert mappen
              (each (lambda (a b)
                  (if (eqv? b '?) a (list b))
                )
                grid alsvektor)
            )
          )
          ; Ausgeben
          ((eqv? message 'print)
            (vector-map (lambda (e)
              (vector-map (lambda (x)
                (print-char
                  (cond
                    ((list? x) (if (= (length x) 1) (car x) (if verbose x ".")))
                    (else x)
                  ))
              ) e)
              (print)
            ) grid)
          )
          (else (error 'make-str8ts "Unknown message" message))
        )
      )
    )
  )
)

(define str8ts
  (lambda werte
    (let ((s (make-str8ts (sqrt' (length werte)))))
      (s 'init werte)
      (s 'print)
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
#| (lambda (m n)
      (let ((the-grid (make-vector m
       (make-vector n 0)))
      (size-x m)
      (size-y n))
   (letrec ((get (lambda (x y)

        (vector-ref (vector-ref the-grid x) y)))
      (set (lambda (x y value)
        (vector-set! (vector-ref the-grid x) y value)
        value)))
      (lambda (message x y . params)
         (cond
      ((or (< x 0) (>= x size-x))
       (error "make-grid" "x out of bounds" x))
      ((or (< y 0) (>= y size-y))
       (error "make-grid" "y out of bounds" y))
      (else
       (case message
          ((get) (get x y))
          ((set) (if (null? params)
         (error "make-grid" "No value to set" params)
         (set x y (car params))))
          (else
           (error "make-grid" "Unknown message" message))))))))))

;;;--------------------------------------------------------------------------------
;;; Benutzung

(define my-grid (make-grid 4 5))

(my-grid 'set 3 4 9)

(my-grid 'get 3 4)

(my-grid 'get 0 0)

(define X 'X)

(define (konvertiere liste)
  (if (null? liste)
    '()
    (let((c (car liste)))
      (cons
        (cond ((eq? c X) X)
              ((= c 0) '(1 2 3 4 5 6 7 8 9))
              (else (list c))
        )
        (konvertiere (cdr liste))
      )
    )
  )
)

(define (str8ts . grid)
  (print (list->vector (list-split (konvertiere grid) 9)))
)

|#