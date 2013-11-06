(module sudoku)
; Sudoku und Konsorten mit Backtracking.

(define X 'X)

; Allgemeine Ausgabe
(define (ausgabe grid)
  (letrec*(
      (schwarz (lambda (z) (string-append "\033[37;1;40m" z "\033[0m")))
      (zeichen (lambda (z)
          (cond ((eq? z 0) "_")
                ; Debug:
                ((list? z) z)
                ; ((list? z) ".")
                ((equal? z 'X) (schwarz " "))
                ((< z 0) (schwarz (number->string (- z))))
                (else z))))
      (zeile (lambda (liste)
          (cond ((null? liste) '())
                ((null? (car liste))
                  (display "\n")
                  (zeile (cdr liste)))
                (else
                  (display* (zeichen (caar liste)))
                  (zeile (cons (cdar liste) (cdr liste))))))))
    (zeile grid)
    (display "   ---\n")))

; Allgemeine Grid-Zugriffe
(define (get-xy grid x y)
  (list-ref (list-ref grid (- y 1)) (- x 1)))

(define (set-xy grid x y elem)
  (if (= y 1)
    (letrec*((set-x
      (lambda (grid x elem)
        (if (= x 1)
          (set-car! grid elem)
          (set-x (cdr grid) (- x 1) elem)))))
      (set-x (car grid) x elem))
    (set-xy (cdr grid) x (- y 1) elem)))

; Konvertiert alle 0en in Listen mit (1 .. 9) und alle Zahlen in Listen, die nur die
; Zahl enthalten -- damit wird in gelöste-felder dann die Zahl eingesetzt und
; anliegende Möglichkeiten entfernt.
(define (konvertiere e)
  (if (null? e) '()
    (cons
      (cond
        ; alle 0en in Zahlen konvertieren
        ((eq? (car e) 0) '(1 2 3 4 5 6 7 8 9))

        ; temporäre Umwandlung
        ((number? (car e)) (list (car e)))
        (else (car e)))
      (konvertiere (cdr e)))))

; Allgemeine Suche - gibt #f zurück, falls eine Funktion etwas geändert hat
(define (durchsuche grid funktionen)
  (if (null? funktionen)
    #t
    (letrec*
      (
        (fn (car funktionen))
        (alt (tree-copy grid))
        (z
          (lambda (zeile x y)
            (if (not (null? zeile))
              (begin
                (fn grid (car zeile) x y)
                (z (cdr zeile) (+ x 1) y)
              )
            )
          )
        )
        (durchsuche-zeilen
          (lambda (zeilen x y)
            (if (= y 10)
              '()
              (begin
                (z (car zeilen) x y)
                (durchsuche-zeilen (cdr zeilen) x (+ y 1))
              )
            )
          )
        )
      )
      (durchsuche-zeilen grid 1 1)
      (or
        (equal? alt grid)
        (durchsuche grid (cdr funktionen))
      )
    )
  )
)

; Hilfsfunktion für das durchlaufen einzelner Abschnitte
(define (region grid x1 x2 y1 y2 fn)
  (letrec*
    (
      (region'
        (lambda (x1 x2)
          (let
            (
              (ergebnis (fn (get-xy grid x1 y1)))
            )
            (if (or (number? ergebnis) (list? ergebnis))
              (set-xy grid x1 y1 ergebnis)
            )
          )
          (if (not (= x1 x2))
            (region' (+ x1 1) x2)
          )
        )
      )
    )
    (region' x1 x2)
    (if (not (= y1 y2))
      (region grid x1 x2 (+ y1 1) y2 fn)
    )
  )
)

; Entfernt eine Zahl als Möglichkeit für den Bereich [x1, y1] -> [x2, y2]
(define (entferne-möglichkeit grid elem x1 x2 y1 y2)
  (region grid x1 x2 y1 y2
    (lambda (e)
      (if (list? e) (remq (abs elem) e))
    )
  )
)

; Sucht nach allen gelösten Feldern
(define (gelöste-felder grid elem x y)
  (if (and (list? elem) (= (length elem) 1))
    ; einzelnes Feld, welches nur noch eine Zahl als mögliche Lösung enthält.
    (begin
      (set-xy grid x y (car elem))
      (entferne-möglichkeit grid (car elem) x x 1 9)
      (entferne-möglichkeit grid (car elem) 1 9 y y)
    )
  )
)

(define (schwarz? elem)
  (or (and (number? elem) (< elem 0)) (equal? elem 'X))
)

; Gibt die Grenzen eines Compartments zurück.
(define (grenze grid x y xop yop)
  (cond
    ((< x 1) 1)
    ((> x 9) 9)
    ((< y 1) 1)
    ((> y 9) 9)
    (else
      (if (schwarz? (get-xy grid x y))
        (if (equal? * xop) (yop y -1) (xop x -1))
        (grenze grid (xop x 1) (yop y 1) xop yop)
      )
    )
  )
)

; Sammelt alle in diesem Compartment vorhandene Möglichkeiten
(define (get-intervall grid x1 x2 y1 y2)
  (let
    (
      (l (list X))
    )
    (region grid x1 x2 y1 y2
      (lambda (e)
        (print (list? e))
        (append! l (list-copy (list e)))
      )
    )
    (print "->" l)
    (remq! X l)
  )
)

#|
(define (möglichkeiten l)
  (print "->" l)
  (if (null? l)
    '()
    (let
      (
        (c (car l))
      )
      (print "X" c ", " (cdr l))
      (delete-duplicates
        (cond ((number? c) (cons c (möglichkeiten (cdr l))))
              ((list? c) (append (möglichkeiten (cdr l)) c))
        )
      )
    )
  )
)
|#

(define (min' l)
  (let((L (filter number? l)))
    (if (null? L)
      1
      (apply min L)
    )
  )
)

(define (max' l)
  (let((L (filter number? l)))
    (if (null? L)
      9
      (apply max L)
    )
  )
)

; Compartment-Prüfung, d.h. hier wird geprüft, ob innerhalb einer Zeile bzw.
; Spalte auch genügend Platz ist, um eine Straße mit den Möglichkeiten zu bauen.
; Dabei wird immer das Intervall vom Rand/vorherigen 'X bis zum rechten Rand/
; nächsten X betrachtet.
;;; In ein Intervall der Länge 4, in dem es für jedes Feld die Möglichkeiten
;;; (1 2 3 4 5 7 8 9) gibt, kann nie eine Straße aus (7 8 9) entstehen, da die 6 fehlt
; Ist im Intervall der Länge 3 beispielsweise eine 5 vorgegeben, können keine Zahlen
; < 3 oder > 7 hinein.
(define (compartments grid elem x y)
  (print x ";;" y)
  (if (list? elem)
    (letrec*
      (
        (start (grenze grid x y - *))
        (ende (grenze grid x y + *))
        (länge (- ende start))
        (intervall (get-intervall grid start ende y y))
        (maximum (max' intervall))
        (minimum (min' intervall))
        (f
          (lambda (x)
            (print "-> " x)
            (if (list? x)
              (filter! (lambda (w)
                (or (< w (- minimum länge)) (> w (+ maximum länge)))
              ) x)
            )
          )
        )
      )
      (print minimum)
      (print maximum)
      (print start " -> " ende)
      (print intervall)
      (for-each f intervall)
      (ausgabe grid)
      (exit)
    )
  )
)

; Rät ein freies Feld... und löst das Grid weiter, falls dies erfolgreich ist.
#|(define (raten grid elem x y)
  (if (and (list? elem) (not (null? elem)))
    (let
      (
        (kopie (tree-copy grid))
      )
      (set-xy kopie x y (car elem))
      (if (lösen kopie)
        ; Chacka!
        (durchsuche kopie
          (list
            (lambda (_ elem x y)
              (set-xy grid x y elem)
            )
          )
        )
        (raten grid (cdr elem) x y)
      )
    )
  )
)|#

; Prüft, ob für alle Felder eine Zahl eingetragen wurde.
(define (gelöst? grid)
  (let
    (
      (gelöst #t)
    )
    (durchsuche grid
      (list
        (lambda (grid e x y)
          (if (list? e)
            (set! gelöst #f)
          )
        )
      )
    )
    gelöst
  )
)

; Lösen!
(define (lösen grid)
  (ausgabe grid)
  (if (not (durchsuche grid (list gelöste-felder compartments)))
    (lösen grid)
    (gelöst? grid)
  )
)

; Str8ts
(define (str8ts . liste)
  (let
    (
      (grid (list-split (konvertiere liste) 9))
    )
    (ausgabe grid)
    (print (if (lösen grid) "Gelöst!" "Nicht gelöst :("))
    (ausgabe grid)
  )
)

#|
(str8ts
   X  0  6  0 -4  X  0  0  X
   0  0  X  0  0  X  0  5  X
   0  0  X  0  0  0 -1  0  0
  -5  0  0  0  X  7  0  0  8
   X  4  5  0  X  0  8  0 -2
   0  1  0  0  X  9  0  0  X
   0  0 -9  0  0  0  X  0  0
   X  0  8  X  0  0  X  4  0
  -6  0  0  X  X  0  0  0  X
  ; X 5 6 7 4 X 3 2 X
  ; 7 6 X 1 2 X 4 5 X
  ; 8 9 X 5 3 4 1 6 7
  ; 5 3 4 2 X 7 6 9 8
  ; X 4 5 3 X 6 8 7 2
  ; 2 1 3 4 X 9 7 8 X
  ; 1 2 9 6 7 8 X 3 4
  ; X 7 8 X 6 5 X 4 3
  ; 6 8 7 X X 3 2 1 X
)
|#

(str8ts
  X 0 3 0 X 0 0 7 X
  -1 0 0 0 X 0 7 0 9
  0 0 -2 0 0 X X 0 0
  2 0 X 0 6 0 0 0 7
  X X 5 0 X 3 0 X -6
  0 0 0 0 0 0 X 0 0
  0 0 X -9 4 0 X 0 3
  0 0 8 7 X 0 0 0 X
  X 9 0 0 -3 0 5 4 X
)