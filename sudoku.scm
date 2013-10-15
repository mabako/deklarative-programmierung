(module sudoku)
; Sudoku und Konsorten mit Backtracking.

(define X 'X)

; Allgemeine Ausgabe
(define (ausgabe grid)
  (letrec*
    (
      (schwarz
        (lambda (z)
          (string-append
            "\033[37;1;40m"
            z
            "\033[0m"
          )
        )
      )
      (zeichen
        (lambda (z)
          (cond ((equal? z 'X) (schwarz " "))
                ((= z 0) ".")
                ((< z 0) (schwarz (number->string (- z))))
                (else z)
          )
        )
      )
      (zeile
        (lambda (z n)
          (if (null? z)
            (display* "\n")
            (begin
              (if (and (> n 0) (= (modulo n 9) 0)) (display* "\n"))
              (display* (zeichen (car z)))
              (zeile (cdr z) (+ n 1))
            )
          )
        )
      )
    )
    (zeile grid 0)
    (display "   ---\n")
  )
)

; Allgemeine Grid-Zugriffe
(define (get-xy grid x y)
  (list-ref grid (+ (* (- y 1) 9) (- x 1)))
)

(define (sel-zeile grid x0 x1 y)
  (if (> x0 x1)
    '()
    (cons (get-xy grid x0 y) (sel-zeile grid (+ x0 1) x1 y))
  )
)

(define (sel-spalte grid x y0 y1)
  (if (> y0 y1)
    '()
    (cons (get-xy grid x y0) (sel-zeile grid x (+ y0 1) y1))
  )
)

(define (unique? liste)
  (cond ((null? liste) #t)
        ((or (symbol? (car liste)) (<= (car liste) 0)) (unique? (cdr liste)))
        ((memq (car liste) (cdr liste)) #f)
        (else (unique? (cdr liste)))
  )
)

; Constraints
(define (c:zeile n)
  (lambda (grid)
    (unique? (sel-zeile grid 1 9 n))
  )
)

(define (c:spalte n)
  (lambda (grid)
    (unique? (sel-spalte grid n 1 9))
  )
)

(define (c:build fn n)
  (if (null? n)
    '()
    (cons (fn (car n)) (c:build fn (cdr n)))
  )
)

(define (gültig? vorher nachher constraints)
  (print vorher)
  (letrec*
    (
      (grid (reverse (append vorher nachher)))
      (fn
        (lambda (c)
          (if (null? c)
            #t
            (and ((car c) grid) (fn (cdr c)))
          )
        )
      )
    )
    (fn constraints)
  )
)

; Allgemeine Lösung
(define (lösen grid constraints)
  (letrec*
    (
      (werte '(1 2 3 4 5 6 7 8 9))
      (löse
        (lambda (vorher nachher w)
          (cond ((not (gültig? vorher nachher constraints)) '())
                ((null? w) '())
                ((null? nachher) vorher)
                ((equal? (car nachher) 0)
                  (let
                    (
                      (lösung (löse (cons (car w) vorher) (cdr nachher) werte))
                    )
                    (if (null? lösung)
                      (löse vorher nachher (cdr w))
                      lösung
                    )
                  )
                )
                (else (löse (cons (car nachher) vorher) (cdr nachher) werte))
          )
        )
      )
    )
    (reverse (löse '() grid werte))
  )
)

; Sudoku, irgendwann mal.

(define (sudoku grid)
  (ausgabe grid)

  (ausgabe
    (lösen grid
      (append
        (c:build c:zeile '(1 2 3 4 5 6 7 8 9))
        (c:build c:spalte '(1 2 3 4 5 6 7 8 9))
      )
    )
  )
)

(sudoku '(
  0 0 0 0 0 0 0 1 0
  4 0 0 0 0 0 0 0 0
  0 2 0 0 0 0 0 0 0
  0 0 0 0 5 0 4 0 7
  0 0 8 0 0 0 3 0 0
  0 0 1 0 9 0 0 0 0
  3 0 0 4 0 0 2 0 0
  0 5 0 1 0 0 0 0 0
  0 0 0 8 0 6 0 0 0
))
; Str8ts-Contraints.
(define (str8ts grid)
  (ausgabe grid)

  ; Constraints bestimmen

  ; Lösen
  (ausgabe (lösen grid '()))
)

; Zur Abwechslung mal ein Str8ts lösen
#|
(str8ts '(
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
))
|#
