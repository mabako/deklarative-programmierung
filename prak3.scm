(module prak3)

; remq auf alle Elemente
(define remove-all
  (lambda (list objs)
    (if (null? objs)
      list
      (remove-all (remq (car objs) list) (cdr objs)))))

; berechnet die Werte, die ausgeschlossen werden können, weil sie auf einer Diagonale liegen
(define calc-diagonal
  (lambda (state)
    (if (null? state)
      '()
      (let ((distance (length state)) (c (car state)))
        (append (calc-diagonal (cdr state)) (list (- c distance) (+ c distance)))))))

; lazy evaluation mit delay/force, damit first-solution nicht den gesamten Zweig b durchlaufen muss,
; wenn a eine Lösung ist.
(define first-solution (lambda (a b) (if (null? a) (force b) a)))
(define all-solutions (lambda (a b) (append a (force b))))

(define n-damen
  (lambda (n eval-fn)
    (letrec*(
      (alle-moeglichkeiten (iota n 1))
      (func (lambda (state)
        (if (= (length state) n)
          (if (equal? eval-fn all-solutions)
            (list state) ; wir wollen alle Lösungen, also als hier für append eine Liste vorher basteln
            state) ; nur eine Lösung relevant... also nicht extra in eine Liste packen

          ; für alle Möglichkeiten versuchen, die entsprechenden Kombinationen ab da zu versuchen.
          (let try'
            ((moeglichkeiten
              (remove-all
                (remove-all alle-moeglichkeiten state) ; alle Spalten, in denen bereits eine Dame steht
                (calc-diagonal state)))) ; alle Spalten, die diagonal zu einer Dame stehen
            (if (null? moeglichkeiten)
              '() ; keine Möglichkeiten mehr verbleibend
              (eval-fn
                (func (append state (list (car moeglichkeiten)))) ; die 1. Möglichkeit anhängen
                (delay (try' (cdr moeglichkeiten)))))))))) ; mit allen verbleibenden Möglichkeiten probieren
      (func '()))))

;(print (n-damen 5 all-solutions))
;(print (n-damen 12 first-solution))

; Ausgabe n-Damen-Problem
(define print-chessboard
  (lambda (board)
    (letrec*(
      (length' (length board))
      (print-row (lambda (num pos)
        (cond ((equal? num -) (display "+---"))
              ((= num pos)    (display "| * "))
              (else           (display "|   ")))
        (if (< pos length')
          (print-row num (+ pos 1))
          (print (if (equal? num -) "+" "|")))))
      (func (lambda (board)
        (if (null? board) (print-row - 1)
          (begin
            (print-row - 1)
            (print-row (car board) 1)
            (func (cdr board)))))))
      (func board))))
;(print-chessboard '(2 4 1 3))
;(print-chessboard (n-damen 8 first-solution))

(define boot-ist-links? (lambda (index) (= (modulo index 2) 0)))
(define missionare
  (lambda ()
    ; < = Boot ist auf der linken Seite, > = Boot ist auf der rechten Seite
    (letrec*(
      (ziehen
        (lambda (state zug)
          (letrec* ((links (car state))
                (rechts (cadr state))
                (history (caddr state)))
            (if (boot-ist-links? (length history))
              (list (map - links zug) (map + rechts zug) (append history (list zug)))
              (list (map + links zug) (map - rechts zug) (append history (list zug)))))))
      ; Lösung, falls auf der linken Uferseite keine Leute mehr stehen
      (solution? (lambda (state) (and (= (caar state) 0) (= (cadar state) 0))))

      (valid? (lambda (state)
        ; keine Missionare auf der Seite oder >= Missionare als Kannibalen
        (and (or (= (caar state) 0) (>= (caar state) (cadar state)))
             (or (= (caadr state) 0) (>= (caadr state) (cadadr state)))
             (>= (caar state ) 0)
             (>= (cadar state ) 0)
             (>= (caadr state ) 0)
             (>= (cadadr state ) 0)
        )))

      ; Breitensuche
      (func (lambda (queue)
        (let zug(
          (q (cdr queue)) ; Restliche Queue -> aktueller Zustand wird bearbeitet
          ; alle möglichen Züge im Format (Missionare Kannibalen)
          (zuege (list (list 1 0) (list 2 0) (list 0 1) (list 0 2) (list 1 1))))

          (if (null? zuege) (func q) ; keine Züge mehr, mit dem Rest der Queue weitermachen
            (let ((tmp (ziehen (car queue) (car zuege))))
              (if (solution? tmp) (cons 'Loesung (cddr tmp)) ; Lösung -> in diesem Fall wird der Pfad zurückgegeben
                (if (valid? tmp)
                  (zug (append q (list tmp)) (cdr zuege))
                  (zug q (cdr zuege))))))))))
      (func (list
        (list
          (list 3 3) ; 3 Missionare, 3 Kannibalen
          (list 0 0)
          '()))))))

(define print-m
  (lambda (loesung)
    (if (or (null? loesung) (not (equal? (car loesung) 'Loesung)))
      (error 'loesung "Ist keine Lösung?" loesung)
      (let self ((index 0) (l (cadr loesung)))
        (if (null? l) (unspecified)
          (begin
            (print
              (if (boot-ist-links? index) "->>" "<<-") " "
              (caar l) " Missionare und "
              (cadar l) " Kannibalen")
            (self (+ index 1) (cdr l))))))))
(print-m (missionare))