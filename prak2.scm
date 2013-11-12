; Ausgabe 1
(define my-make-list
	(lambda (anzahl obj)
		(if (= anzahl 0) '()
			(cons obj (my-make-list (- anzahl 1) obj)))))

(print (my-make-list 7 '(x y)))

; Aufgabe 2
(define shorter
	(lambda (x . y)
		(if (null? y) x
			(if (< (length x) (length (car y)))
				(apply shorter x (cdr y))
				(apply shorter y)))))
(print (shorter '(a b c) '(1 2) '(5 6 7)))

; Aufgabe 3
#|

				((eqv? msg 'multipop)
					(let ((anzahl (car args)) (t (self 'top)))
						(print (car args))
						(self 'pop)
						(cons t (self 'multipop (- anzahl 1)))
				))
|#
(define make-stack
	(lambda ()
		(letrec*
			(
				(ls '())
				(pop (lambda () (set! ls (cdr ls))))
				(top (lambda () (car ls)))
			)
			(lambda (msg . args)
				(cond
				((eqv? msg 'empty?) (null? ls))
				((eqv? msg 'push) (set! ls (cons (car args) ls)))
				((eqv? msg 'pop) (pop))
				((eqv? msg 'top) (top))
				(else "oops!"))))))

(let ((s (make-stack)))
	(s 'push 'A)
	(s 'push 'B)
	(s 'push 'C)
	(s 'push 'D)
	(print (s 'top))
	; (print (s 'multipop 3))
)

; Aufgabe 5
(define faktoren
	(lambda (zahl elem liste)
		(if (= zahl elem) liste
			(if (= (modulo zahl elem) 0)
				(faktoren zahl (+ elem 1) (cons elem liste))
				(faktoren zahl (+ elem 1) liste)))))

(print "15" (faktoren 15 2 '()))
(print "16" (faktoren 16 2 '()))



(define prim?
	(lambda (zahl)
		(= (length (faktoren zahl 2 '())) 0)))

(print "2 -> " (prim? 2))
(print "3 -> " (prim? 3))
(print "4 -> " (prim? 4))
(print "5 -> " (prim? 5))
(print "6 -> " (prim? 6))
