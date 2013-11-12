(module praktikum1)

; Aufgabe 8; Aus den 3 übergebenen Zahlen die Summe der Quadrate der größten zwei Zahlen ermitteln
(define (quadrat z)
  (* z z)
)

; Lösung für genau 3 Elemente
(define (a8o a b c)
  (if (= a (max a b c))
    (+ (quadrat a) (quadrat (max b c)))
    (a8o b c a)
  )
)

(print (a8o 3 4 5))
(print (a8o 3 5 4))
(print (a8o 4 3 5))
(print (a8o 4 5 3))
(print (a8o 5 3 4))
(print (a8o 5 4 3))

; Lösung für beliebig viele Elemente
(define (a8 a . r)
  (let
    (
      (m (apply max r))
    )
    (if (>= a m)
      (+ (quadrat a) (quadrat m))
      (apply a8 (append r (list a)))
    )
  )
)

(print (a8 3 4 5))
(print (a8 3 5 4))
(print (a8 4 3 5))
(print (a8 4 5 3))
(print (a8 5 3 4))
(print (a8 5 4 3))

(print (a8 1 2 3 4 5 4 3 -3 9 2 9))
