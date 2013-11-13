(define kombinationen
    (lambda (max)
        (let this ((queue '((0 0))) (states '()))
            ; queue: -> (0 1) wäre 0 Missionare, ein Kannibale
            (if (null? queue) ; noch Elemente in der Queue?
                (delete-duplicates ; doppelte Elemente sind ja toof
                    (delete (list 0 0) states)) ; States zurückgeben, mit (0, 0) gelöscht, da ja
                                           ; immer jemand im Boot sitzen muss.

                (let ((top (car queue))) ; Vermerk auf das erste Element
                    (if (> (apply + top) max) ; Mehr Kannibale + Missionare als das Maximum, was aufs Boot geht
                        (this (cdr queue) states) ; dieses Element überspringen, mit dem Rest der Liste weitermachen
                        (this
                            ; zwei neue Elemente an den Rest der Queue einfügen, einmal mit einem
                            ; Missionar mehr, das andere mal mit einem Kannibalen mehr
                            (cons
                                (list (+ (car top) 1) (cadr top))
                                (cons
                                    (list (car top) (+ (cadr top) 1))
                                    (cdr queue)
                                )
                            )
                            ; top an states anhängen
                            (cons top states)
                        )
                    )
                )
            )
        )
    )
)

(define überlebend?
    (lambda (top)
        (or
            (= (car top) 0) ; keine Missionare da
            (>= (car top) (cadr top)) ; es sind mindestens genausoviele Missionare wie Kannibalen
        )
    )
)

(print (filter überlebend? (kombinationen 10)))
