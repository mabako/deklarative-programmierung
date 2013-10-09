; Allgemein hilfreiche Funktionen für die Cubes
(module rubik)

; Original-Würfel
(define-struct wuerfel
  (left  '(W W W W W W W W W))
  (top   '(B B B B B B B B B))
  (back  '(O O O O O O O O O))
  (down  '(G G G G G G G G G))
  (front '(R R R R R R R R R))
  (right '(Y Y Y Y Y Y Y Y Y))
)

; Kopieren
(define (copy w)
  (let
    (
      (x (make-wuerfel))
    )
    (wuerfel-left-set! x (wuerfel-left w))
    (wuerfel-top-set! x (wuerfel-top w))
    (wuerfel-back-set! x (wuerfel-back w))
    (wuerfel-down-set! x (wuerfel-down w))
    (wuerfel-front-set! x (wuerfel-front w))
    (wuerfel-right-set! x (wuerfel-right w))
    x
  )
)

; Eingeschränktes Zugset, Notation von
;   http://de.wikibooks.org/wiki/Zauberw%C3%BCrfel/_3x3x3/_Notation
; Nur äußere Layer, keine inneren/doppelten Layer.

; Züge - 90°
(define L :L)
(define R :R)
(define F :F)
(define B :B)
(define U :U)
(define D :D)

; Erweiterte Züge - 180°. Offiziell zählen diese als 1 Zug.
(define L2 :L2)
(define R2 :R2)
(define F2 :F2)
(define B2 :B2)
(define U2 :U2)
(define D2 :D2)

; Erweiterte Züge - -90°.
(define L- :L-)
(define R- :R-)
(define F- :F-)
(define B- :B-)
(define U- :U-)
(define D- :D-)

(define symbole (list F F2 F- B B2 B- U U2 U- D D2 D- L L2 L- R R2 R-))



(define gruppe
  ; Vereinfachung der Drehung: Dieselbe Option darf auch in anderen Varianten nicht
  ; mehrfach hintereinander ausgeführt werden, da wir sonst Duplikate enthalten
  (list
    (list F F2 F-)
    (list B B2 B-)
    (list L L2 L-)
    (list R R2 R-)
    (list U U2 U-)
    (list D D2 D-)
  )
)

; ---

(define get-n list-ref)

(define (set-n liste n wert)
  ; Setzt das (n)-te Element der (liste) auf (wert).
  (if (= n 0)
    (cons wert (cdr liste))
    (cons (car liste) (set-n (cdr liste) (- n 1) wert))
  )
)

(define (ausgabe w)
  ;Ausgabe des Würfels in Farbe als 'Draufsicht'.
  ;  erste Zeile - oben (eingerückt, oberhalb von 'vorn')
  ;  zweite Zeile - links, vorn, rechts, hinten
  ;  dritte Zeile - unten (eingerückt)
  (letrec
    (
      (blocks
        ; Definiert die Seiten des Würfels zum ausgeben
        (list
          ; Insbesondere sind Oben und Unten eingerückt.
          (list '(0 0 0 0 0 0 0 0 0) (wuerfel-top w))
          (list (wuerfel-left w) (wuerfel-front w) (wuerfel-right w) (wuerfel-back w))
          (list '(0 0 0 0 0 0 0 0 0) (wuerfel-down w))
        )
      )

      (hintergrund
        (lambda (farbe)
          ; Hilfsfunktion, damit der Hintergrund auch weiß bleibt, wenn 0 (leer) kommt.
          (if (= farbe 49)
            37
            (- farbe 10)
          )
        )
      )
      (farbig
        (lambda (farbe)
          ; Gibt einen String zurück, der ein 'X' mit Steuerzeichen für die Linux-Konsole enthält.
          (string-append
            "\033["
            (number->string farbe)
            ";1;"
            (number->string (hintergrund farbe))
            "mX\033[0m"
          )
        )
      )
      (zeichen
        (lambda (z)
          ; Gibt ein einzelnes Zeichen farbig aus. Konsolenfarben sollten idealerweise passen.
          (display*
            (farbig
              (case z
                ((0) 49)
                ((W) 47) ; Weiß
                ((B) 44) ; Blau
                ((O) 45) ; nicht farbecht - es gibt kein Orange, also ist das hier violett.
                ((G) 42) ; Grün
                ((R) 41) ; Rot
                ((Y) 43) ; Gelb, sieht auch leicht bräunlich aus.
              )
            )
          )
        )
      )
      (zeile
        (lambda (z num)
          ; Gibt drei Zeichen der aktuellen Liste aus. (num) ist dabei der Index des ersten Zeichens
          ; Damit wird der Würfel zeilenweise ausgegeben.
          (if (null? z) (display "\n")
            (begin
              (let ((x (car z)))
                (zeichen (get-n x num))
                (zeichen (get-n x (+ num 1)))
                (zeichen (get-n x (+ num 2)))
                (display* " ")
              )
              (zeile (cdr z) num)
            )
          )
        )
      )
      (ausgeben
        (lambda (w)
          "Gibt die Blocks zeilenweise aus."
          (if (null? w) '()
            (begin
              ; 3 Zeilen für die aktuelle Darstellung, danach eine Leerzeile.
              (zeile (car w) 0)
              (zeile (car w) 3)
              (zeile (car w) 6)
              (display "\n")

              ; für die restlichen Blockgruppen wiederholen.
              (ausgeben (cdr w))
            )
          )
        )
      )
    )
    (ausgeben blocks)
  )
)

(define (_w seite nachher . rest)
  ; Baut eine Funktion für Zugriff auf den Würfel (struct) auf und ruft diese mit Parametern auf.
  (apply
    (eval
      (string->symbol (string-append "wuerfel-" (symbol->string seite) nachher))
    )
    rest
  )
)

(define (get-w w seite)
  (_w seite "" w)
)

(define (set-w w seite wert)
  (_w seite "-set!" w wert)
)

(define (rotiere+ w seite)
  ; Dreht eine Fläche des Würfels im Uhrzeigersinn.
  ; Würfelseiten sind Listen:
  ;   0 1 2
  ;   3 4 5 ; 4 bleibt immer gleich, da es keine Operationen für Kernstücke gibt.
  ;   6 7 8
  ; neue Würfelseite - im Uhrzeigersinn gedreht:
  ;   6 3 0
  ;   7 4 1
  ;   8 5 2
  (let
    (
      (liste (get-w w seite))
    )

    ; Direkt setzen
    (set-w w seite
      (list
        (get-n liste 6)
        (get-n liste 3)
        (get-n liste 0)
        (get-n liste 7)
        (get-n liste 4)
        (get-n liste 1)
        (get-n liste 8)
        (get-n liste 5)
        (get-n liste 2)
      )
    )
  )
)

(define (rotiere- w seite)
  ; Neu:
  ;   2 5 8
  ;   1 4 7
  ;   0 3 6

  ; das hier ist eigtl.
  ; (rotiere* w seite)
  ; (rotiere+ w seite)
  (let
    (
      (liste (get-w w seite))
    )

    ; Direkt setzen
    (set-w w seite
      (list
        (get-n liste 2)
        (get-n liste 5)
        (get-n liste 8)
        (get-n liste 1)
        (get-n liste 4)
        (get-n liste 7)
        (get-n liste 0)
        (get-n liste 3)
        (get-n liste 6)
      )
    )
  )
)

(define (rotiere-helper A teile-A B teile-B)
  ; Dreht alle Elemente an Position (a1, a2, a3) der Liste A auf neue Elemente
  ; an Position (b1, b2, b3) in Liste B. Der Rest der Liste B bleibt erhalten.
  (set-n
    (set-n
      (set-n
        B
        (car teile-B)
        (get-n A (car teile-A))
      )
      (cadr teile-B)
      (get-n A (cadr teile-A))
    )
    (caddr teile-B)
    (get-n A (caddr teile-A))
  )
)

(define (rotiere-vier w seite-a teile-a seite-b teile-b seite-c teile-c seite-d teile-d)
  ; Rotiert die vier gegebenen Listen im Uhrzeigersinn.
  (let
    (
      ; Convenience: spart das zweimalige Ausführen/Schreiben
      (a (get-w w seite-a))
      (b (get-w w seite-b))
      (c (get-w w seite-c))
      (d (get-w w seite-d))
    )

    ; Für jede anliegende Seite ausführen
    (set-w w seite-b (rotiere-helper a teile-a b teile-b))
    (set-w w seite-c (rotiere-helper b teile-b c teile-c))
    (set-w w seite-d (rotiere-helper c teile-c d teile-d))
    (set-w w seite-a (rotiere-helper d teile-d a teile-a))
  )
)

(define (rotiere* w seite)
  ; Hinterher:
  ;   8 7 6
  ;   5 4 3
  ;   2 1 0
  (set-w w seite (reverse (get-w w seite)))
)

(define (rotiere-zwei w . e)
  (letrec*
    (
      (seite-a (car e))
      (seite-b (caddr e))
      (a (get-w w seite-a))
      (b (get-w w seite-b))
      (teile-a (cadr e))
      (teile-b (cadddr e))
    )
    (set-w w seite-b (rotiere-helper a teile-a b teile-b))
    (set-w w seite-a (rotiere-helper b teile-b a teile-a))
  )
)

(define (zugset seite)
  ; Das Zugset ist für normal drehen definiert. Anpassungen für 180° bzw. -90° sind
  ; notwendig.
  (case seite
    ;               0    1        2      3        4     5        6     7
    ((front) (list 'top '(6 7 8) 'right '(0 3 6) 'down '(2 1 0) 'left '(8 5 2)))
    ((back)  (list 'top '(0 1 2) 'left '(6 3 0) 'down '(8 7 6) 'right '(2 5 8)))
    ((left)  (list 'top '(0 3 6) 'front '(0 3 6) 'down '(0 3 6) 'back '(8 5 2)))
    ((right) (list 'top '(2 5 8) 'back '(6 3 0) 'down '(2 5 8) 'front '(2 5 8)))
    ((top)   (list 'left '(0 1 2) 'back '(0 1 2) 'right '(0 1 2) 'front '(0 1 2)))
    ((down)  (list 'left '(6 7 8) 'front '(6 7 8) 'right '(6 7 8) 'back '(6 7 8)))
  )
)

(define (zug! w aktion)
  ; Einen einzelnen Zug durchführen.
  ; (print aktion)
  (letrec
    (
      (kurz
        (lambda (seite t)
          (let
            (
              (set (zugset seite))
            )
            (case t
              ((0)
                (rotiere+ w seite)
                (apply rotiere-vier (cons w set))
              )
              ((1)
                (rotiere* w seite)
                (apply rotiere-zwei
                  (list w (get-n set 0) (get-n set 1)
                          (get-n set 4) (get-n set 5)
                  )
                )
                (apply rotiere-zwei
                  (list w (get-n set 6) (get-n set 7)
                          (get-n set 2) (get-n set 3)
                  )
                )
              )
              ((2)
                (rotiere- w seite)
                (apply rotiere-vier
                  (list w (get-n set 0) (get-n set 1)
                          (get-n set 6) (get-n set 7)
                          (get-n set 4) (get-n set 5)
                          (get-n set 2) (get-n set 3)
                  )
                )
              )
            )
          )
        )
      )
      (einzeln
        (lambda (a)
          ; Simple Unterscheidung je nach Zugart bestimmte Seiten drehen. Referenz für die Zahlen
          ; ist die Anmerkung in 'rotiere'.
          (case a
            ((:F) (kurz 'front 0))
            ((:F2) (kurz 'front 1))
            ((:F-) (kurz 'front 2))

            ((:B) (kurz 'back 0))
            ((:B2) (kurz 'back 1))
            ((:B-) (kurz 'back 2))

            ((:L) (kurz 'left 0))
            ((:L2) (kurz 'left 1))
            ((:L-) (kurz 'left 2))

            ((:R) (kurz 'right 0))
            ((:R2) (kurz 'right 1))
            ((:R-) (kurz 'right 2))

            ((:U) (kurz 'top 0))
            ((:U2) (kurz 'top 1))
            ((:U-) (kurz 'top 2))

            ((:D) (kurz 'down 0))
            ((:D2) (kurz 'down 1))
            ((:D-) (kurz 'down 2))

            (else (error "einzelner Zug" "zug!/einzeln" a))
          )
          ; (ausgabe w)
        )
      )
      (aufruf
        (lambda (a)
          ; Solange ziehen, bis nichtsmehr zum Ziehen existiert.
          (if (null? a) '()
            (einzeln a)
          )
        )
      )
    )
    
    ; alle Aktionen durchkauen
    (aufruf aktion)
  )
  w
)

(define (zug w folge)
  ; Alle Züge der Restfolge ausführen.
  (if (null? folge)
    w ; Cube, bei dem alle Züge ausgeführt wurden
    (begin
      (zug! w (car folge))
      (zug w (cdr folge))
    )
  )
)

(define (folge-ausgeben folge)
  ; Gibt die Befehle in auszuführender Reihenfolge aus.
  (letrec*
    (
      (anhaengen
        ; Verkettung der einzelnen Listenbefehle.
        (lambda (f)
          (if (null? f)
            ""
            (string-append (keyword->string (car f)) (anhaengen (cdr f)))
          )
        )
      )
    )
    (print (string-append "Reihenfolge " (anhaengen folge)))
  )
)

; Erstellt einen unbenutzten Würfel und dreht ihn in der gegebenen Reihenfolge.
(define (wuerfel! . folge)
  (folge-ausgeben folge)
  (zug (make-wuerfel) folge)
)

; Gibt eine zufällige Reihenfolge an Zugsymbolen zurück, die auch mehrfach auftauchen können.
(define (mischen anzahl)
  (if (= anzahl 0)
    '()
    (cons (get-n symbole (random (length symbole))) (mischen (- anzahl 1)))
  )
)

(define (mischen! anzahl)
  (apply wuerfel! (mischen anzahl))
)


; Superflip!
; (ausgabe (wuerfel! R L U2 F U- D F2 R2 B2 L U2 F- B- U R2 D F2 U R2 U))

; cube20.org, nur nach 20 Zügen lösbarer Würfel
; (ausgabe (wuerfel! F U- F2 D- B U R- F- L D- R- U- L U B- D2 R- F U2 D2))















; ---
; Lösungsvorschläge

; Cache mit Hash
(define (hash w)
  ; Erzeugt einen Hashwert für die Funktion
  (letrec*
    (
      (char
        (lambda (c)
          (if (null? c)
            0
            (+
              (case (car c)
                ((W) 0)
                ((Y) 1)
                ((R) 2)
                ((G) 3)
                ((B) 4)
                ((O) 5)
              )
              (* 6 (char (cdr c)))
            )
          )
        )
      )
      (h
        (lambda (seiten)
          (if (null? seiten)
            0
            (+ (char (get-w w (car seiten))) (* 54 (h (cdr seiten))))
          )
        )
      )
    )
    (h '(front back left right top down))
  )
)

(define *cache
  (letrec
    (
      (*h (make-hashtable))
      (tiefe 3)
      (aufruf
        (lambda (w t)
          ; Der genaue Inhalt der Tabelle ist wirklich irrelevant.
          ; Kann bei Bedarf jederzeit errechnet werden - einfach solange die Operationen durchlaufen,
          ; bis eine mit t-1 gefunden ist.
          (hashtable-put! *h (hash w) t)

          (if (> t 0)
            (letrec*
              (
                (n (- t 1))
                (alle-symbole
                  (lambda (s)
                    (if (null? s)
                      '()
                      (begin
                        (aufruf (zug! (copy w) (car s)) n)
                        (alle-symbole (cdr s))
                      )
                    )
                  )
                )
              )
              (alle-symbole symbole)
            )
            '()
          )
        )
      )
    )
    (aufruf (make-wuerfel) tiefe)
    *h
  )
)

(print (hashtable-size *cache))

(define (gelöst? w)
  (hashtable-contains? *cache (hash w))
)

; Lösungsalgorithmen

; Trivialer Lösungsalgorithmus, der innerhalb einer maximalen Tiefe nach möglichen Lösungen sucht.
(define (tiefensuche w max bisher außer)
  (cond ((< max 0) '())
        ((gelöst? w) (reverse bisher))
        (else 
          (letrec
            (
              (s symbole)
              (ohne
                (lambda (l x)
                  (if (memq x (car l))
                    (car l)
                    (ohne (cdr l) x)
                  )
                )
              )
              (rekursiv
                (lambda (w s)
                  (if (or (memq s außer) (null? s))
                    '()
                    (letrec*
                      (
                        (ret (tiefensuche (zug! (copy w) (car s)) (- max 1) (cons (car s) bisher) (ohne gruppe (car s))))
                      )
                      (if (null? ret)
                        (rekursiv w (cdr s))
                        ret
                      )
                    )
                  )
                )
              )
            )
            (rekursiv w s)
          )
        )
  )
)

(print (gelöst? (make-wuerfel)))

(let
  (
    (t (wuerfel! D2 L R U- R D U))
  )
  (ausgabe t)
  (print (gelöst? t))
  (let
    (
      (ts (tiefensuche t 4 '() '()))
    )
    (if (list? ts)
      (folge-ausgeben ts)
      (print ts)
    )
  )
)
