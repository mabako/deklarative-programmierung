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
(define L2 (list L L))
(define R2 (list R R))
(define F2 (list F F))
(define B2 (list B B))
(define U2 (list U U))
(define D2 (list D D))

; Erweiterte Züge - -90°. Der Einfachheit halber als 3 mal ausführen realisiert,
; da dies identisch mit -1 mal ist.
(define L- (list L L L))
(define R- (list R R R))
(define F- (list F F F))
(define B- (list B B B))
(define U- (list U U U))
(define D- (list D D D))

(define symbole (list F F2 F- B B2 B- U U2 U- D D2 D- L L2 L- R R2 R-))

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

(define (func seite nachher . rest)
  ; Baut eine Funktion für Zugriff auf den Würfel (struct) auf und ruft diese mit Parametern auf.
  (apply
    (eval
      (string->symbol (string-append "wuerfel-" (symbol->string seite) nachher))
    )
    rest
  )
)

(define (rotiere w seite)
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
      (liste (func seite "" w))
    )

    ; Direkt setzen
    (func seite "-set!" w
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

(define (rotiere-vier w seite-a teile-a seite-b teile-b seite-c teile-c seite-d teile-d)
  ; Rotiert die vier gegebenen Listen im Uhrzeigersinn.
  (letrec*
    (
      (get
        (lambda (seite)
          ; Gibt den Inhalt einer Würfelseite zurück.
          (func seite "" w)
        )
      )
      (set
        (lambda (seite wert)
          ; Setzt den Inhalt einer Würfelseite.
          (func seite "-set!" w wert)
        )
      )
      (rot
        (lambda (A teile-A B teile-B)
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
      )

      ; Convenience: spart das zweimalige Ausführen/Schreiben
      (a (get seite-a))
      (b (get seite-b))
      (c (get seite-c))
      (d (get seite-d))
    )

    ; Für jede anliegende Seite ausführen
    (set seite-b (rot a teile-a b teile-b))
    (set seite-c (rot b teile-b c teile-c))
    (set seite-d (rot c teile-c d teile-d))
    (set seite-a (rot d teile-d a teile-a))
  )
)

(define (zug! w aktion)
  ; Einen einzelnen Zug durchführen.
  ;(print aktion)
  (letrec
    (
      (einzeln
        (lambda (a)
          ; Simple Unterscheidung je nach Zugart bestimmte Seiten drehen. Referenz für die Zahlen
          ; ist die Anmerkung in 'rotiere'.
          (case a
            ((:F)
              (rotiere w 'front)
              (rotiere-vier w 'top '(6 7 8) 'right '(0 3 6) 'down '(2 1 0) 'left '(8 5 2))
            )
            ((:B)
              (rotiere w 'back)
              (rotiere-vier w 'top '(0 1 2) 'left '(6 3 0) 'down '(8 7 6) 'right '(2 5 8))
            )
            ((:L)
              (rotiere w 'left)
              (rotiere-vier w 'top '(0 3 6) 'front '(0 3 6) 'down '(0 3 6) 'back '(8 5 2))
            )
            ((:R)
              (rotiere w 'right)
              (rotiere-vier w 'top '(2 5 8) 'back '(6 3 0) 'down '(2 5 8) 'front '(2 5 8))
            )
            ((:U)
              (rotiere w 'top)
              (rotiere-vier w 'left '(0 1 2) 'back '(0 1 2) 'right '(0 1 2) 'front '(0 1 2))
            )
            ((:D)
              (rotiere w 'down)
              (rotiere-vier w 'left '(6 7 8) 'front '(6 7 8) 'right '(6 7 8) 'back '(6 7 8))
            )
            (else (error "einzelner Zug" "zug!/einzeln" a))
          )
          ; (ausgabe w)
        )
      )
      (aufruf
        (lambda (a)
          ; Solange ziehen, bis nichtsmehr zum Ziehen existiert.
          (if (list? a)
            (if (null? a) '()
              (begin
                (einzeln (car a))
                (aufruf (cdr a))
              )
            )

            ; ist keine Liste, das Symbol(?) weitergeben.
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
            (let
              (
                (c (car f))
              )
              (string-append
                ; eventuell eine Liste bei mehrfach ausgeführten Befehlen
                (if (list? c)
                  (let
                    (
                      (d (keyword->string (car c)))
                    )
                    (case (length c)
                      ((2) (string-append d "2")) ; Länge2 ist immer 2x ausführen, d.h. 180°
                      ((3) (string-append d "-")) ; Länge3 ist immer 3x ausführen, d.h. -90°
                    )
                  )
                  (keyword->string c)
                )
                (anhaengen (cdr f))
              )
            )
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
            (+ (char (func (car seiten) "" w)) (* 54 (h (cdr seiten))))
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

(define (gelöst? w)
  (hashtable-contains? *cache (hash w))
)

; Lösungsalgorithmen

; Trivialer Lösungsalgorithmus, der innerhalb einer maximalen Tiefe nach möglichen Lösungen sucht.
(define (tiefensuche w max bisher)
  (cond ((< max 0) '())
        ((gelöst? w) (reverse bisher))
        (else 
          (letrec
            (
              (s symbole)
              (rekursiv
                (lambda (w s)
                  (if (null? s)
                    '()
                    (letrec*
                      (
                        (ret (tiefensuche (zug! (copy w) (car s)) (- max 1) (cons (car s) bisher)))
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

; Superflip!
;(ausgabe (wuerfel! R L U2 F U- D F2 R2 B2 L U2 F- B- U R2 D F2 U R2 U))

; cube20.org, nur nach 20 Zügen lösbarer Würfel
;(ausgabe (wuerfel! F U- F2 D- B U R- F- L D- R- U- L U B- D2 R- F U2 D2))

(print (gelöst? (make-wuerfel)))

(let
  (
    (t (wuerfel! D2 L R U- R))
  )
  (ausgabe t)
  (print (gelöst? t))
  (let
    (
      (ts (tiefensuche t 2 '()))
    )
    (if (list? ts)
      (folge-ausgeben ts)
      (print ts)
    )
  )
)
