; Total nicht von http://de.wikipedia.org/wiki/RSA-Kryptosystem geklaut.

; Assertions.
(define (assert gegeben erwartet)
  (if (equal? gegeben erwartet) '()
    (begin
      (print "gegeben<" gegeben ">")
      (print "erwartet<" erwartet ">")
      (error "assert" "assetion failed" erwartet)
    )
  )
)

; Exponenten mit Modulo bilden. Erstens unterstützt expt keine bignums,
; zweitens würde das rekursiv lange dauern.
;   http://en.wikipedia.org/wiki/Modular_exponentiation
(define (pow basis exponent mod)
  (letrec*
    (
      (fastpow
        (lambda (basis exponent result)
          (if (> exponent 0)
            (let
              (
                (r'
                  (if (= (modulo exponent 2) 1)
                    (modulo (* result basis) mod)
                    result
                  )
                )
              )
              (fastpow (modulo (* basis basis) mod) (/fx exponent 2) r')
            )
            result
          )
        )
      )
    )
    (fastpow basis exponent 1)
  )
)

(assert (pow 2 5 100) 32)
(assert (pow 2 5 7) (modulo 32 7))
(assert (pow 4 13 497) 445)

; erweiterter pro-Algorithmus
(define (erweiterter_euklid a b)
  (if (= b 0)
    (list a 1 0)
    (begin
      (let*
        (
          (tmp (erweiterter_euklid b (modulo a b)))
          (d (car tmp))
          (s (cadr tmp))
          (t (caddr tmp))
          (s' (- s (* (/fx a b) t)))
        )
        (list d t s')
      )
    )
  )
)

; Prüft, ob eine Zahl eine Primzahl ist laut Miller-Rabin-Test.
;   http://mathworld.wolfram.com/Rabin-MillerStrongPseudoprimeTest.html
; > Given an odd integer n [1], let n=2^rs+1 with s odd [2,3].
; > Then choose a random integer a with 1<=a<=n-1 [4].
; > If a^s=1 (mod n) [5] or a^(2^js)=-1 (mod n) [6] for some 0<=j<=r-1, then n passes the test.
(define (prim? n)
  (cond
    ((< n 2) #f)
    ((>= n 2152302898747) (error "prim?" "Zu große Zahl" n)) ; '(2, 3) sind ausreichend für alle Zahlen < dieser
    ((memq n '(2 3 5 7 11)) #t)
    ((odd? n) ; [1]
      (letrec*
        (
          (calcR
            (lambda (n)
              (if (odd? n)
                0
                (+ 1 (calcR (/fx n 2)))
              )
            )
          )
          (r (calcR (- n 1))) ; [2]
          (s (/ (- n 1) (expt 2 r))) ; [3]

          ; [6] a^(2js) = -1 mod n
          (testPotenzen
            (lambda (a r)
              (cond ((< r 0) #f)
                    ((= (- n 1) (pow a (* (expt 2 r) s) n)) #t)
                    (else (testPotenzen a (- r 1)))
              )
            )
          )

          ; [5] a^s = 1 mod n
          (testEinzeln
            (lambda (a r)
              ;(print "einz " a " " r)
              (if (= 1 (pow a s n))
                #t
                (testPotenzen a r)
              )
            )
          )

          (test
            (lambda (r werte)
              ;(print r " " werte)
              (if (null? werte)
                #t
                (and (testEinzeln (car werte) r) (test r (cdr werte)))
              )
            )
          )
        )
        (test r
          '(2 3 5 7 11) ; [4]
        )
      )
    )
    (else #f)
  )
)

(assert (prim? 2) #t)
(assert (prim? 3) #t)
(assert (prim? 5) #t)
(assert (prim? 7) #t)
(assert (prim? 13) #t)
(assert (prim? 99) #f)
(assert (prim? 101) #t)
(assert (prim? 72101) #t)


; Länge eines Blocks, wird anhand der Länge von n bestimmt.
; log n/log 2 bestimmt die Bits, die in n enthalten sind, das wird anschließend durch 8 geteilt = 1 Byte
(define (blocklänge n) (flonum->fixnum (/ (log n) (log 256))))

; Implementierung hinreichender Primzahlen.
; Gibt eine Liste mit (n e d) zurück.
(define (schlüssel)
  (letrec*
    (
      ; Berechnet eine Primzahl im Bereich von <= p < bis.
      (prime
        (lambda (von bis)
          (let*
            (
              (r (+ von (random (- bis von))))
            )
            (if (prim? r) r (prime von bis))
          )
        )
      )

      ; Primzahlen p, q für RSA, im Bereich von 2^20 <= p < 2^30; 2^20 <= q < 2^30
      (p (prime (expt 2 20) (expt 2 30)))
      (q (prime (expt 2 20) (expt 2 30)))
      (n (* p q))
      (phi (* (- p 1) (- q 1)))

      (e (prime 2 (- phi 1))) ; eine zu phi(n) teilerfremde Zahl - Primzahlen sind immer teilerfremd
                              ; phi(n) ist nichtmal ne Primzahl.

      (d (cadr (erweiterter_euklid e phi)))
    )
    (if (< d 0)
      (schlüssel) ; Shit happens
      (list n e d)
    )
  )
)

; Verschlüsselt einen Text m. Gibt eine Liste mit den verschlüsselten Blöcken der Länge (blocklänge) zurück.
(define (verschlüsseln m e n)
  (letrec*
    (
      (b' (min (string-length m) (blocklänge n)))
      (alszahl
        ; es ist bedeutend einfacher, mit der umgekehrten Liste zu rechnen - dann ergibt sich das
        ; Kodieren in eine Zahl (einfaches Byte-Addieren) als Trivial.

        ; Beispiel.
        ;   String original = Di
        ;   String in Bytes (Dezimal) = 68 105 --> 68 * 256 + 105
        ; hier, 105 (da umgekehrte Liste) + 256 * rest-der-liste --> 105 + (256 * (68 + (255 * 0))) = 17513
        (lambda (L)
          (if (null? L)
            0
            (+
              (char->integer (car L))
              (* 256 (alszahl (cdr L)))
            )
          )
        )
      )
      (text
        ; Verschlüsselt einen String mit einer Länge von maximal (blocklänge) Zeichen.
        (lambda (s)
          (pow (alszahl (reverse (string->list s))) e n)
        )
      )
    )
    (if (= b' 0)
      '()
      (cons (text (substring m 0 b')) (verschlüsseln (substring m b') e n))
    )
  )
)

; Entschlüsselt einen Text.
(define (entschlüsseln m d n)
  (if (null? m)
    ""
    (letrec*
      (
        ; Konvertiert ein bignum in ein Integer, da von bigloo dazu keine Funktion bereitgestellt wird
        (bignum->integer
          (lambda (a b)
            (if (= #z0 a)
              b
              (bignum->integer (- a 1) (+ b 1))
            )
          )
        )
        ; Konvertiert eine Zahl (integer/bignum) in ein Zeichen.
        (tochar
          (lambda (z)
            (integer->char (if (bignum? z) (bignum->integer z 0) z))
          )
        )
        ; Konvertiert eine Zahl in eine Ansammlung von Buchstaben.
        (alstext
          (lambda (t)
            (if (< t 1)
              "" ; Ende
              (let
                (
                  (rest (/ t 256))
                  (aktuell
                    (tochar
                      (modulo
                        (if (flonum? t)
                          (flonum->fixnum (floor t))
                          t
                        )
                        256
                      )
                    )
                  )
                )
                ; Wir rollen das Feld natürlich wieder von hinten auf - also die aktuellen
                ; Zeichen jeweils hinten ranhängen.
                (string-append (alstext rest) (string aktuell))
              )
            )
          )
        )
        (text
          ; Entschlüsselt einen Text, d.h. Zahl -> Buchstaben.
          (lambda (L)
            (alstext (pow L d n))
          )
        )
      )
      (string-append (text (car m)) (entschlüsseln (cdr m) d n))
    )
  )
)

; manuell berechnetes Beispiel von http://www.nord-com.net/h-g.mekelburg/krypto/mod-asym.htm#rsa
(assert (verschlüsseln "" 7 123107) '())
(assert (verschlüsseln "Di" 7 123107) '(2291))
(assert (verschlüsseln "Dies ist ein Klartext.." 7 123107) ; ungerade Länge, aber gerade Blocklänge
  '(2291 110182 93977 3622 77784 98507 38295 105415 8355 22915 45595 41845))
(assert (verschlüsseln "Dies ist ein Klartext..." 7 123107)
  '(2291 110182 93977 3622 77784 98507 38295 105415 8355 22915 45595 81891))


(assert (entschlüsseln '() 69943 123107) "")
(assert (entschlüsseln '(2291) 69943 123107) "Di")
(assert (entschlüsseln '(2291 110182 93977 3622 77784 98507 38295 105415 8355 22915 45595 41845)
  69943 123107) "Dies ist ein Klartext..")
(assert (entschlüsseln '(2291 110182 93977 3622 77784 98507 38295 105415 8355 22915 45595 81891)
  69943 123107) "Dies ist ein Klartext...")

; And now for something completely different: eine Konsole, damit man mal eine hat.
; Falls man die braucht, weil bigloo sonst keine (triviale) Möglichkeit dafür bereitstellt.
; (repl)
(let*
  (
    (s (schlüssel))
    (n (car s))
    (e (cadr s))
    (d (caddr s))
    (text (verschlüsseln "Dieser Text soll nur soetwas wie ein Beispiel sein, braucht den jemand?" e n))
  )
  (print " n = " n ", blocklänge(n) =  " (blocklänge n))
  (print " öffentlicher Schlüssel e = " e)
  (print " privater Schlüssel d = " d)
  (print text)
  (print (entschlüsseln text d n))
)
