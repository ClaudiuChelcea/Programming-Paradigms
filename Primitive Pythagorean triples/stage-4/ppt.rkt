#lang racket

(provide (all-defined-out))

;; Dacă ne interesează doar al n-lea TPP din arbore, este
;; convenabil să determinăm secvența de transformări care
;; conduce la acest TPP, așa cum am procedat până acum.
;;
;; În schimb, dacă ne interesează primele n TPP (sau în
;; general o secvență mai lungă de TPP) ar fi de preferat
;; crearea unui flux infinit care să le conțină pe toate
;; în ordine.
;;
;; Observăm că această ordine corespunde unei parcurgeri
;; BFS a arborelui infinit. Acesta este un BFS mai simplu
;; decât BFS-ul uzual
;; (https://en.wikipedia.org/wiki/Breadth-first_search),
;; întrucât succesorii unui TPP sunt automat triplete noi,
;; deci nu este necesar să verificăm dacă un nod a mai
;; fost sau nu vizitat.
;; 
;; Schema acestui BFS simplificat este:
;;  1. inițializăm coada de noduri care trebuie vizitate cu
;;     rădăcina arborelui (tripletul (3,4,5))
;;  2. adăugăm primul nod din coadă în rezultat
;;  3. adăugăm cei 3 succesori ai săi în coada de noduri
;;     care trebuie vizitate
;;  4. revenim la pasul 2 (întrucât construim un flux
;;     infinit, nu există condiție de oprire, și toate
;;     structurile sunt fluxuri: atât coada cât și
;;     rezultatul funcției BFS)

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementările
; funcțiilor dot-product și multiply din etapa 1 sau 2.
; Cele două funcții nu sunt re-punctate de checker, însă 
; sunt necesare generării succesorilor unui nod.
(define (dot-product X Y)
  (foldl + 0 (map * X Y)));

(define (multiply M V)
  (map (lambda (row-of-M) (dot-product row-of-M V)) M))

(define (get-transformations n)
  (get-list-of-transformations n 0 0 0))

(define (get-list-of-transformations n level total-prev-nodes total-lower-level-nodes)
  (if (= n 1) ; default case
      (list)
  (if (= n 2) ; default case
         (list 1)
         (if (= n 3) ; default case
             (list 2)
             (if (= n 4) ; default case
                 (list 3)
  (if (< n total-prev-nodes) ; Daca nr de noduri curente va depasi n (de ex, pt n = 64, se va opri la total-prev-nodes = 121)
      ; For true, show result
      (get-result n (+ total-lower-level-nodes 1) total-prev-nodes (- level 2) (list))

      ; For false, go down the tree
      (get-list-of-transformations n (+ level 1) (+ total-prev-nodes (expt 3 level)) (quotient (+ total-prev-nodes (expt 3 level)) 3))))))))

; Daca nivelul este -1, returneaza lista
; Altfel parcurge recursiv, actualizand minimul si maximul sau chiar ambele in functie de unul dintre cele 3 cazuri
; pana gasim locul in care se incadreaza 'n'-ul nostru si returneaza lista dorita
(define (get-result n min max level list-ans)
  (if (= level -1)
      list-ans
      ; altfel pune in lista valoarea ceruta
      (cond
        ((< n (+ min (expt 3 level))) (get-result n min (- (+ min (expt 3 level)) 1) (- level 1) (append list-ans (list 1)))); actualizam maximul
        ((and (>= n (+ min (expt 3 level))) (< n (+ min (* (expt 3 level) 2)))) (get-result n (+ min (expt 3 level)) (- (+ min (* (expt 3 level) 2)) 1) (- level 1) (append list-ans (list 2)))) ; actualizam si minimul si maximul
        (else (get-result n (+ min (* (expt 3 level) 2)) max (- level 1) (append list-ans (list 3))))))) ; actualizam minimul

(define (apply-functional-transformations Fs tuple)
  (and (map
   (lambda (f) (set! tuple (f tuple)))
   Fs)) tuple)

(define default-q (list 1 1 2 3))
(define (get-nth-tuple Ts start-tuple fct-1)
  (if (= (length start-tuple) 3)
      (get-answer (cdr (reverse Ts)) start-tuple (fct-1 (get-right-element-list Ts)))
      (and (set! default-q (list 1 1 2 3)) (for/list ([value Ts])
      (set! default-q (apply (fct-1 value) default-q))) default-q)))

; un take-right pe lista
(define (get-right-element-list L)
  (car (reverse L)))

; bazat pe index, returneaza T1, T2 sau T3
(define (get-matrix index)
  (cond
    ((= index 1) T1)
    ((= index 2) T2)
    (else T3)))

(define (get-answer Ts ppt acc)
  (if (null? Ts)
      (multiply acc ppt)
      (get-answer (cdr Ts) ppt (matrix* acc (get-matrix (car Ts))))))

; Inmultirea a doua matrici
(define (matrix* Matrix1 Matrix2)
  (for/list ([row Matrix1])
    (for/list ([column (apply map list Matrix2)])
      (apply + (map * row column)))))

(define (get-nth-ppt-from-matrix-transformations n)
  (if (= n 1)
      (list 3 4 5) ; default case
      (get-nth-tuple (get-transformations n) (list 3 4 5) get-matrix)))

; TODO
; Definiți fluxul infinit de TPP folosind algoritmul descris
; (parcurgerea BFS a arborelui infinit).
; Funcție utilă: stream-append
; Folosiți cel puțin o formă de let.
(define ppt-stream-in-tree-order
   (let bfs [(queue (list (list 3 4 5)))]
   (stream-cons (car queue)
                (bfs (cdr (append queue
                                  (list (multiply T1 (car queue)))
                                  (list (multiply T2 (car queue)))
                                  (list (multiply T3 (car queue)))))))))


;; Un alt mod de a genera TPP se folosește de perechi (g, h)
;; care indeplinesc condițiile:
;;    g, h impare
;;    g < h
;;    g, h prime între ele
;;
;; Nu întâmplător am ales aceste notații, teoria este aceeași
;; cu cea din spatele cvartetelor (g, e, f, h), pe care le
;; putem exprima și ca (g, (h-g)/2, (h+g)/2, h).
;;
;; Pentru a obține un TPP dintr-o pereche (g, h) se aplică
;; aceleași formule (dar le vom exprima în funcție de g și h):
;;    a = gh
;;    b = 2ef = (h - g)(h + g) / 2
;;      = (h^2 - g^2) / 2
;;    c = e^2 + f^2 = (h - g)^2 / 4 + (h + g)^2 / 4
;;      = (h^2 + g^2) / 2
;;
;; Acest mod de generare ne furnizează TPP în altă ordine
;; decât cea dată de parcurgerea în lățime a arborelui TPP.
;;
;; Noua ordine se obține parcurgând pe coloane diagrama:
;;                        h      
;;         3     5     7     9     11   .  .  .
;;    1  (1,3) (1,5) (1,7) (1,9) (1,11) .  .  .
;;    3        (3,5) (3,7)   -   (3,11) .  .  .
;;    5              (5,7) (5,9) (5,11) .  .  .
;; g  7                    (7,9) (7,11) .  .  .
;;    9                          (9,11) .  .  .
;;    .                                 .  .  .
;;    .                                    .  .
;;    .                                       .
;; (lipsește perechea (3,9), 3 și 9 nefiind prime între ele)
;;
;; Folosind această indexare, primele 6 TPP sunt:
;;    (3,4,5)                           - din perechea (1,3)
;;    (5,12,13), (15,8,17)              - din (1,5), (3,5)
;;    (7,24,25), (21,20,29), (35,12,37) - din (1,7), (3,7), (5,7)
;;
;; Ne propunem să definim fluxul infinit de TPP în ordinea de
;; mai sus. Acesta se bazează pe fluxul corespunzător de 
;; perechi (g, h), pe care îl generăm astfel:
;;  - pornim cu 2 fluxuri infinite:
;;    * G = 1, 3, 5, 7 ...
;;    * H = 3, 5, 7, 9 ... (întrucât g < h)
;;  - fluxul ordonat pe coloane va conține:
;;    * perechea compusă din cele mai mici numere din G și H
;;      (ex: (1,3))
;;    * apoi interclasarea (conform ordinii "pe coloane") între:
;;      - perechile compuse dintre minimul din G și restul din H
;;        (ex: (1,5), (1,7), (1,9) ...)
;;      - fluxul ordonat generat de restul lui G și restul lui H
;;        (ex: (3,5), (3,7), (5,7) ...)
;; Aceasta este abordarea generală, în urma căreia generăm toate
;; perechile, inclusiv pe cele de numere care nu sunt prime  
;; între ele. Perechile neconforme trebuie înlăturate ulterior
;; (utilizând funcția de bibliotecă gcd).


; TODO
; Definiți o funcție care primește 2 fluxuri numerice infinite
; G și H, și generează fluxul de perechi de câte un element 
; din G și unul din H ordonate conform metodei de mai sus.
; Condițiile ca g și h să fie impare, prime între ele, respectiv
; menținerea restricției g < h (cât timp urmați algoritmul) nu
; trebuie impuse în implementarea funcției pairs.
; Ele vor fi asigurate de definirea fluxurilor de mai jos prin:
;  - apelarea lui pairs exclusiv pe fluxurile
;    G = 1, 3, 5, 7 ... și H = 3, 5, 7, 9 ...
;  - eliminarea perechilor de numere neprime între ele (care 
;    există în rezultatul funcției pairs, dar nu vor mai exista
;    în fluxul gh-pairs-stream)
(define (pairs G H)
  (let [(index 1)]
    

; TODO
; Definiți fluxul de perechi (g, h) pe care se bazează noua
; indexare a TPP.
; Nu folosiți recursivitate explicită (decât pentru a genera
; fluxurile de pornire - G și H).
(define gh-pairs-stream
  'your-code-here)


; TODO
; Definiți fluxul de TPP corespunzător fluxului anterior de
; perechi (g, h).
(define ppt-stream-in-pair-order
  'your-code-here)


