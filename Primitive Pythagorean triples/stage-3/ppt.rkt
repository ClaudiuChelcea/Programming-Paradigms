#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15

; Inmultueste elementele cu 'map' si aduna-le cu 'foldl'
(define (dot-product X Y)
  (foldl + 0 (map * X Y)));


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|

; Aplica dot-product pe fiecare rand al lui M
(define (multiply M V)
  (map (lambda (row-of-M) (dot-product row-of-M V)) M))

; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.

; Apeleaza functia 'get-list-of-transformations' cu valori nule
(define (get-transformations n)
  (get-list-of-transformations n 0 0 0))

; Aceasta functie va merge recursiv in calcularea nivelului pe care se va afla n si a nodurilor
; dinainte si din timpul acelui nivel, care, atunci cand gaseste nivelul dorit (cand nr de noduri totale
; e mai mare decat n, apeleaza functia care returneaza rezultatul)
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


; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).

; Aplicam fiecare functie din Fs pe tuple, modificand-o constant
(define (apply-functional-transformations Fs tuple)
  (and (map
   (lambda (f) (set! tuple (f tuple)))
   Fs)) tuple)


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.

; In cazul PPT, ca si data trecuta, se executa acelasi proces
; de gasire a raspunsului.
; In cazul cvartetelor, se aplica functia apply Q_index pe tuple initial
; pana se ajunge la raspunsul dorit
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

; bazat pe index, returneaza Q1, Q2 sau Q3
(define (get-quartet index)
  (cond
    ((= index 1) Q1)
    ((= index 2) Q2)
    (else Q3)))

; Pt PPT --------
; Daca am ajuns la Ts gol, inmulteste cu ppt folosind multiply definit anterior
; Daca nu, foloseste recursivitatea pe coada pentru a inmulti matricile dorite si salvat raspuns
; in acc - acumulator
; PT Q ----------
; Daca am ajuns la Ts gol, aplica functia de inmultire pe quartet-ul default
; Daca nu, foloseste recursivitatea pe coada pentru a inmulti quartetete si a salva
; raspunsul in acc - acumulator ; (apply Q1 (apply Q1 default-q))
(define (get-answer Ts ppt acc)
  (if (null? Ts)
      (multiply acc ppt)
      (get-answer (cdr Ts) ppt (matrix* acc (get-matrix (car Ts))))))

; Inmultirea a doua matrici
(define (matrix* Matrix1 Matrix2)
  (for/list ([row Matrix1])
    (for/list ([column (apply map list Matrix2)])
      (apply + (map * row column)))))

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define (get-nth-ppt-from-matrix-transformations n)
  (if (= n 1)
      (list 3 4 5) ; default case
      (get-nth-tuple (get-transformations n) (list 3 4 5) get-matrix)))

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define (get-nth-quadruple n)
  (if (= n 1)
      (list 1 1 2 3)
      (get-nth-tuple (get-transformations n) (list 1 1 2 3) get-quartet)))


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.

; Functia de conversie a unui cvartet la un ppt
(define (gh g e f h) (list (* g h) (* 2 e f) (+ (* e e) (* f f))))
(define (get-nth-ppt-from-GH-quadruples n)
  (apply gh (get-nth-quadruple n)))
