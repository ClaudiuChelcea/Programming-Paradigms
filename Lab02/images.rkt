#lang racket/gui

; ignorați următoarele linii de cod...
(define show-defaults 2) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #f) (define nopoints #t) (define name-ex '(testul testele trecut exercițiul)) ; variante: '(exercițiul exercițiile rezolvat exercițiul) sau '(testul testele trecut exercițiul) sau '(task taskurile rezolvat capitolul)
(define default-results `(#f 0 () ,empty-image your-code-here)) (define (default-result r) (set! default-results (cons r default-results))) (define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define exerciții 'string)
(define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define n-exercs -1) (define default-returns '()) (define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (sunt n s) (set! n-exercs n)) (define s-a string-append)
(define (p . L) (map (λ (e) (display e) (when (> (string-length (format "~a" e)) 0) (display " "))) L) (newline)) (define (p-n-ex) (format "[~a]" (if nopoints (string-join (list (symbol->string (cadddr name-ex)) (number->string n-ex) "/" (number->string n-exercs))) n-ex)))
(define (epart ep% pfix full) (if (< (caddr ep%) 1) (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (if (and nopoints (not full)) "" (number->string n-ex)) (symbol->string (cadr ep%))) (if (and nopoints (not full)) "" (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (number->string n-ex)))))
(define (whengood ep%) (let [(pts (* p-ex (caddr ep%)))] (and (if prepend (printf "+~v: " pts) (printf "~a[OK] " (p-n-ex))) (if nopoints (p (epart ep% "" #f) "rezolvat") (p (epart ep% "" #f) "rezolvat: +" pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts)))))
(define (whenbad ep% gvn expcd msg) (and (when (member gvn default-results) (set! default-returns (cons (epart ep% "" #t) default-returns))) (when (or (not (member gvn default-results)) (<= (length default-returns) show-defaults)) (bad-res ep% gvn expcd msg))))
(define (bad-res ep% gvn expcd msg) (p (if prepend "+0.0:" (format "~a[--]" (p-n-ex))) (epart ep% "la " #f) 'rezultatul gvn msg expcd))
(define (check-conds e gvn conds) (or (null? conds) (let ([r ((car conds) gvn)]) (if (eq? r #t) (check-conds e gvn (cdr conds)) (whenbad e gvn "" (or r "nu îndeplinește condiția"))))))
(define (check% part per given main-test expected . conds) (let* ([e (list n-ex part per)] [p? (pair? (cdr main-test))] [p (if p? (car main-test) identity)] [t ((if p? cadr car) main-test)] [m ((if p? cddr cdr) main-test)]) (when (eq? #t (check-conds e given conds)) (if (t (p given) expected) (whengood e) (whenbad e (p given) expected m)))))
(define (check given main-test expected . conds) (apply check% '- 1 given main-test expected conds)) (define the cons)
(define is (cons equal? "diferă de cel așteptat"))
(define in (cons member "nu se află printre variantele așteptate"))
(define same-set-as (cons (λ (x y) (apply equal? (map list->seteqv (list x y)))) "nu este aceeași mulțime cu"))
(define same-elements-as (cons (λ (x y) (and (apply = (map length (list x y))) ((car same-set-as) x y))) "nu sunt aceleași rezultate cu"))
(define (sumar) (when (and (not (null? default-returns)) (< show-defaults (length default-returns))) (p "... rezultatul implicit dat la" (cadr name-ex) (reverse default-returns))) (when (not nopoints) (p 'total: total 'puncte)))
(define (mark-helper) (printf "---~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p (car e-p) ': (cadr e-p) "puncte. total 1 -" (car e-p) ': (+ t (cadr e-p))) (+ t (cadr e-p))) 0 all) (newline))

(require 2htdp/image)

(define (cool-ellipse angle)
  (rotate angle (ellipse 150 30 "solid"
                         (make-color 0 0 255
                                     (max 10 (round (* (/ angle 360) 255)))))))
(define GEN-CIRCLE-LIST (append-map (λ (size) (map (λ (color) (circle size "solid" color)) '("blue" "green" "red" "yellow"))) '(10 5 20 40 50 30)))
(define GEN-IMAGES (list (circle 25 "solid" "yellow") (star 40 "solid" "orange") (triangle 50 "solid" "red") (square 50 "solid" "blue") 
      (triangle 50 "solid" "red") (star-polygon 20 10 3 "solid" "purple") 
      (circle 25 "solid" "yellow") (star 40 "solid" "orange") (triangle 50 "solid" "red") 
      (rhombus 30 60 "solid" "magenta") (regular-polygon 25 8 "solid" "green") (circle 25 "solid" "blue")
      (star-polygon 20 10 3 "solid" "purple") (circle 25 "solid" "yellow") (star 40 "solid" "orange") 
      (regular-polygon 15 10 "solid" "violet")))
(define GEN-BONUS-IMAGES (list (circle 25 "solid" "yellow") (triangle 50 "solid" "red") (square 50 "solid" "blue") (star 40 "solid" "orange") 
     (regular-polygon 25 8 "solid" "green") (circle 25 "solid" "blue") (star-polygon 20 10 3 "solid" "purple")))
(define GEN-INITIAL (foldl overlay empty-image (map cool-ellipse (range 0 91 10))))
(define TEST-4 (list (regular-polygon 15 10 "solid" "violet") (circle 25 "solid" "blue") (regular-polygon 25 8 "solid" "green")
     (rhombus 30 60 "solid" "magenta") (star-polygon 20 10 3 "solid" "purple") (square 50 "solid" "blue")  (triangle 50 "solid" "red")
     (star 40 "solid" "orange") (circle 25 "solid" "yellow")))
(define TEST-5 (list (square 50 "solid" "blue") (triangle 50 "solid" "red") (rhombus 30 60 "solid" "magenta") (regular-polygon 25 8 "solid" "green")
      (circle 25 "solid" "blue") (star-polygon 20 10 3 "solid" "purple") (circle 25 "solid" "yellow") (star 40 "solid" "orange")
      (regular-polygon 15 10 "solid" "violet")))

;; Funcții de checker pt ex 10
(define (has? x L)
  (and (not (null? L)) (or (equal? x (car L)) (has? x (cdr L)))))

(define (check-imgs1 images)
  (and (not (equal? images 'your-code-here))
       (equal? (length images) (length BONUS-IMAGES-RESULT1))
       (equal? (length images) (length (remove-duplicates-left images)))
       (andmap (λ (img) (has? img BONUS-IMAGES-RESULT1)) images)))

(define (check-imgs2 images)
  (and (not (equal? images 'your-code-here))
       (equal? (length images) (length BONUS-IMAGES-RESULT2))
       (equal? (length images) (length (remove-duplicates-left images)))
       (andmap (λ (img) (has? img BONUS-IMAGES-RESULT2)) images)))
(define BONUS-IMAGES-RESULT1 '(. . .))

(define BONUS-IMAGES-RESULT2 '(. . . . . . . . . . . . . . . . . . . . .))
(define IMAGES '(. . . . . . . . . . . . . . . .))
(define BONUS-IMAGES '(. . . . . . .))
(define INITIAL .)
; ...până aici.

(sunt 9 exerciții)

;; 0. (Introductiv)
;; Exercițiul 2 din laboratorul 1 solicita implementarea funcției de inversare,
;; pornind de la următoarele axiome:
;; rev([ ]) = [ ]
;; rev(x:l) = rev(l) ++ [x]
;; Ce tip de recursivitate este surprins aici?

(exercițiul 1 : 1 punct)
;; Implementați funcția de inversare utilizând celălalt tip de recursivitate.

(define (get-nth-right-element L n)
  (car (take-right L n)))

(define (rev L)
  (rev-aux L (list)))

(define (rev-aux L ans)
  (if (null? L)
      ans
      (rev-aux (drop-right L 1) (append ans (list (car (take-right L 1)))))))
    
  
(check (rev '(5 1 4 8 7)) is '(7 8 4 1 5))

(exercițiul 2 : 1.5 puncte)
;; Păstrați dintr-o listă de imagini doar acele imagini care au înălțimea mai mică decât height.
;; Pentru a determina înălțimea unei imagini folosiți funcția image-height. 
;; Exemplu: (image-height (ellipse 30 40 "solid" "orange"))

;; Restricții:
;; - funcția va fi recursivă pe stivă
;; - rezultatul va conține imaginile în ordinea în care ele apar în lista inițială

(define (lesser images height)
  (rev (get-answer images height)))

(define (get-answer images height)
  (if (null? images)
      '()
      (if (< (image-height (car images)) height)
          (append (get-answer (cdr images) height) (list (car images)))
       (get-answer (cdr images) height))))

(check% 'a 1/4 (lesser '(. . . . . . . . . . . . . . . . . . . . . . . .) 80)
                       is '(. . . . . . . . . . . . . . . .))
(check% 'b 1/4 (lesser '(. . . . . . . . . . . . . . . . . . . . . . . .) 10)
        is '())
(check% 'c 1/4 (lesser IMAGES 60) is '(. . . . . . . . . .))
(check% 'd 1/4 (lesser IMAGES 50) is '(. . . .))


(exercițiul 3 : 1.5 puncte)
;; Păstrați dintr-o listă de imagini doar acele imagini care au înălțimea mai mare sau egală cu height.

;; Restricții:
;; - funcția va fi recursivă pe coadă
;; - rezultatul va conține imaginile în ordinea în care ele apar în lista inițială

(define (greater images height)
  (greater-aux images height (list)))


(define (greater-aux images height acc)
  (if (null? images)
      acc
      (if (>= (image-height (car images)) height)
          (greater-aux (cdr images) height (append acc (list (car images))))
          (greater-aux (cdr images) height acc))))
  



(check% 'a 1/4 (greater IMAGES 50) is '(. . . . . . . . . . . .))
(check% 'b 1/4 (greater '(. . . . . . . . . . . . . . . . . . . . . . . .) 80)
        is '(. . . . . . . .))
(check% 'c 1/4 (greater BONUS-IMAGES 50) is '(. . . . . .))
(check% 'd 1/4 (greater IMAGES 100) is ' ())


(exercițiul 4 : 1.5 puncte)
;; Păstrați dintr-o listă prima apariție a elementelor duplicat. 
;; Care este tipul natural de recursivitate pentru această funcție?
;; Sugestie: (member x L) - verifică dacă elementul x este în lista L
;; Exemple: (member 2 '(1 2 3)) -> '(2 3), (member 4 '(1 2 3)) -> #f

;; Restricții:
;; - se va folosi funcția member pentru a verifica dacă un element aparține unei liste
;; - rezultatul va conține imaginile în ordinea în care ele apar în lista inițială


(define (remove-duplicates-left L)
  (if (null? L)
      '()
      (if (equal? #t (member (car L) (cdr L))) ; daca urmeaza si alte elemente identice (dubluri), adauga-l pe el. Daca in lista deja creata mai sunt deja acelasi el, nu il adauga
          (append (remove-duplicates-left (cdr L)) (list (car L)))
          (remove-duplicates-left (cdr L)))))


(check% 'a 1/2 (remove-duplicates-left '(1 2 3 3 4 5 6 3 3)) is '(1 2 3 4 5 6))
(check% 'b 1/2 (remove-duplicates-left IMAGES) is '(. . . . . . . . .))

(exercițiul 5 : 1.5 puncte)
;; Păstrați dintr-o listă ultima apariție a elementelor duplicat. Elementele din
;; lista finală vor fi în aceeași ordine.
;; Care este tipul natural de recursivitate pentru această funcție?

;; Restricții:
;; - se va folosi funcția member pentru a verifica dacă un element aparține unei liste
;; - NU se va folosi funcția remove-duplicates-left
;; - NU se va inversa lista 

(define (remove-duplicates-right L)
  'your-code-here)


(check% 'a 1/2 (remove-duplicates-right '(1 2 3 3 4 5 6 3 3)) is '(1 2 4 5 6 3))
(check% 'b 1/2 (remove-duplicates-right IMAGES) is '(. . . . . . . . .))


(exercițiul 6 : 3 puncte)
;; Generați triunghiul lui Sierpinski de latură L, pentru n iterații, folosind o listă de culori distincte.
;; Generarea va începe cu un triunghi echilateral colorat de latura L. La prima iterație,
;; acesta va fi împărțit în 3 triunghiuri echilaterale de latura L/2, urmând ca acest pas să fie
;; repetat pentru toate triunghiurile obținute.
;; Fiecare dintre cele 3 triunghiuri este determinat de unul din unghiurile triunghiului și segmentul care
;; unește mijloacele laturilor care formează unghiul.
;; Pentru colorarea triunghiului se va folosi lista colors, iar aceasta va fi rotită dupa colorarea unui triunghi.
;;   - pentru triunghiul de sus se va folosi lista inițială
;;   - pentru triunghiul din stânga se va folosi lista rotită cu o poziție (deci începând cu a doua culoare)
;;   - pentru triunghiul din dreapta se va folosi lista rotită cu 2 poziții (deci începând cu a treia culoare)
;;   - când terminăm iterațiile colorăm triunghiul de dimensiune elementară cu prima culoare din lista primită ca argument
;; Sugestii:
;; - Pentru lipirea a două imagini pe verticală puteți folosi functia above: (above upper_image lower_image)
;; - Pentru lipirea a două imagini pe orizontală puteți folosi funcția beside: (beside left_image right_image)
;; - Pentru obținerea triunghiului puteți folosi: (above triunghi_sus (beside triunghi_stange triunghi_dreapta))
;;  - Pentru a desena un triunghi echilateral verde de latură 20 veți folosi următoarea comandă: (triangle 20 "solid" "green")
;; Exemplu: L = 50; colors = ["green", "cyan", "purple", "yellow", "orange", "pink"]
;; n = 0 -> .
;; n = 1 -> .
;; n = 2 -> .

;; Restricții:
;; - se va folosi recursivitate arborescentă 

;; Hints - recursivitate
;; 1) cazul n = 0 -> se întoarce tringhiul desenat cu prima culoare din lista de culori
;; 2) cazul general (n > 0) -> construim un triunghi folosind trei triunghiuri, in felul urmator:
;;       - se generează triunghiul de sus, de latură L/2, folosind prima culoare din listă, și imaginea formată din triunghiul de sus
;; este lipită vertical de imaginea formată de celelalte 2 triunghiuri (generate jos), folosind funcția above
;;       - se generează cele două tringhiuri de jos, de latură L/2, folosind a doua și a treia culoare din listă (care este circulară!),
;; iar imaginile formate de cele două triunghiuri sunt alipite orizontal,
;; imaginea rezultată fiind lipită apoi de imaginea triunghiului de mai sus folosind funcția above (fapt menționat mai sus)

(define (sierpinski n L colors)
  'your-code-here)

(check% 'a 1/4 (sierpinski 3 100 '("green" "cyan" "purple" "yellow" "orange" "pink")) is .)

(check% 'b 1/4 (sierpinski 4 100 '("cyan" "pink")) is .)

(check% 'c 1/4 (sierpinski 5 100 '("cyan" "purple" "pink")) is .)
(check% 'd 1/4 (sierpinski 6 120 '("cyan" "pink" "blue" "yellow")) is .)


(exercițiul 7 : 1 punct BONUS)
;; Pornind de la o listă de imagini, și de la o imagine inițială, construiți
;; o unică imagine, rezultată prin suprapunerea tuturor imaginilor, în maniera
;; următoare:
;; * prima imagine din listă deasupra imaginii inițiale
;; * a doua imagine din listă deasupra rezultatului
;; ...
;; * ultima imagine din listă deasupra rezultatului.
;; Numele funcției este 'overlay->', pentru a indica sensul de parcurgere
;; de la stânga la dreapta.
;; Care este tipul natural de recursivitate pentru această funcție?

;; Restricții:
;; - se va folosi  funcția predefinită (overlay <deasupra> <dedesubt>)
;; - dacă lista de imagini este vidă, se va returna imaginea inițială


(define (overlay-> initial images)
  'your-code-here)

(check (overlay-> INITIAL BONUS-IMAGES) is .)

(exercițiul 8 : 2 puncte BONUS)
;; Implementați sortarea prin interclasare (merge sort) pentru imagini
;; Sortarea trebuie să fie în ordine descrescătoare, cu funcția de ordine:
;; img1 < img2 = (image-height img1) < (image-height img2)
;; În cazul în care înălțimile imaginilor sunt egale, se va compara lățimea folosind image-width
;; Axiome mergesort:
;; mergesort [] = [] - o listă goală este deja sortată
;; mergesort [x] = [x]  - o listă cu un element este deja sortată
;; mergesort L = merge ;; mergesort L = merge (mergesort (fst-half L)) (mergesort (snd-half L)) unde:
;; - merge = funcție de interclasare a două liste (primește două liste sortate
;;           și întoarce o listă sortată compusă din cele două liste primite)
;; - fst-half / snd-half = funcții ce împart o listă în două jumătăți

;; Sugestii:
;; - vă puteți defini o funcție auxiliară pentru comparații (operația img1 < img2)
;; - vă puteți defini o funcție auxiliară pentru logica de merge
;; - dacă aveți nevoie de un parametru în plus la o funcție si valoarea inițială a acestuia este cunoscută,
;; puteți defini o funcție auxiliară cu un parametru în plus


(define (mergesort L)
  'your-code-here)


(check% 'a 1/2 (mergesort '(.  . ...  . . .)) is '(. . . . . . . .))
(check% 'b 1/2 (mergesort '(. . . . . . . . .)) is '(. . . . . . . . .))


(exercițiul 9 : 2 puncte BONUS)
;; Se dă o listă cu n imagini și un număr natural k mai mic sau egal cu dimensiunea listei. Să se genereze o noua listă cu imagini astfel:
;; - se generează toate submulțimile de dimensiune k ale listei primite
;; - se sorteaza în ordine crescătoare fiecare submulțime
;; - se suprapun imaginile din fiecare submulțime astfel încât prima imagine să fie cea mai din spate, iar ultima imagine să fie imaginea
;; din față
;; - lista de imagini cerută este lista formată din imaginile suprapuse din fiecare submulțime generată
;; - pentru a genera o imagine de fundal folosiți empty-image
;;
;; Sugestii:
;; - pentru fiecare element din listă poți alege dacă îl adaugi sau nu la submulțimea curentă. Deoarece dorim să creăm toate submulțimile
;; trebuie să luăm în calcul ambele variante.
;; - în cadrul unui apel de funcție, prima dată sunt evaluați parametrii, apoi funcția propriu zisă! Astfel, rezultatul unui apel recursiv
;; poate fi folosit drept rezultat intermediar pentru un alt apel.
;;
;; Pentru verificarea rezultatelor (în caz ca aveți un rezultat greșit) listele cu imaginile ce trebuie să rezulte se gasesc în variabilele
;; BONUS-IMAGES-RESULT1/2. Rezultatul vostru îl puteți obține prin rularea apelului image-subsets din cele două apeluri check%.

(define (image-subsets images k)
  'your-code-here)
                      

(check% 'a 1/2 (check-imgs1 (image-subsets '(. . .) 2)) is #t)

(check% 'b 1/2 (check-imgs2 (image-subsets '(. . ..  . . .) 5)) is #t)


(sumar)
