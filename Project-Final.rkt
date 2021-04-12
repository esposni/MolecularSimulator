;Molecular Simulator

;This file provide a tool to visualize a chemical formula as a 3d model
;Accept:
;  - MAX 2 element for covalent bond
;  - All element with group numbered with roman numbers (not metals of transition)
;  - "Element1" "N1" and "Element1" "N2" with N1<2 if N1<N2 or viceversa. (Ex. NH4, not N2H4)
;  - "Element1" "N1"  with N1<=2 

;Author: Nicola Esposito, Enrico Fomasi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

;Libraries
(require racket/gui/base
         racket/include
         pict3d
         racket/draw
         test-engine/racket-tests
         racket/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Data definition

;An element is a structure.
;make-element : Integer String Real String -> element
;element-group : element -> Integer
;element-symbol : element -> String
;element-electroneg : element -> Real
;element-oxidation : element -> String
;element? : Any -> Boolean
;Interpretation: the properties of each element of the periodic table
;Template for element
;(define (fn-for-element element)
;  (... (element-group element)
;       (element-symbol element)
;       (element-electroneg element)
;       (element-oxidation element)))

(struct element (group symbol electroneg oxidation) #:transparent)

;A given is a structure
;make-given : String Integer -> given
;given-symbol: given -> String
;given-n : given -> Integer
;given? : Any -> boolean
;Interpretation: the properties of the elements given by the user
;Template for given
;(define (fn-for-given given)
;  (...(given-symbol given)
;      (given-n given)))

(struct given (symbol n) #:transparent)

;A degrees is a structure
;make-degrees : Integer Integer Integer Integer -> degrees
;degrees-Y : degrees -> Integer
;degrees-X : degrees -> Integer
;degrees-Z : degrees -> Integer
;degrees? : Any -> boolean
;Interpretation: the angle of rotation of the bond into a 3d scene
;Template for degrees
;(define (fn-for-degrees degrees)
;  (...(degrees-X degrees)
;      (degrees-Y degrees)
;      (degrees-Z degrees))

(struct degrees (Y X Z) #:transparent)

;A posit is a structure
;make-posit :  Integer Integer Integer Integer Integer Integer-> posit
;posit-Y : posit -> Integer
;posit-X : posit -> Integer
;posit-Z : posit -> Integer
;posit-gY : posit -> Integer
;posit-gX : posit -> Integer
;posit-gZ : posit -> Integer
;posit? : Any -> boolean
;Interpretation: the position and the angle of rotatio of the molecule into the 3d scene
;Template for posit
;(define (fn-for-posit posit)
;  (...(posit-Y posit)
;      (posit-X posit)
;      (posit-Z posit)
;      (posit-gY posit)
;      (posit-gX posit)
;      (posit-gZ posit)))

(struct posit (Y X Z gY gX gZ) #:transparent)

;A param is a structure
;make-param : Struct Struct %color
;param-posit : param -> (make-posit Integer Integer Integer Integer Integer Integer)
;param-degrees : param -> (make-degrees Integer Integer Integer Integer)
;param-color : param -> %color
;param? : Any -> boolean
;Interpretation: the parameters of a single molecule into a 3d scene
;Template for param
;(define (fn-for-param param)
;  (...(param-posit param)
;      (param-degrees param)
;      (param-color param)))

(struct param (posit degrees color) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions

;String Integer Integer String Real String String -> Struct
;Given an integer about the number of the group
;a string about the symbol of the element
;a real about the electronegativity of the element
;a string about the numbers of oxidation of the element
;Return a struct
(define (make-element group symbol electroneg oxidation)
  (element group symbol electroneg oxidation))

(check-expect (element-group (make-element 1 "H" 2.20 "+1-1")) 1)
(check-expect (element-symbol (make-element 1 "H" 2.20 "+1-1")) "H")
(check-within (element-electroneg (make-element 1 "H" 2.20 "+1-1")) 2.20 2.20)
(check-expect (element-oxidation (make-element 1 "H" 2.20 "+1-1")) "+1-1")

;String Integer -> Struct
;Given a string about the symbol of the element given
;an integer about the number of elements of the given element
;return a struct
(define (make-given symbol n)
  (given symbol n))

(check-expect (given-symbol (make-given "H" 1)) "H")
(check-expect (given-n (make-given "H" 1)) 1)

;Integer Integer Integer Integer -> Struct
;Given integers about the rotation on X Y Z
;into a 3d scene return a struct
(define (make-degrees Y X Z)
  (degrees Y X Z))

(check-expect (degrees-Y (make-degrees 90 180 0)) 90)
(check-expect (degrees-X (make-degrees 90 180 0)) 180)
(check-expect (degrees-Z (make-degrees 90 180 0)) 0)

;Integer Integer Integer Integer Integer Integer -> Struct
;Given integers about the rotation on X Y Z and the posititon X Y Z
;into a 3d scene return a struct 
(define (make-posit Y X Z gY gX gZ)
  (posit Y X Z gY gX gZ))

(check-expect (posit-Y (make-posit 90 180 0 90 180 0)) 90)
(check-expect (posit-X (make-posit 90 180 0 90 180 0)) 180)
(check-expect (posit-Z (make-posit 90 180 0 90 180 0)) 0)
(check-expect (posit-gY (make-posit 90 180 0 90 180 0)) 90)
(check-expect (posit-gX (make-posit 90 180 0 90 180 0)) 180)
(check-expect (posit-gZ (make-posit 90 180 0 90 180 0)) 0)

;Struct Struct %color -> Struct
;Given a struct about the position of the element and
;a struct about the degrees of the element and the color of the
;element return a struct
(define (make-param posit degrees color)
  (param posit degrees color))

(check-expect (param-posit (make-param (make-posit 90 180 0 90 180 0)
                           (make-degrees 90 180 0)
                           (make-color 0 0 0))) (make-posit 90 180 0 90 180 0))
(check-expect (param-degrees (make-param (make-posit 90 180 0 90 180 0)
                             (make-degrees 90 180 0)
                             (make-color 0 0 0))) (make-degrees 90 180 0))

;periodic-table is a List<element> which contains all the 120 element of a Periodic Table
(define periodic-table
  (list                       
   (make-element     1     "H"     2.20      "+1-1")
   (make-element     18    "He"    0         "x")
   (make-element     1     "Li"    0.98      "+1")
   (make-element     2     "Be"    1.57      "+2")
   (make-element     13    "B"     2.04      "+3")
   (make-element     14    "C"     2.55      "-4+2+4")
   (make-element     15    "N"     3.04      "-3+2+3+4+5")
   (make-element     16    "O"     3.44      "-2")
   (make-element     17    "F"     3.98      "-1")
   (make-element     18    "Ne"    0         "x")
   (make-element     1     "Na"    0.93      "+1")
   (make-element     2     "Mg"    1.31      "+2")
   (make-element     13    "Al"    1.61      "+3")
   (make-element     14    "Si"    1.90      "-4+2+4")
   (make-element     15    "P"     2.19      "-3+3+5")
   (make-element     16    "S"     2.58      "-2+4+6")
   (make-element     17    "Cl"    3.16      "-1+1+3+5+7")
   (make-element     18    "Ar"    0         "x")
   (make-element     1     "K"     0.82      "+1")
   (make-element     2     "Ca"    1.00      "+2")
   (make-element     3     "Sc"    1.36      "+3")
   (make-element     4     "Ti"    1.54      "+2+3+4")
   (make-element     5     "V"     1.63      "+2+3+4+5")
   (make-element     6     "Cr"    1.66      "+2+3+4+6")
   (make-element     7     "Mn"    1.55      "+2+3+4+6+7")
   (make-element     8     "Fe"    1.83      "+2+3+6")
   (make-element     9     "Co"    1.88      "+2+3")
   (make-element     10    "Ni"    1.91      "+2+3")
   (make-element     11    "Cu"    1.90      "+1+2")
   (make-element     12    "Zn"    1.65      "+2")
   (make-element     13    "Ga"    1.81      "+3")
   (make-element     14    "Ge"    2.01      "+2+4")
   (make-element     15    "As"    2.18      "-3+3+5")
   (make-element     16    "Se"    2.55      "-2+4+6")
   (make-element     17    "Br"    2.96      "-1+1+3+5")
   (make-element     18    "Kr"    3.00      "x")
   (make-element     1     "Rb"    0.82      "+1")
   (make-element     2     "Sr"    0.95      "+2")
   (make-element     3     "Y"     1.22      "+3")
   (make-element     4     "Zr"    1.33      "+4")
   (make-element     5     "Nb"    1.60      "+3+5")
   (make-element     6     "Mo"    2.16      "+1+2+3+4+5+6")
   (make-element     7     "Tc"    1.90      "+4+5+6+7")
   (make-element     8     "Ru"    2.20      "+2+3+4+5+6+7")
   (make-element     9     "Rh"    2.28      "+3")
   (make-element     10    "Pd"    2.20      "+2+4")
   (make-element     11    "Ag"    1.93      "+1")
   (make-element     12    "Cd"    1.69      "+2")
   (make-element     13    "In"    1.78      "+3")
   (make-element     14    "Sn"    1.96      "+2+4")
   (make-element     15    "Sb"    2.05      "-3+3+5")
   (make-element     16    "Te"    2.10      "-2+4+6")
   (make-element     17    "I"     2.66      "-1+1+5+7")
   (make-element     18    "Xe"    2.60      "x")
   (make-element     1     "Cs"    0.79      "+1")
   (make-element     2     "Ba"    0.89      "+2")
   (make-element     3     "La"    1.10      "+3")
   (make-element     3     "Ce"    1.12      "+3+4")
   (make-element     3     "Pr"    1.13      "+3")
   (make-element     3     "Nd"    1.14      "+3")
   (make-element     3     "Pm"    1.14      "+3")
   (make-element     3     "Sm"    1.17      "+2+3")
   (make-element     3     "Eu"    1.20      "+2+3")
   (make-element     3     "Gd"    1.29      "+3")
   (make-element     3     "Tb"    1.20      "+3")
   (make-element     3     "Dy"    1.22      "+3")
   (make-element     3     "Ho"    1.23      "+3")
   (make-element     3     "Er"    1.24      "+3")
   (make-element     3     "Tm"    1.25      "+2+3")
   (make-element     3     "Yb"    1.10      "+2+3")
   (make-element     3     "Lu"    1.27      "+3")
   (make-element     4     "Hf"    1.30      "+4")
   (make-element     5     "Ta"    1.50      "+5")
   (make-element     6     "W"     2.36      "+2+3+4+5+6")
   (make-element     7     "Re"    1.90      "+4+6+7")
   (make-element     8     "Os"    2.20      "+2+3+4+6+8")
   (make-element     9     "Ir"    2.20      "+3+4")
   (make-element     10    "Pt"    2.28      "+2+4")
   (make-element     11    "Au"    2.54      "+1+3")
   (make-element     12    "Hg"    1.90      "+1+2")
   (make-element     13    "Tl"    2.04      "+1+3")
   (make-element     14    "Pb"    2.33      "+2+4")
   (make-element     15    "Bi"    2.02      "+3+5")
   (make-element     16    "Po"    2.00      "+2+4+6")
   (make-element     17    "At"    2.20      "-1+1+3+5+7")
   (make-element     18    "Rn"    2.20      "x")
   (make-element     1     "Fr"    0.70      "+1")
   (make-element     2     "Ra"    0.90      "+2")
   (make-element     3     "Ac"    1.10      "+3")
   (make-element     3     "Th"    1.30      "+4")
   (make-element     3     "Pa"    1.50      "+4+5")
   (make-element     3     "U"     1.38      "+3+4+5+6")
   (make-element     3     "Np"    1.36      "+3+4+5+6")
   (make-element     3     "Pu"    1.28      "+3+4+5+6")
   (make-element     3     "Am"    1.30      "+3+4+5+6")
   (make-element     3     "Cm"    1.30      "+3")
   (make-element     3     "Bk"    1.30      "+3+4")
   (make-element     3     "Cf"    1.30      "+3")
   (make-element     3     "Es"    1.30      "+3")
   (make-element     3     "Fm"    1.30      "+3")
   (make-element     3     "Md"    1.30      "+2+3")
   (make-element     3     "No"    1.30      "+2+3")
   (make-element     3     "Lr"    0         "+3")
   (make-element     4     "Rf"    0         "+4")
   (make-element     5     "Db"    0         "+5")
   (make-element     6     "Sg"    0         "+6")
   (make-element     7     "Bh"    0         "+7")
   (make-element     8     "Hs"    0         "+8")
   (make-element     9     "Mt"    0         "+1+3+4+6")
   (make-element     10    "Ds"    0         "+2+4+8")
   (make-element     11    "Rg"    0         "+3")
   (make-element     12    "Cn"    0         "+1+2+4")
   (make-element     13    "Uut"   0         "+1+3")
   (make-element     14    "Fl"    0         "+2")
   (make-element     15    "Uup"   0         "+1+3")
   (make-element     16    "Lv"    0         "+2+4")
   (make-element     17    "Uus"   0         "+1+3")
   (make-element     18    "Uuo"   0         "+2+4")
   (make-element     1     "Uue"   0         "x")
   (make-element     2     "Ubn"   0         "x")
  ))   

; Given a List<element> and an element, return true
; if the element-symbol of the element is contained
; in the list<element>.
; exist-element? : List<element> String -> Boolean
(define (exist-element? list el)
  (cond[(empty? list) #false]
       [else (if (string=? el (element-symbol (first list)))
                 #true
                 (exist-element? (rest list) el))]))

(check-expect (exist-element? periodic-table "F") #true)
(check-expect (exist-element? periodic-table "Cer") #false)

; Given a List<element> and a List<given> and a boolean state, return true
; if all the elements of List<given exist in the List<element>.
; exist-elements? : List<element> List<given> Boolean -> Boolean
(define (exist-elements? list list2 state)
  (cond[(empty? list) state]
       [else (if (and (exist-element? list2 (given-symbol (first list))) state)
                    (exist-elements? (rest list) list2 #true)
                    (exist-elements? (rest list) list2 #false))]))

(check-expect (exist-elements? (list (make-given "F" 5) (make-given "Br" 1)) periodic-table #true) #true)
(check-expect (exist-elements? (list (make-given "FR" 5) (make-given "Br" 1)) periodic-table #true) #false)
(check-expect (exist-elements? (list (make-given "CR" 5) (make-given "Cn" 1)) periodic-table #true) #false)

; Given a List<element> and a element, return the value
; of the element-electroneg of that element.
; get-electronegativity : List<element> String -> Real
(define (get-electronegativity list el)
   (cond[(string=? el (element-symbol (first list)))
         (element-electroneg (first list))]
        [else (get-electronegativity (rest list) el)]))

(check-within (get-electronegativity periodic-table "H") 2.20 2.20)
(check-within (get-electronegativity periodic-table "B") 2.04 2.04)

; Given a List<given> and a Delta, return the absolute value of the
; difference between the electronegativity of the 
; elements of the list.
; delta-electronegativity : List<given> Number -> Real
(define (delta-electronegativity list Delta)
  (cond[(= (length list) 1) 0]
       [(empty? list)  Delta]
       [else (delta-electronegativity (rest list) (abs (- Delta (get-electronegativity periodic-table (given-symbol (first list))))))]))

(check-within (delta-electronegativity (list (make-given "F" 5) (make-given "Br" 1)) 0) 1.02 1.02)
(check-expect (delta-electronegativity (list (make-given "F" 1)) 0) 0)

; Given a List<empty> and a List<string>
; a string about the symbol of the element
; and an integer set firstly to 1
; referred to the element where the number is unspecified
; return a List<given>.
; el-given-list : List<string> List<empty> String Integer -> List<given>
(define (el-given-list list list2 el num)
  (cond [(empty? list)
         (if (= num 1)
             (cons (make-given el num) list2)
             (cons (make-given el (- num (expt 10 (- (string-length (number->string num)) 1)))) list2))]
        [(number? (string->number (string(first list))))
         (el-given-list (rest list) list2 el (string->number (string-append (number->string num) (string(first list)))))]
        [(not (number? (string->number (string(first list)))))
         (cond [(and (string=? (string(first list)) (string-upcase (string(first list)))) (>= (string-length (string-append el (string(first list)))) 2))
                (cond [(= (string-length (number->string num)) 1)
                       (el-given-list (rest list) (cons (make-given el 1) list2) (string(first list)) 1)]
                      [(>= (string-length (number->string num)) 2)
                       (el-given-list (rest list) (cons (make-given el (- num (expt 10 (- (string-length (number->string num)) 1)))) list2) (string(first list)) 1)])]
               [(and (string=? (string(first list)) (string-downcase (string(first list)))) (= (string-length (string-append el (string(first list)))) 2))
                 (el-given-list (rest list) list2 (string-append el (string(first list))) num)]
               [else (el-given-list (rest list) list2 (string(first list)) num)]
             )]))

(check-expect (el-given-list (string->list "H2O") '() "" 1) (list (make-given "O" 1) (make-given "H" 2)))
(check-expect (el-given-list (string->list "SF4") '() "" 1) (list (make-given "F" 4) (make-given "S" 1)))
(check-expect (el-given-list (string->list "BrF5") '() "" 1) (list (make-given "F" 5) (make-given "Br" 1)))
(check-expect (el-given-list (string->list "Br2F5") '() "" 1) (list (make-given "F" 5) (make-given "Br" 2)))
(check-expect (el-given-list (string->list "Br2F5H2") '() "" 1) (list (make-given "H" 2) (make-given "F" 5) (make-given "Br" 2)))
(check-expect (el-given-list (string->list "C6H12O6") '() "" 1) (list (make-given "O" 6) (make-given "H" 12) (make-given "C" 6)))
(check-expect (el-given-list (string->list "C6H22O69999") '() "" 1) (list (make-given "O" 69999) (make-given "H" 22) (make-given "C" 6)))

; Given a List<element> and an the symbol of an element, return a string of
; the element-oxidation about the element if it exists.
; get-oxidation : List<element> String -> String
(define (get-oxidation list el)
  (cond[(empty? list) ""]
       [else (if (string=? el (element-symbol (first list)))
                 (element-oxidation (first list))
                 (get-oxidation (rest list) el))]))

(check-expect (get-oxidation periodic-table "H")  "+1-1")
(check-expect (get-oxidation periodic-table "N")  "-3+2+3+4+5")
(check-expect (get-oxidation periodic-table "")  "")

; Given a List<string> and a List<empty> and an empty string, return a List<Number> of the
; numbers of oxidation of the element.
; list-oxidation : List<string> List<empty> string -> List<Number>
(define (list-oxidation givenl createl operator)
   (cond[(empty? givenl) createl]
        [(not (number? (string->number (string(first givenl)))))
         (list-oxidation (rest givenl) createl (string(first givenl)))]
        [(number? (string->number (string(first givenl))))
         (if (string=? operator "-")
             (list-oxidation (rest givenl) (cons (- 0 (string->number (string(first givenl)))) createl) (string(first givenl))) 
             (list-oxidation (rest givenl) (cons (string->number (string(first givenl))) createl) (string(first givenl)))) ]))

(check-expect (list-oxidation (string->list "-2+4+6") '() "") (list 6 4 -2))
(check-expect (list-oxidation (string->list "+2+3-3+4+5") '() "") (list 5 4 -3 3 2))

; Given a List<Number>, the number of element and the number of oxidation,
; return true if the oxidation (difference) between at least one element of List<Number> and the element
; given is stable (= 0)
; stable? : List<Number> Integer Integer -> Boolean
(define (stable? list n el)
  (cond [(empty? list) #false]
        [else (if (= (+ (* (first list) n) el) 0)
                  #true
                  (stable? (rest list) n el))]))

(check-expect (stable? (list 6 4 -2) 1 -6) #true)
(check-expect (stable? (list 6 4 -2) 2 -6) #false)

; Given two List<Number> about the oxidation of two element
; and the given-n number of the List<given>, return true if 
; the condition stable? is true.
; ox-stable? : List<Number> Integer List<Number> Integer -> Boolean
(define (ox-stable? oxl1 n1 oxl2 n2)
  (cond [(empty? oxl1) #false]
        [else(if(stable? oxl2 n2 (* (first oxl1) n1))
                #true
                (ox-stable? (rest oxl1) n1 oxl2 n2))]))

(check-expect (ox-stable? (list -2 4 6) 1 (list -1) 6) #true)
(check-expect (ox-stable? (list 2 4 -4) 1 (list -1 +1) 1) #false)
(check-expect (ox-stable? (list 2 4 -4) 1 (list -1 +1) 4) #true)

; Given List<element> and List<given>, return true if the oxidation
;between the elements of List<given> is stable.
; check-oxidation :  List<element>  List<given> -> Boolean 
(define (check-oxidation list1 list2)
  (cond[(and (= (length list2) 2)
             (ox-stable? (list-oxidation (string->list (get-oxidation list1 (given-symbol (first list2)))) '() "")  (given-n (first list2))
                         (list-oxidation (string->list (get-oxidation list1 (given-symbol (second list2)))) '() "") (given-n (second list2))))
        #true]
       [(= (length list2) 1) 
        #true]
       [else #false]))

(check-expect (check-oxidation periodic-table  (list (make-given "S" 1) (make-given "F" 6))) #true)
(check-expect (check-oxidation periodic-table  (list (make-given "C" 1) (make-given "H" 1))) #false)
(check-expect (check-oxidation periodic-table  (list (make-given "C" 1) (make-given "H" 4))) #true)

; Given a List<given>, return the value of how
; many elements are connected to the element with less atoms.
; n-molecule : List<given> -> Number
(define (n-molecule list)
  (cond
    [(= (length list) 1)
     (cond [(= (given-n (first list)) 1) 0]
           [(= (given-n (first list)) 2) 1]
           [(> (given-n (first list)) 2) (- (given-n (first list)) 1)])]
    [(= (length list) 2)
     (if (> (given-n (first list)) (given-n (second list)))
         (/ (given-n (first list)) (given-n (second list)))
         (/ (given-n (second list)) (given-n (first list))))]))

(check-expect (n-molecule (list (make-given "S" 1) )) 0)
(check-expect (n-molecule (list (make-given "S" 1) (make-given "F" 6))) 6)
(check-expect (n-molecule (list (make-given "O" 1) (make-given "H" 2))) 2)
(check-expect (n-molecule (list (make-given "N" 2) (make-given "H" 4))) 2)

; Given a List<element> and a string about the symbol of the element,
; return the number of the gruop where the element is contained.
; If the element element is  not a metal of transistion (number of the group - 10 > 3 and
; the number gruop is not 1 or 2) return (number of the group - 10) that is referred
; to the element with group numbered with roman numbers (gruop 1 -> group I)
; find-group : List<element> String -> Number
(define (find-group list el)
  (if (string=? el (element-symbol (first list))) 
      (if (and (not(< (- (element-group (first list)) 10) 3))
               (not (= (element-group (first list)) 1))
               (not (= (element-group (first list)) 2)))
          (- (element-group (first list)) 10)
          (element-group (first list)))
      (find-group (rest list) el)))

(check-expect (find-group periodic-table "C") 4)
(check-expect (find-group periodic-table "O") 6)
(check-expect (find-group periodic-table "H") 1)
(check-expect (find-group periodic-table "Ga") 3)
(check-expect (find-group periodic-table "Ag") 11)

; Given a List<element> and a string about the symbol of the element,
; return true if the element group number is referred to
; the element with group numbered with roman numbers (gruop 1 -> group I)
; find-group : List<element> String -> Boolean
(define (correct-group? list el)
  (cond[(empty? list) #false]
       [else (if (and (string=? el (element-symbol (first list))) 
                      (or (not (< (- (element-group (first list)) 10) 3))
                          (= (element-group (first list)) 1)
                          (= (element-group (first list)) 2)))
                 #true
                 (correct-group? (rest list) el))]))

(check-expect (correct-group? periodic-table "F") #true)
(check-expect (correct-group? periodic-table "Ag") #f)
(check-expect (correct-group? periodic-table "Be") #t)

; Given  List<given> and List<element> and a boolean state, return true
; if the correct-group? of all the elements are true.
; correct-groups? : List<given> List<element> Boolean -> Boolean
(define (correct-groups? list list2 state)
  (cond[(empty? list) state]
       [else (if (and (correct-group? list2 (given-symbol (first list))) state)
                    (correct-groups? (rest list) list2 #true)
                    (correct-groups? (rest list) list2 #false))]))

(check-expect (correct-groups? (list (make-given "F" 5) (make-given "Hg" 1)) periodic-table #true) #f)
(check-expect (correct-groups? (list (make-given "Ag" 2) (make-given "O" 1)) periodic-table #true) #f)

; Given a number, return the value of the group
; subtracted by 8.
; how-el-need : Number -> Number
(define (how-el-need group)
  (- 8 group))

(check-expect (how-el-need 4) 4)
(check-expect (how-el-need 1) 7)
(check-expect (how-el-need 3) 5)

;Given two Structs about the element given,
;return how many  element
;remain to complete the Octet rule (Lewis Structure) for element
;of the group III, IV, V ,VI, VII, VIII.
;el-rim-other-groups : Struct Struct -> Number
(define (el-rim-other-groups el1 el2)
  (- (find-group periodic-table (given-symbol el1)) (* (how-el-need (find-group periodic-table (given-symbol el2))) (given-n el2))))

;Given two Structs about the element given,
;return how many  element
;remain to complete the Octet rule (Lewis Structure) for element
;of the group I, II.
;el-rim-group1-2 : Struct Struct -> Number
(define (el-rim-group1-2 el1 el2)
  (abs (- (* (find-group periodic-table (given-symbol el1)) (given-n el1)) (* (find-group periodic-table (given-symbol el2)) (given-n el2)))))

; Given two Struct, return how many  element
; remain to complete the Octet rule (Lewis Structure). 
; el-rim : Struct Struct -> Number
(define (el-rim el1 el2)
  (if (or (= (find-group periodic-table (given-symbol el1)) 1)  (= (find-group periodic-table (given-symbol el2)) 1))
      (if (or (= (modulo (el-rim-group1-2 el1 el2) 2) 0) (= (el-rim-group1-2 el1 el2) 1))
          (el-rim-group1-2 el1 el2)
          (+(el-rim-group1-2 el1 el2) 1))
      (el-rim-other-groups el1 el2)))

(check-expect  (el-rim (make-given "C" 1) (make-given "O" 1)) 2)
(check-expect  (el-rim (make-given "C" 1) (make-given "O" 2)) 0)
(check-expect  (el-rim (make-given "O" 1) (make-given "C" 1)) 2)
(check-expect  (el-rim (make-given "S" 1) (make-given "O" 2)) 2)
(check-expect  (el-rim (make-given "S" 1) (make-given "F" 4)) 2)
(check-expect  (el-rim (make-given "Cl" 1) (make-given "F" 3)) 4)
(check-expect  (el-rim (make-given "N" 1) (make-given "H" 4)) 1)
(check-expect  (el-rim (make-given "N" 1) (make-given "H" 2)) 4)
(check-expect  (el-rim (make-given "H" 2) (make-given "O" 1)) 4)
(check-expect  (el-rim (make-given "S" 1) (make-given "F" 6)) 0)
(check-expect  (el-rim (make-given "F" 6) (make-given "S" 1)) 5)
(check-expect  (el-rim (make-given "N" 1) (make-given "N" 1)) 2)
(check-expect  (el-rim (make-given "O" 1) (make-given "O" 1)) 4)
(check-expect  (el-rim (make-given "C" 1) (make-given "H" 2)) 2)

; Given a List<given> , return how many  element
; remain to complete the Octet rule (Lewis Structure).
;If the list is composed by only one element return 0.
;If the list is composed by 2 element and the el-rim of that element
;is not even and not equal to 1 return the el-rim divided by 2 (number of lone pairs).
;Otherwise return 0 to indicate that is not possible to create lone pairs.
; el-rim2 : List<given> -> Number
(define (el-rim2 list)
   (cond
    [(= (length list) 1) 0]
    [(= (length list) 2)
     (if (or (not(= (modulo (el-rim (first list) (second list)) 2) 0)) (= (el-rim (first list) (second list)) 1))
         0
         (/ (el-rim (first list) (second list)) 2))]))

(check-expect  (el-rim2 (list(make-given "C" 1) (make-given "O" 1))) 1)
(check-expect  (el-rim2 (list(make-given "O" 1))) 0)
(check-expect  (el-rim2 (list(make-given "S" 1) (make-given "F" 4))) 1)
(check-expect  (el-rim2 (list(make-given "N" 1) (make-given "H" 4))) 0)
(check-expect  (el-rim2 (list(make-given "N" 1) (make-given "H" 2))) 2)
(check-expect  (el-rim2 (list(make-given "C" 1) (make-given "O" 2))) 0)
(check-expect  (el-rim2 (list(make-given "Ca" 1) (make-given "Cl" 2))) 0)
(check-expect  (el-rim2 (list(make-given "N" 2))) 0)
(check-expect  (el-rim2 (list(make-given "O" 2))) 0)
(time (el-rim2 (list (make-given "C" 2) (make-given "H" 4))))

; Given a List<given> and a string about the symbol of the element,
; return true if the group of the element is 1.
; group1? : List<given> String -> Boolean
(define (group1? list el)
  (cond [(empty? list) #false]
        [else (if (and (string=? (element-symbol (first list)) el)
                       (= (element-group(first list)) 1))
                  #true
                  (group1? (rest list) el))]))

(check-expect (group1? periodic-table "H") #true)
(check-expect (group1? periodic-table "O") #f)

; Given a  List<element> List<given> ,
; return true if the group of at least one of the elements is 1.
; group1s? : List<element> List<given> -> Boolean
(define (group1s? list1 list2)
  (cond [(empty? list2) #false]
        [else (if (group1? list1 (given-symbol (first list2)))
                  #true
                  (group1s? list1 (rest list2)))]))

(check-expect (group1s? periodic-table (list (make-given "C" 1) (make-given "O" 1))) #false)
(check-expect (group1s? periodic-table (list (make-given "Na" 1) (make-given "Cl" 1))) #true)
(check-expect (group1s? periodic-table (list (make-given "C" 1) (make-given "H" 4))) #true)

; Given a List<given> and a string about the symbol of the element,
; return true if the group of the element is 2.
; group2? : List<given> String -> Boolean
(define (group2? list el)
  (cond [(empty? list) #false]
        [else (if (and (string=? (element-symbol (first list)) el)
                       (= (element-group(first list)) 2))
                  #true
                  (group2? (rest list) el))]))

(check-expect (group2? periodic-table "Be") #true)
(check-expect (group2? periodic-table "Ca") #true)
(check-expect (group2? periodic-table "N") #false)

; Given a  List<element> List<given> ,
; return true if the group of at least one of the elements is 2.
; group2s? : List<element> List<given> -> Boolean
(define (group2s? list1 list2)
  (cond [(empty? list2) #false]
        [else (if (group2? list1 (given-symbol (first list2)))
                  #true
                  (group2s? list1 (rest list2)))]))

(check-expect (group2s? periodic-table (list (make-given "C" 1) (make-given "O" 1))) #false)
(check-expect (group2s? periodic-table (list (make-given "Be" 1) (make-given "Cl" 1))) #true)
(check-expect (group2s? periodic-table (list (make-given "Ca" 1) (make-given "Cl" 2))) #true)

; Given a List<given>, return 1 or 2 or 3 depending to indicate the kind of bond
; between two molecules of the same type.
; compute-bond1 : List<given> -> Number
(define (compute-bond1 list)
   (/ (/ (- (* (find-group periodic-table (given-symbol (first list))) (given-n (first list)))
           (* (el-rim (first list) (first list)) (given-n (first list)))) (given-n (first list))) 2))

(check-expect  (compute-bond1 (list (make-given "N" 2))) 3)
(check-expect  (compute-bond1 (list (make-given "O" 2))) 2)
(check-expect  (compute-bond1 (list (make-given "O" 3))) 3)
(time (compute-bond1 (list (make-given "C" 1))))

; Given a List<given>, return 1 or 2 or 3 depending to indicate the kind of bond
; between two molecules of the different type.
; compute-bond2 : List<given> -> Number
(define (compute-bond2 list)
  (round (/ (/ (- (+ (find-group periodic-table (given-symbol (first list))) (find-group periodic-table (given-symbol (second list))))
                     (+ (el-rim (first list) (second list)) (el-rim (second list) (first list)))) 2) (n-molecule list))))

(check-expect  (/ (compute-bond2 (list (make-given "Ca" 1) (make-given "Cl" 2))) 2) 1)
(check-expect  (compute-bond2 (list (make-given "C" 1) (make-given "O" 2))) 2)
(check-expect  (/ (compute-bond2 (list (make-given "Sr" 1) (make-given "O" 1))) 2) 2)
(check-expect  (compute-bond2 (list (make-given "C" 1) (make-given "O" 1))) 3)
(time (compute-bond2 (list (make-given "C" 1) (make-given "O" 2))))

; Given a List<given>, return 1 or 2 or 3 depending to indicate the kind of bond
; between two molecules.
; if the number of molecules is 0 return 0
; if there is only one element in the List<given> of the group 1 and the given-n >2 ,
; return an even number >3 (error bond)
; otherwise return 1.
; if there is only one element in the List<given> of the group 1 and the given-n >2 ,
; return an even number >3 (error bond)
; otherwise if the the lenght of List<given> is 1 return 2 else compute bond / 2.
;if the the lenght of List<given> is 1 compute bond 1
;if the the lenght of List<given> is 2 compute bond 2
; kind-of-bond : List<given> -> Number
(define (kind-of-bond list)
  (cond
   [(= (n-molecule list) 0) 0]
   [(group1s? periodic-table list)
    (if (and (= (length list) 1) (> (given-n (first list)) 2)) 5  1)]
   [(group2s? periodic-table list) 
        (if (and (= (length list) 1) (> (given-n (first list)) 2)) 5
            (if (= (length list) 1) 2
                (/ (compute-bond2 list) 2)))]
   [(= (length list) 1) (compute-bond1 list)]
   [(= (length list) 2) (compute-bond2 list)]))

(check-expect  (kind-of-bond (list (make-given "N" 1))) 0)
(check-expect  (kind-of-bond (list (make-given "N" 2))) 3)
(check-expect  (kind-of-bond (list (make-given "O" 2))) 2)
(check-expect  (kind-of-bond (list (make-given "O" 3))) 3)
(check-expect  (kind-of-bond (list (make-given "N" 1) (make-given "H" 4))) 1)
(check-expect  (kind-of-bond (list (make-given "Ca" 1) (make-given "Cl" 2))) 1)
(check-expect  (kind-of-bond (list (make-given "C" 1) (make-given "O" 2))) 2)
(check-expect  (kind-of-bond (list (make-given "Sr" 1) (make-given "O" 1))) 2)
(check-expect  (kind-of-bond (list (make-given "Mg" 1) (make-given "O" 1))) 2)
(check-expect  (kind-of-bond (list (make-given "Na" 1) (make-given "Cl" 1))) 1)
(check-expect  (kind-of-bond (list (make-given "Be" 2))) 2)
(check-expect  (kind-of-bond (list (make-given "C" 1) (make-given "O" 1))) 3)
(check-expect  (kind-of-bond (list (make-given "N" 2) (make-given "H" 4))) 1)
(check-expect  (kind-of-bond (list (make-given "C" 2) (make-given "H" 4))) 1)
(check-expect  (kind-of-bond (list (make-given "C" 2))) 4)
(time (kind-of-bond (list (make-given "C" 2) (make-given "H" 4))))

; Given a List<given>, return true if
; in the List<given> with lenght 1 the given-n is not > 2 (O3 can't be represented)
; and if in the list of two element the less or equal given-n is not > 2.
; limitation : List<given> -> Boolean
(define (limitation list)
  (cond
    [(and (= (length list) 1) (> (given-n (first list)) 2)) #f]
    [(= (length list) 2)
     (if (or
          (and (<= (given-n (first list)) (given-n (second list))) (< (given-n (first list)) 2))
          (and (<= (given-n (second list)) (given-n (first list))) (< (given-n (second list)) 2)))
         #t
         #f)]
    [else #t]))

(check-expect (limitation (list (make-given "F" 5) (make-given "Br" 1))) #true)
(check-expect (limitation (list (make-given "F" 5) (make-given "Br" 2))) #false)
(check-expect (limitation (list (make-given "H" 2) (make-given "N" 1))) #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Built the 3D moluecule
;VSEPR model is used in chemistry to predict the geometry of individual molecules

;color1 is a random color
(define color1 (make-color (random 255) (random 255) (random 255)))

;color2 is a random color
(define color2 (make-color (random 255) (random 255) (random 255)))

;list2 is a List<param> which contains the parameters
;to built a molecule,containing 2 atoms of the same type,following the VSEPR model, into a 3d scene
(define list1 (list (make-param (make-posit 0 0 0 0 0 0) (make-degrees 0 0 0) color2)
                    (make-param (make-posit 0 0 2 0 0 0) (make-degrees 0 0 0) color2)))

;list2 is a List<param> which contains the parameters
;to built a molecule,containing 3 atoms (2 same, 1 different),following the VSEPR model, into a 3d scene
(define list2 (list (make-param (make-posit 0 0 0 0 0 0) (make-degrees 0 0 0) color2)
                    (make-param (make-posit 0 0 2 0 0 0) (make-degrees 0 180 0) color1)
                    (make-param (make-posit 0 0 2 0 180 0) (make-degrees 0 180 0) color1)))

;list3 is a List<param> which contains the parameters
;to built a molecule,containing 4 atoms (3 same, 1 different),following the VSEPR model, into a 3d scene
(define list3 (list (make-param (make-posit 0 0 0 0 0 0) (make-degrees 0 0 0) color2)
                    (make-param (make-posit 0 0 2 0 0 0) (make-degrees 240 0 0) color1)
                    (make-param (make-posit 0 0 2 240 0 0) (make-degrees 120 0 0) color1)
                    (make-param (make-posit 0 0 2 120 0 0) (make-degrees 120 0 0) color1)))

;list4 is a List<param> which contains the parameters
;to built a molecule,containing 5 atoms (4 same, 1 different),following the VSEPR model, into a 3d scene
(define list4 (list (make-param (make-posit 0 0 0 0 0 0) (make-degrees 0 0 0) color2)
                    (make-param (make-posit 0 0 2 0 0 0) (make-degrees 0 240 0) color1)
                    (make-param (make-posit 0 0 2 0 240 0) (make-degrees 0 120 60) color1)
                    (make-param (make-posit 0 0 2 0 120 60) (make-degrees 0 120 -60) color1)
                    (make-param (make-posit 0 0 2 0 120 -60) (make-degrees 0 120 -60) color1)))

;list5 is a List<param> which contains the parameters
;to built a molecule,containing 6 atoms (5 same, 1 different),following the VSEPR model, into a 3d scene
(define list5 (list (make-param (make-posit 0 0 0 0 0 0) (make-degrees 0 0 0) color2)
                    (make-param (make-posit 0 0 2 0 0 0) (make-degrees 0 180 0) color1)
                    (make-param (make-posit 0 0 2 0 180 0) (make-degrees 0 -90 0) color1)
                    (make-param (make-posit 0 0 2 0 -90 0) (make-degrees 0 -90 -120) color1)
                    (make-param (make-posit 0 0 2 0 -90 -120) (make-degrees 0 -90 -240) color1)
                    (make-param (make-posit 0 0 2 0 -90 -240) (make-degrees 0 -90 -240) color1)))

;list6 is a List<param> which contains the parameters
;to built a molecule,containing 7 atoms (6 same, 1 different),following the VSEPR model, into a 3d scene
(define list6 (list (make-param (make-posit 0 0 0 0 0 0) (make-degrees 0 0 0) color2)
                    (make-param (make-posit 0 0 2 0 0 0) (make-degrees 0 180 0) color1)
                    (make-param (make-posit 0 0 2 0 180 0) (make-degrees 0 -90 0) color1)
                    (make-param (make-posit 0 0 2 0 -90 0) (make-degrees 0 90 0) color1)
                    (make-param (make-posit 0 0 2 0 90 0) (make-degrees 90 0 0) color1)
                    (make-param (make-posit 0 0 2 90 0 0) (make-degrees -90 0 0) color1)
                    (make-param (make-posit 0 0 2 -90 0 0) (make-degrees -90 0 0) color1)))

;list7 is a List<param> which contains the parameters
;to built a molecule,containing 8 atoms (7 same, 1 different),following the VSEPR model, into a 3d scene
(define list7 (list (make-param (make-posit 0 0 0 0 0 0) (make-degrees 0 0 0) color2)
                    (make-param (make-posit 0 0 2 0 0 0) (make-degrees 0 180 0) color1)
                    (make-param (make-posit 0 0 2 0 180 0) (make-degrees 0 -90 0) color1)
                    (make-param (make-posit 0 0 2 0 -90 0) (make-degrees 0 -90 -72) color1)
                    (make-param (make-posit 0 0 2 0 -90 -72) (make-degrees 0 -90 -144) color1)
                    (make-param (make-posit 0 0 2 0 -90 -144) (make-degrees 0 -90 -216) color1)
                    (make-param (make-posit 0 0 2 0 -90 -216) (make-degrees 0 -90 -288) color1)
                    (make-param (make-posit 0 0 2 0 -90 -288) (make-degrees 0 -90 -288) color1)))

;Given a struct degrees and an integer about the radius of the sphere in which the
;molecule is contained represent the 3d model of the molecule
(define (light-camera degrees radius)
  (combine
   (rotate-z (rotate-x (rotate-y (light (pos 3 3 3) (emitted "white" 6))(degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees))
   (rotate-z (rotate-x (rotate-y  (light (pos -3 -3 3) (emitted "white" 6))(degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees))
   (with-color (rgba "white" 0)(sphere origin radius))))

;Given a struct degrees represent the 3d model of a single bond 
(define (1bond degrees)
  (combine
   (rotate-z (rotate-x (rotate-y (cylinder (pos 0 0 1) (dir 1/15 1/15 1)) (degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees))
   (light-camera degrees 2.5)))

;Given a struct degrees represent the 3d model of a double bond 
(define (2bond degrees)
  (combine
   (rotate-z (rotate-x (rotate-y (cylinder (pos 0 0.1 1) (dir 1/15 1/15 1)) (degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees))
   (rotate-z (rotate-x (rotate-y (cylinder (pos 0 -0.1 1) (dir 1/15 1/15 1)) (degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees))
   (light-camera degrees 2.5)))

;Given a struct degrees represent the 3d model of a tiple bond 
(define (3bond degrees)
  (combine
   (rotate-z (rotate-x (rotate-y  (cylinder (pos 0.2 0 1) (dir 1/15 1/15 1)) (degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees))
   (rotate-z (rotate-x (rotate-y (cylinder (pos 0 0 1) (dir 1/15 1/15 1))  (degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees))
   (rotate-z (rotate-x (rotate-y (cylinder (pos -0.2 0 1) (dir 1/15 1/15 1))  (degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees))
   (light-camera degrees 2.5)))

;Given a struct degrees represent the 3d model of a lone pair
(define (lone-pair degrees)
  (rotate-z (rotate-x (rotate-y (combine (with-color (rgba "white" 0.6)(sphere (pos 0 0 0.65) 0.4))
                                         (rotate-z (with-color (rgba "yellow")(sphere (pos 0 1/10 0.65) 1/15)) 45)
                                         (rotate-z (with-color (rgba "yellow")(sphere (pos 0 -1/10 0.65) 1/15)) 45)) (degrees-Y degrees)) (degrees-X degrees)) (degrees-Z degrees)))

;Given the parameters of the VSEPR model configuration ,the kind of bond
;between the atoms ,the number of element to represent
;and the element remaing from the bond return a 3d representation
;of the molecule. If the element to represent are equal to the element
;remaing insteat to show the molecules and the bonds, show the lone pairs
(define (conf param bond dec el-rim)
  (combine
   (cond [(>= dec el-rim)
          (rotate-z
           (rotate-x
            (rotate-y
             (with-color
                 (rgba (param-color param))
               (sphere (pos (posit-X (param-posit param))
                            (posit-Y (param-posit param))
                            (posit-Z (param-posit param))) 1/2))
             (posit-gY (param-posit param)))
            (posit-gX (param-posit param)))
           (posit-gZ (param-posit param)))]
         [else empty-pict3d])
   (cond [(and (> dec el-rim) (= bond 1)) (1bond (param-degrees param))]
         [(and (> dec el-rim) (= bond 2)) (2bond (param-degrees param))]
         [(and (> dec el-rim) (= bond 3)) (3bond (param-degrees param))]
         [(and (not (= el-rim 0)) (<= dec el-rim) (send LonePairs get-value)) (lone-pair (param-degrees param))]
         [else empty-pict3d])))

;Given the parameters of the VSEPR model configuration the kind of bond
;between the atoms ,the number of atoms to represent
;and the element remaing from the bond return a 3d representation
;of the molecule. 
(define (draw3d list 3d bond dec el-rim)
  (cond [(empty? list) 3d]
        [else (draw3d (rest list) (combine (conf (first list) bond dec el-rim) 3d) bond (sub1 dec) el-rim)]))

;Given the List<given> return a 3d representation
;of the molecule. From the list we can get the number of molecule and the lone pairs(el-rim) and
;the choose what configuration we want to represent into the 3d scene
(define (molecule list)
  (cond
    [(= (+ (n-molecule list) (el-rim2 (reverse list))) 0)
     (combine (light-camera (make-degrees 0 0 0) 1.2)
              (draw3d list1 empty-pict3d (kind-of-bond list) (+ (n-molecule list) (el-rim2 (reverse list))) (el-rim2 (reverse list))))]
    [(= (+ (n-molecule list) (el-rim2 (reverse list))) 1) (draw3d list1 empty-pict3d (kind-of-bond list) (+ (n-molecule list) (el-rim2 (reverse list))) (el-rim2 (reverse list)))]
    [(= (+ (n-molecule list) (el-rim2 (reverse list))) 2) (draw3d list2 empty-pict3d (kind-of-bond list) (+ (n-molecule list) (el-rim2 (reverse list))) (el-rim2 (reverse list)))]
    [(= (+ (n-molecule list) (el-rim2 (reverse list))) 3) (draw3d list3 empty-pict3d (kind-of-bond list) (+ (n-molecule list) (el-rim2 (reverse list))) (el-rim2 (reverse list)))]       
    [(= (+ (n-molecule list) (el-rim2 (reverse list))) 4) (draw3d list4 empty-pict3d (kind-of-bond list) (+ (n-molecule list) (el-rim2 (reverse list))) (el-rim2 (reverse list)))]
    [(= (+ (n-molecule list) (el-rim2 (reverse list))) 5) (draw3d list5 empty-pict3d (kind-of-bond list) (+ (n-molecule list) (el-rim2 (reverse list))) (el-rim2 (reverse list)))]
    [(= (+ (n-molecule list) (el-rim2 (reverse list))) 6) (draw3d list6 empty-pict3d (kind-of-bond list) (+ (n-molecule list) (el-rim2 (reverse list))) (el-rim2 (reverse list)))]
    [(= (+ (n-molecule list) (el-rim2 (reverse list))) 7) (draw3d list7 empty-pict3d (kind-of-bond list) (+ (n-molecule list) (el-rim2 (reverse list))) (el-rim2 (reverse list)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GUI

;Main window to store the GUI
(define mainWindow (new frame% 
                        [label "Molecular Simulator"]
                        [width 800]
                        [height 480]
                        [style (list 'no-resize-border)]))

;Main content panel
(define mainCont (new horizontal-panel%
                      [vert-margin 5]
                      [horiz-margin 5]
                      [parent mainWindow]))

;Left Panel is contained into manCont panel to show
;the the texbox in which the user insert the formula
;the button to convert the forumla
;the check box for the lone pairs
;and the buttons to rotate the image
(define leftPanel (new vertical-panel% 
                        [parent mainCont]))

;formulaPanel is containe into Left Panel to show
;the the texbox in which the user insert the formula
;the button to convert the forumla
(define formulaPanel (new vertical-panel% 
                        [parent leftPanel]))

;check-boxPanel is containe into Left Panel to show
;the check box for the lone pairs
;and the buttons to rotate the image
(define check-boxPanel (new vertical-panel% 
                        [parent leftPanel]))

;These panel show the buttons to rotate the image
(define sliderPanel (new vertical-panel% [horiz-margin 20] [parent leftPanel] ))
(define sliderPanel-Message (new horizontal-panel% [horiz-margin 20] [parent sliderPanel] ))
(define sliderPanel-X (new horizontal-panel% [horiz-margin 20] [parent sliderPanel] ))
(define sliderPanel-Y (new horizontal-panel% [horiz-margin 20] [parent sliderPanel] ))

;A message box containing the information "Insert the Formula:" 
(define label (new message%
                     (parent formulaPanel)
                     (label "Insert the Formula:")))

;The text-field where the user insert the formula
(define text-field (new text-field%
                        [label ""]
                        [vert-margin 5]
                        [horiz-margin 30]
                        [parent formulaPanel]
                        [init-value ""]
                        [callback (lambda (text event)
                                    molecule)]))

;Procedure to retrieve text from a text-field. 
(define myGetText
  (lambda (thisText)
    (send (send thisText get-editor) get-text)))

;A message box containing the error messages
(define message (new message%
                     [parent formulaPanel]
                     [enabled #t]
                     [label ""]
                     [auto-resize #t]))

;Given a List<given> print the possible errors about the text written inside the
;text-field
(define (print-error list)
  (cond [(string=? (given-symbol (first list)) "") (send message set-label "Error: Empty TextField")]
        [(not (exist-elements? list periodic-table #true)) (send message set-label "Error: Element not found")]
        [(not(correct-groups? list periodic-table #true)) (send message set-label "Error: Wrong Group")]
        [(not (or (>= (delta-electronegativity list 0) 0)
                  (<= (delta-electronegativity list 0) 1.7))) (send message set-label "Error: Not covalent bond")]
        [(number? (string->number (string(first (string->list (myGetText text-field)))))) (send message set-label "Error: First element is a number ")]
        [(> (length list) 2) (send message set-label "Error: Max 2 elements")]
        [(not(check-oxidation periodic-table list)) (send message set-label "Error: Oxidation instable")]
        [(>= (kind-of-bond list) 4) (send message set-label "Error: Error Bond")]
        [(not(limitation list)) (send message set-label "Error: This formula can't be represented")]
        [else (send message set-label "Error")]))

;Given a string update the GUI only if the condition of correct syntax and bond are true.
;If the syntax of the formula is correct print the 3d image otherwise set the 3d scene to empty.
(define (gui-update input-text)
  (cond [(and (not(string=? input-text ""))
              (exist-elements? (el-given-list (string->list input-text) '() "" 1) periodic-table #true)
              (correct-groups? (el-given-list (string->list input-text) '() "" 1) periodic-table #true)
              (or (>= (delta-electronegativity (el-given-list (string->list input-text) '() "" 1) 0) 0)
                  (<= (delta-electronegativity (el-given-list (string->list input-text) '() "" 1) 0) 1.7))
              (not (number? (string->number (string(first (string->list input-text))))))
              (<= (length (el-given-list (string->list input-text) '() "" 1)) 2)
              (check-oxidation periodic-table (el-given-list (string->list input-text) '() "" 1))
              (< (kind-of-bond (el-given-list (string->list input-text) '() "" 1)) 4)
              (limitation (el-given-list (string->list input-text) '() "" 1))
              )
         (begin (send message set-label "Formula Updated " )
                (send pict set-pict3d (rotate-x
                                       (rotate-y
                                        (molecule (el-given-list (string->list input-text) '() "" 1))
                                        (send rotateY get-value))
                                       (send rotateX get-value))))]
        [else (begin (print-error (el-given-list (string->list input-text) '() "" 1))
                     (send pict set-pict3d  empty-pict3d))]))

;formula-button is a Button to activate the conversion of the formula given inside the
;text-field
(define formula-button (new button%
                            [parent formulaPanel]
                            [label "Convert Formula"]
                            [callback (lambda (button event)
                                        (begin
                                          (gui-update (myGetText text-field))
                                          (time (gui-update (myGetText text-field)))))]))

;A message-box containing an example of a formula that can be represent
(define formula-example (new message%
                             [parent formulaPanel]
                             [enabled #t]
                             [vert-margin 8]
                             [label "Formula Example: SO2"]
                             [auto-resize #t]))

;A check-box that if selected activate the representation of the lone pairs
;on the mulecule
(define LonePairs (new check-box%
                       [parent check-boxPanel]
                       [label "Show Lone Pairs"]
                       [value #f]
                       [callback (lambda (check-box e)
                                   (gui-update (myGetText text-field)))]))

;A message box which contains the message"Rotate Image"
(define label3 (new message%
                    [parent sliderPanel-Message]
                    [label "Rotate Image"]
                    [auto-resize #t]))

;Button to rotate the molecule by -20 on Y
(define DOWN-BUTTON-Y (new button%
                           [parent sliderPanel-Y]
                           [label "Move Y -"]
                           [horiz-margin 25]
                           [min-width 150]
                           [callback (lambda (button event)
                                       (cond [(> (- (send rotateY get-value) 20) 0)
                                              (begin (send rotateY set-value (- (send rotateY get-value) 20))
                                                     (gui-update (myGetText text-field)))]))]))

;Button to rotate the molecule by +20 on Y
(define UP-BUTTON-Y (new button%
                         [parent sliderPanel-Y]
                         [label "Move Y +"]
                         [min-width 150]
                         [callback (lambda (button event)
                                     (cond [(< (+ (send rotateY get-value) 20) 360)
                                            (begin (send rotateY set-value (+ (send rotateY get-value) 20))
                                                   (gui-update (myGetText text-field)))]))]))

;Button to rotate the molecule by -20 on X
(define DOWN-BUTTON-X (new button%
                           [parent sliderPanel-X]
                           [label "Move X -"]
                           [horiz-margin 25]
                           [min-width 150]
                           [callback (lambda (button event)
                                       (cond [(> (- (send rotateX get-value) 20) 0)
                                              (begin
                                                (send rotateX set-value (- (send rotateX get-value) 20))
                                                (gui-update (myGetText text-field)))]))]))

;Button to rotate the molecule by +20 on X
(define UP-BUTTON-X (new button%
                         [parent sliderPanel-X]
                         [label "Move X +"]
                         [min-width 150]
                         [callback (lambda (button event)
                                     (cond [(< (+ (send rotateX get-value) 20) 360)
                                            (begin
                                              (send rotateX set-value (+ (send rotateX get-value) 20))
                                              (gui-update (myGetText text-field)))]))]))

;A slider which contains the intervals of degree value in which the molecule can be rotated on Y
(define rotateY (new slider%	 
                     [label "Rotate Y "]
                     [min-value 0]	 
                     [max-value 360]
                     [parent sliderPanel]
                     [style '(horizontal)]
                     ))

;A slider which contains the intervals of degree value in which the molecule can be rotated on X
(define rotateX (new slider%	 
                     [label "Rotate X "]
                     [min-value 0]	 
                     [max-value 360]
                     [style '(horizontal)]
                     [parent sliderPanel]	 
                     ))

;The pict3d canvas which contains the 3d model of the molecule
(define pict (new pict3d-canvas%
                  [min-width 450]
                  [parent mainCont]
                  [pict3d empty-pict3d]
                 ))

;Show element inside the window frame
(send text-field show #t)
(send pict show #t)
(send formulaPanel show #t)
(send sliderPanel show #t)
(send rotateY show #f)
(send rotateX show #f)
(send mainWindow show #t)

(test-format)
(test)