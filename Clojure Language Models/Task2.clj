
;; Problem 1: define a function `absval`
(defn absval [x] (Math/sqrt(Math/pow x 2)))

;; Problem 2: fix the functions `take-square` and `sum-of-squares`
(defn take-square [x](* x x))
(defn sum-of-squares [x y]
(+ (take-square x) (take-square y)))
;; Problem 3: define expressions `exp-13-1`, `exp-13-2`, `exp-13-3`, and `exp-13-4`
(def exp-13-1 '(* 13 1))
(def exp-13-2 '(Math/sqrt 169))
(def exp-13-3 '(Math/abs -13))
(def exp-13-4 '(/ 26 2))
;; Problem 4: define a function `third`
(defn get-3rd[lst] (nth lst 2))

;; Problem 5:
;; [let's import define `sqrt` and `abs` from Java.lang.Math for the example]
(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))
;; define a function `compose` 
(defn compose [f g] (fn [x] (f (g x))))

;; Problem 6: define a function `first-two`
(defn first-two[lst] (list (nth lst 0)(nth lst 1)))

;; Problem 7: define a function `remove-second`
(defn remove-second [lst]
  (concat 
    (take 1 lst)
    (drop (inc 1) lst)))

;; Problem 8: define a function `add-to-end`
(defn add-to-end [lst]
   (keep-indexed #(if (not= %1 1) %2) lst))

;; Problem 9: define a function `reverse`
;;           (Note that this funciton will overwrite a built in function. 
;;            This is okay.)
(defn reverse [lst] (into '() lst))

;; Problem 10: define a function `count-to-1`
(defn count-to-1 [n]  
  (into '() (range 1 (+ 1 n))))

;; Problem 11: define a function `count-to-n`
(defn count-to-n [n]  
  (into (range 1 (+ 1 n))))

;; Problem 12: define a function `get-max`
(defn max [n & lst]
  (cond (empty? lst) n
        (> (first lst) n) (recur (first lst) (rest lst))
        :else (recur n (rest lst))))

;; Problem 13: define a function `greater-than-five?`
(defn larger-than-5 [coll](map (fn [x] (> x 5) ) coll))

;; Problem 14: define a function `concat-three`
(defn concat-three [x y z] (if (empty? x) (if (empty? y) z (cons (first y) (concat-three x (rest y) z))) (cons (first x) (concat-three (rest x) y z))))

;; Problem 15: define a function `sequence-to-power`
(defn sequence-to-power [lst n]
  (if (= n 0)
    '()
    (concat lst (sequence-to-power lst (- n 1)))))

;; Problem 16: define a function `in-L-star?`
(defn in-L-star? [str]
  (if (empty? str)
    true
    (if (prefix? '(a) str)
      (in-L-star? (rest str))
      false)))



