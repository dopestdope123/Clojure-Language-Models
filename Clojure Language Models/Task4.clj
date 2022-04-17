(comment "Make sure that you follow the instructions carefully
          and use the right procedure names, inputs, and outputs")

;;Preliminaries
(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))
(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))

(defn score-categorical [outcome outcomes params]
  (if (empty? params)
    (print "no matching outcome")
    (if (= outcome (first outcomes))
      (first params)
      (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
  (if (empty? lst)
    base
    (f (first lst)
      (list-foldr f base (rest lst)))))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn score-BOW-sentence [sen probabilities]
  (list-foldr
    (fn [word rest-score]
      (+ (log2 (score-categorical word vocabulary probabilities))
        rest-score))
    0
    sen))

(defn score-corpus [corpus probabilities]
  (list-foldr
    (fn [sen rst]
      (+ (score-BOW-sentence sen probabilities) rst))
    0
    corpus))

(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
      (log2
        (apply +
          (map (fn [z] (Math/pow 2 z))
            (map (fn [x] (- x mx)) log-vals)))))))

(def my-corpus '((call me)
        (call ishmael)))

;;Problem 1: define `theta-corpus-joint`
(defn theta-corpus-joint [theta corpus theta-probs]
  (if (= theta (first thetas))
    (+ (score-corpus corpus theta)(log2 (first theta-probs)))
    (+ (score-corpus corpus theta)(log2 (first (rest theta-probs))))))

;;Problem 2: define `compute-marginal`

(defn compute-marginal [corpus theta-probs]
  (logsumexp (list (theta-corpus-joint theta1 corpus theta-probs)
    (theta-corpus-joint theta2 corpus theta-probs))))

;;Problem 3: define `compute-conditional-prob`
(defn compute-conditional-prob [theta corpus theta-probs]
  (- (theta-corpus-joint theta corpus theta-probs)
    (compute-marginal corpus theta-probs)))

;;Problem 4: define `compute-conditional-dist`
(defn compute-conditional-dist [corpus theta-probs]
  (list (compute-conditional-prob theta1 corpus theta-probs)
    (compute-conditional-prob theta2 corpus theta-probs)))

;;Problem 6: define `compute-posterior-predictive`

(defn exponentiate [lst]
  (if (empty? lst)
    '()
    (cons (Math/pow 2 (first lst)) (exponentiate (rest lst)))))

(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
  (let [conditional-dist (exponentiate 
    (compute-conditional-dist observed-corpus theta-probs))]
  (compute-marginal new-corpus conditional-dist)))



;Problem 7: define `sample-BOW-corpus`
(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(defn flip [weight]
  (if (< (rand 1) weight)
      true
      false))

(defn sample-categorical [outcomes params]
  (if (flip (first params))
      (first outcomes)
      (sample-categorical (rest outcomes)
                          (normalize (rest params)))))

(defn sample-BOW-sentence [len probabilities]
    (if (= len 0)
      '()
      (cons (sample-categorical vocabulary probabilities)
        (sample-BOW-sentence (- len 1) probabilities))))

(defn sample-BOW-corpus [theta sent-len corpus-len]
  (repeatedly corpus-len (fn [] (sample-BOW-sentence sent-len theta))))



;;Problem 8
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (let [theta (sample-categorical thetas theta-probs)]
  (list theta (sample-BOW-corpus theta sent-len corpus-len))))


;;Problem 9: define `estimate-corpus-marginal`
(defn get-theta [theta-corpus]
  (first theta-corpus))

(defn get-corpus [theta-corpus]
  (first (rest theta-corpus)))


 (defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
   (repeatedly sample-size (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))

(defn estimate-corpus-marginal [corpus sample-size sent-len 
  corpus-len theta-probs]
  (/ (apply + 
    (let [sample-corpora 
      (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)] 
    (map (fn [c] (if (= c corpus) 1 0)) 
      (map (fn [x] (get-corpus x)) sample-corpora)))) sample-size))

;;Problem 11: define `rejection-sampler`
(defn get-count [obs observation-list count]
  (if (empty? observation-list)
      count
      (if (= obs (first observation-list))
          (get-count obs (rest observation-list) (+ 1 count))
          (get-count obs (rest observation-list) count))))

(defn get-counts [outcomes observation-list]
  (let [count-obs (fn [obs] (get-count obs observation-list 0))]
    (map count-obs outcomes)))

(defn rm-2 [observed-corpus corpora-pairs]
  (filter (fn [x] (= observed-corpus (first (rest x)))) corpora-pairs))

(defn rejection-sampler [theta observed-corpus sample-size sent-len 
  corpus-len theta-probs]
  (let [corpora-pairs 
    (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)]
    (let [matching-corpora (rm-2 observed-corpus corpora-pairs)]
      (let [sample-theta (map (fn [x] (get-theta x)) matching-corpora)]
        (if (= 0 (count matching-corpora))
          nil
          (/ (get-count theta sample-theta 0) (count matching-corpora)))))))