(ns dnf)
(require '[clojure.core.reducers :as r])
(require '[clojure.math.numeric-tower :as math])
(require '[clojure.set :as set])

;a formula is a list of sets
(defn disjunction [f g] (distinct (concat f g)))
(def clause-len count)
(defn vars [f] (apply set/union f))
(defn remove-clause [f x]
    (cond   
            (empty? f) nil
            (some #(= x %) (first f)) (remove-clause (rest f) x)
            :else (cons (first f) (remove-clause (rest f) x))))
(defn remove-var [f x]
    (cond
        (empty? f) nil
        (some #(= x %) (first f)) (cons (into #{} (remove #(= x %) (first f))) (remove-var (rest f) x))
        :else (cons (first f) (remove-var (rest f) x))))
(defn minimum-clause [f]
    (defn combine ([x accum] (if (< (count x) (count accum)) x accum))
                  ([]  (first f)))
    (if (empty? f) nil
        (r/fold combine f)))
(defn maximum-clause [f]
    (defn combine ([x accum] (if (> (count x) (count accum)) x accum))
                  ([]  (first f)))
    (if (empty? f) nil
        (r/fold combine f)))
(defn freduce [f]
    (defn minimal? [clause f]
        (if (empty? f) true
            (and (not (set/subset? (first f) clause))
                 (minimal? clause (rest f)))))
    (defn reduce-help [num f]
        (cond
            (= num (count f)) nil
            (minimal? (nth f num) (remove #(= % (nth f num)) f))  (cons (nth f num) (reduce-help (+ num 1) f))
            :else (reduce-help (+ num 1) f)))
    (if (some empty? f) '(#{})
        (reduce-help 0 (distinct f))))

(def add disjunction)
(defn mult [f g]
    (defn mult-clause [clause g] (map #(set/union clause %) g))
    (defn mult-accum [f accum]
        (if (empty? f) accum
            (mult-accum (rest f) (disjunction (mult-clause (first f) g) accum))))
    (freduce (mult-accum f nil)))
(defn insert [cert pos val]
    (if (= val 0) cert
        (if (list? cert) (cons pos cert) cert)))

;Generators for the f-n and g-n functions
(defn f-n [k]
    (defn bigN [n] (math/expt 2 (- (* 2 n) 1)))
    (defn vargen [a b] (range a (+ 1 b)))
    (defn f-list [k varlist]
        (if (= k 1) (map (fn [x] #{x}) varlist)
            (let [newk (bigN (- k 1))
                  list-1 (take newk varlist)
                  list-2 (take newk (drop (* 1 newk) varlist))
                  list-3 (take newk (drop (* 2 newk) varlist))
                  list-4 (take newk (drop (* 3 newk) varlist))]
                (add (mult (f-list (- k 1) list-1) (f-list (- k 1) list-2))
                     (mult (f-list (- k 1) list-3) (f-list (- k 1) list-4))))))
    (f-list k (vargen 1 (bigN k))))
(defn g-n [k]
    (defn bigN [n] (math/expt 2 (- (* 2 n) 1)))
    (defn vargen [a b] (range a (+ 1 b)))
    (defn g-list [k varlist]
        (if (= k 1) (list (into #{} varlist))
            (let [newk (bigN (- k 1))
                  list-1 (take newk varlist)
                  list-2 (take newk (drop (* 1 newk) varlist))
                  list-3 (take newk (drop (* 2 newk) varlist))
                  list-4 (take newk (drop (* 3 newk) varlist))]
            (mult (add (g-list (- k 1) list-1) (g-list (- k 1) list-2))
                  (add (g-list (- k 1) list-3) (g-list (- k 1) list-4))))))
    (g-list k (vargen 1 (bigN k))))


     


        
 




            
        
    

