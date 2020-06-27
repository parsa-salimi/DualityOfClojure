(ns dnf)
(require '[clojure.core.reducers :as r])
(require '[clojure.set :as set])

;a formula is a list of sets
(defn disjunction [f g] (distinct (concat f g)))
(def clause-len count)
(defn vars [f] (distinct (flatten f)))
(defn remove-clause [f x]
    (cond   
            (empty? f) nil
            (some #(= x %) (first f)) (remove-clause (rest f) x)
            :else (conj (first f) (remove-clause (rest f) x))))
(defn remove-var [f x]
    (cond
        (empty? f) nil
        (some #(= x %) (first f)) (cons (into #{} (remove #(= x %) (first f))) (remove-var (rest f) x))
        :else (conj (first f) (remove-var (rest f) x))))
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
     (r/fold (fn ([x y] (disjunction (mult-clause (first x) g) y))
                ([] '())) f))


        
 




            
        
    

