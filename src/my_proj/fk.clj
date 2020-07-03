(ns fk)
(require '[clojure.set :as set])
(require '[clojure.core.reducers :as r])
(require '[clojure.math.numeric-tower :as math])
(load "my_proj/dnf")


(defn easycases [f g]
    (defn samevars []
        (def var-f (dnf/vars f))
        (def var-g (dnf/vars g))
        (let [fminusg (set/difference var-f var-g)
              gminusf (set/difference var-g var-f)
              diff (if (not (empty? fminusg)) (list 'f fminusg) (list 'g gminusf))]
            (if (= var-f var-g) true
                (map #(list % (first diff)) (second diff)))))
    (defn maxlength-property [f g]
        (defn max (dnf/maximum-clause f))
        (if (<= (count max) (count g)) true max))
    (defn intersection-property [f g]
        (defn nonempty-clause-formula [c f]
            (cond 
                (empty? f) true
                (empty? (set/intersection c (first f))) false
                :else (first f)))
        (cond
            (empty? f) true
            (nonempty-clause-formula (first f) g) (intersection-property (rest f) g)
            :else (first f)))
    (defn inequality-property []
        (defn sum-formula [f] (r/fold (fn ([x accum] (+ x accum) ([] 0)))
                                     (map #(math/expt 2 (- (clause-len %))) f)))
        (if (>= (+ (sum-formula f) (sum-formula g)) 1) true '(inequality)))
    (list (samevars f g) (intersection-property g f) (maxlength-property f g) (maxlength-property g f)))
        
