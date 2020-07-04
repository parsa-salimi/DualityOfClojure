(ns fk)
(require '[clojure.set :as set])
(require '[clojure.core.reducers :as r])
(require '[clojure.math.numeric-tower :as math])
(require '[clojure.math.combinatorics :as combo])
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
        (def maxf (dnf/maximum-clause f))
        (if (<= (count maxf) (count g)) true maxf))
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
    (defn inequality-property [f g]
        (defn sum-formula [f] (r/fold (fn ([x accum] (+ x accum)) ([] 0))
                                     (map #(math/expt 2 (- (dnf/clause-len %))) f)))
        (if (>= (+ (sum-formula f) (sum-formula g)) 1) true '(inequality)))
    (list (samevars) (intersection-property g f) (maxlength-property f g) (maxlength-property g f) (inequality-property f g)))
(defn sanitycheck [f g] (= (easycases f g) '(true true true true true)))

(defn easydual [f g varlist]
    ;certificate if other formula is 0
    (defn cert-0 [f] (first f))
    ;certificate if other formula is 1
    (defn cert-1 [f] (set/difference varlist (first f)))
    (cond
        (and (empty? f) (empty? g))   (list false '())
        (empty? f) (if (= g '(#{})) '(true 'nocert) (list false (cert-0 g)))
        (empty? g) (if (= f '(#{})) '(true 'nocert) (list false (cert-0 f)))
        (empty? (first f))  (list false (cert-1 g))
        (empty? (first g))  (list false (cert-1 f))
        :else '(true 'nocert)))

(defn frequency [var f]
    (if (empty? f) 0
        (/ (count (filter #(contains? % var) f)) (count f))))
(defn total-frequency [var f g] (max (frequency var g) (frequency var f)))

;Here we implement the pivot rules and the tiebreakers
(defn log [arg base] (/ (Math/log arg) (Math/log base)))
;pivot rules:
(defn fthresh [varlist f g]
    (def guarantee (/ 1 (log (+ (count f) (count g)) 2)))
    (filter #(>= (total-frequency % f g) guarantee) varlist))
(defn fmin [varlist f g] varlist)
;tiebreakers:
(defn tbnaive [varlist] (first varlist))
(defn tblast  [varlist] (last varlist))
(defn tblex   [varlist] (apply min varlist))

(defn simpledisjunction? [f g]
    (defn listofsimps? [g]
        (cond
            (empty? g) true
            (= (count (first g)) 1) (listofsimps? (rest g))
            :else false))
    (and (= (count f) 1)
         (listofsimps? g)))
(defn fk-help [f g pivot tiebreaker varlist]
    (def easycaseslist (easycases f g))
    (cond
        (not (= easycaseslist '(true true true true true))) (list false 'cert)
        (<= (* (dnf/clause-len f) (dnf/clause-len g)) 1)  (easydual f g varlist)
        (simpledisjunction? f g)   '(true 'cert)
        (simpledisjunction? g f)   '(true 'cert)
        :else (let [x (tiebreaker (pivot (dnf/vars f) f g))
                    f0 (dnf/remove-var f x)
                    f1 (dnf/remove-clause f x)
                    g0 (dnf/remove-var g x)
                    g1 (dnf/remove-clause g x)
                    left    (future (fk-help (dnf/freduce f1) (dnf/freduce (dnf/disjunction g0 g1)) pivot tiebreaker (set/difference varlist #{x})))
                    right   (future (fk-help (dnf/freduce (dnf/disjunction f0 f1)) (dnf/freduce g1) pivot tiebreaker (set/difference varlist #{x}))) ]
                   (list (and (first  @left) (first   @right)) 'recursion))))
(defn fk [f g pivot tiebreaker]
    (fk-help f g pivot tiebreaker (dnf/vars f)))

(defn combcount [n]
    (def formula (map #(into #{} %) (combo/combinations (range 1 (* 2 n)) n)))
    (fk-help formula formula fmin tbnaive (into #{} (range 1 (* 2 n)))))

          
