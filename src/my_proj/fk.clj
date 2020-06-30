(ns fk)
(require '[clojure.set :as set])
(load "my_proj/dnf")


(defn easycases [f g]
    (defn samevars [f g]
        (def var-f (dnf/vars f))
        (def var-g (dnf/vars g))
        (let [fminusg (set/difference var-f var-g)
              gminusf (set/difference var-g var-f)
              diff (if (not (empty? fminusg)) (list 'f fminusg) (list 'g gminusf))]
            (if (= var-f var-g) true
                (map #(list % (first diff)) (second diff)))))
    (list (samevars f g)))