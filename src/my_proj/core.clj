(load "my_proj/dnf")
(load "my_proj/fk")
(ns my-proj.core
  (:gen-class))





(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (dnf/disjunction '({1 2}) '({2 3}) ))
  )
