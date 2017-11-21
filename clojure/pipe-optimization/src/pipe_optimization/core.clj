(ns pipe-optimization.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn get-cuts
  [length inventory]
  (sort-by first
    (filter 
      #(= (reduce + 0 %) length)
      (combo/subsets
        (apply concat
          (filter (fn[e] (every? #(contains? inventory %) e)) 
          (map 
            #(repeat (- length (- % 1)) %)
            (range 1 (+ length 1)))))))))

(defn get-price-of-cuts
  [length cuts results inventory]
  (if (contains? inventory length)
    (reduce + 0 (map inventory cuts))
    (reduce + 0 (map first (map results cuts)))))

(defn max-price
  "takes a desired length and a hash map of inventory"
  [length inventory]
  (if (= length 0)
    {:max-price 0 :cuts [[]]} 
    (loop [cache {} i 1]
      (if (> i length)
        (apply hash-map (interleave [:max-price :cuts] (cache length)))
        (recur
         (let [cuts (get-cuts i inventory)]
           (let [ max-price (reduce max (map #(get-price-of-cuts i % cache inventory) cuts))]
             (merge 
              cache
              {i
                [max-price,
                (filter #(= (get-price-of-cuts i % cache inventory) max-price) 
                cuts)]})))
         (+ i 1))))))