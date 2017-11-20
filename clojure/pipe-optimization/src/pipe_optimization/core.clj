(ns pipe-optimization.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn get-cuts
  [length prices]
  (sort-by first
    (filter 
      #(= (reduce + 0 %) length)
      (combo/subsets
        (apply concat
          (filter (fn[e] (every? #(contains? prices %) e)) 
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
         (let [cuts (if (< i 2)
                      (if (< i 1) nil [[1]])
                      (get-cuts i inventory))]
           (let [prices (map #(get-price-of-cuts i % cache inventory) cuts)
                 my-max (reduce max prices)]
             (merge cache
                    {i
                     [my-max,
                      (filter
                       #(= (get-price-of-cuts i % cache inventory) my-max)
                       cuts)]})))
         (+ i 1))))))