(ns day8
  (:require [clojure.core.reducers :as reducers]
            [clojure.string :as string]))

(def test-layers
  (->> "123456789012"
       (partition 3)
       (partition 2)))

(def image-layers
  (->> "day8/input.txt"
      slurp
      (partition 25)
      (partition 6)))

(defn num-x [x l]
  (->> l
       (filter (partial = x))
       count))

(let [best-layer (->> image-layers
                      (map flatten)
                      (apply min-key (partial num-x \0)))]
  (* (num-x \1 best-layer) (num-x \2 best-layer)))

(defn merge-numbers [& numbers]
  (->> numbers
       (filter (partial not= \2))
       first))

(defn merge-layers [layers]
  (into [] (apply map merge-numbers layers)))

(->> "0222112222120000"
     (partition 4)
     merge-layers)

(defn pretty-print [image]
  (doseq [line image]
    (println (string/replace (string/join "" line) "0" " "))))

(->> "day8/input.txt"
     slurp
     (partition (* 25 6))
     merge-layers
     (partition 25)
     pretty-print)
