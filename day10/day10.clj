(ns day10
  (:require [clojure.core.matrix :as matrix]
            [clojure.string :as string])
  (:import [java.util ArrayList HashMap]))

(def example1 (-> "
.#..#
.....
#####
....#
...##" .trim))

(def example2
  "
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
")

(def example3
  "
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
")

(def example4
  "
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
")

(def example5 "
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
")

(defn find-asteroids [s]
  (let [s (.trim s)
        lines (string/split-lines s)
        height (count lines)
        width (count (first lines))]
    (for [x (range width)
          y (range height)
          :let [v (nth (nth lines y) x)]
          :when (= v \#)]
      [x y])))

(defn vec2-nor [[x y]]
  (let [len (matrix/length [x y])
        lenfn (fn [n]
                (if (== len 0)
                  (if (> n 0) ##Inf ##-Inf)
                  (/ n len)))]
    [(lenfn x) (lenfn y)]))

(defn direction [from to]
  (-> from
      (matrix/sub to)
      (vec2-nor)))

(defn distance [from to]
  (-> from
      (matrix/sub to)
      (matrix/length)))

(defn asteroid-directions [origin asteroids]
  (map (partial direction origin) asteroids))

(defn direction-in-list? [dir l]
  (->> l
       (map #(matrix/equals dir % 0.001))
       (some true?)))

(defn unique-directions [directions]
  (let [s (ArrayList.)]
    (doseq [dir directions]
      (when-not (direction-in-list? dir s)
        (.add s dir)))
    s))

(defn solve [asteroids]
  (let [prev (atom (first (first asteroids)))
        skipped (ArrayList.)
        out (ArrayList.)]
    (doseq [[direction roid] (rest asteroids)]
      (if (matrix/equals @prev direction 0.0001)
        (.add skipped [direction roid])
        (do
          (reset! prev direction)
          (.add out roid))))
    out))

(def best-location [ 17 22 ])
(def best-location [ 11 13 ])

(defn ratio [a b]
  (if (== a 0)
    (if (< b 0) ##Inf ##-Inf)
    (/ b a)))

(defn asteroid-to-direction [asteroids]
  (->> asteroids
       (interleave (map (partial direction best-location) asteroids))
       (partition 2)
       (map vec)))

(last (doto (ArrayList.)
        (.add 1)
        (.add 2)
        (.add 3)))

(defn filter-closest [s]
  (let [seen (ArrayList.)]
    (doseq [[dir roid] s]
      (let [[prev-dir prev-roid] (last seen)]
        (if-not (and (not (nil? prev-dir))
                     (matrix/equals dir prev-dir 0.001))
          (.add seen [dir roid])

          (do
            (when (<= (distance best-location roid)
                      (distance best-location prev-roid))
              (.set seen (dec (.size seen)) [dir roid]))))))
    seen))

(defn angle [[ox oy] [x y]]
  (let [a (* 57.2958 (Math/atan2 (- oy y) (- ox x)))]
    (if (< a 90) (+ 360 a)
        a)))

(angle [1 0])
(angle [0 1])
(angle [ -0.08304547985373997 0.9965457582448796 ])

(* 57.2958 (Math/atan2 1 0))

(defn vec2-comp [a b]
  (if (matrix/equals a b 0.00001) 0
      (compare (angle a) (angle b))))

(defn asteroid-to-angle [bl asteroids]
  (->> asteroids
       (interleave (map (partial angle bl) asteroids))
       (partition 2)
       (map vec)))

(defn sorta-eq? [a b]
  (< (Math/abs (- a b)) 0.0001))

(defn filter-closest [s]
  (let [seen (ArrayList.)]
    (doseq [[dir roid] s]
      (let [[prev-dir prev-roid] (last seen)]
        (if-not (and (not (nil? prev-dir))
                     (sorta-eq? dir prev-dir))
          (.add seen [dir roid])

          (do
            (when (<= (distance best-location roid)
                      (distance best-location prev-roid))
              (.set seen (dec (.size seen)) [dir roid]))))))
    seen))

(def best-location [ 11 13 ])

(let [asteroids (find-asteroids example5)
      m (asteroid-to-angle best-location asteroids)]
  (->> m
       (sort-by first)
       filter-closest))

(def best-location [ 17 22 ])

(let [asteroids (find-asteroids (slurp "day10/input"))
      m (asteroid-to-angle best-location asteroids)]
  (->> m
       (sort-by first)
       filter-closest))

(let [asteroids (find-asteroids )
      m (asteroid-to-direction asteroids)]
  (->> m
       (sort-by #(second (first %)) >)
       (sort-by #(first (first %)) >)))
