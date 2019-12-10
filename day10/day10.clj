(ns day10
  (:require [clojure.core.matrix :as matrix]
            [clojure.string :as string])
  (:import java.util.ArrayList))

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

(defn direction [from to]
  (-> from
      (matrix/sub to)
      (matrix/normalise)))

(defn visible-asteroids-directions [asteroids origin]
  (->> asteroids
       (filter (partial not= origin))
       (map (partial direction origin))))

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

(let [asteroids (find-asteroids (slurp "day10/input"))]
  (->> asteroids
       (map (partial visible-asteroids asteroids))
       (map unique-directions)
       (map count)
       (apply max)
       println))
