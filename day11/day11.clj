(ns day11
  (:require [clojure.core.async :as async]
            [clojure.core.matrix :as matrix]
            [clojure.string :as string])
  (:import [java.util ArrayList TreeMap]))

(defn parse-input [input-string]
  (map read-string (string/split input-string #",")))

(defn get-input []
  (read-string (read-line)))

(defn parse-instruction [n]
  (let [opcode (mod n 100)
        m1 (mod (quot n 100) 10)
        m2 (mod (quot n 1000) 10)
        m3 (mod (quot n 10000) 10)]
    [opcode m1 m2 m3]))

(defn program-to-table [program]
  (let [table (TreeMap.)
        i (atom 0)]
    (doseq [n program]
      (.put table @i n)
      (swap! i inc))
    table))

(defn get-val-rb [rb table index mode]
  (case mode
    0 (.getOrDefault table (.get table index) 0)
    1 (.getOrDefault table index 0)
    2 (.getOrDefault table (+ (.get table index) rb) 0)))

(defn put-val-rb [rb table index mode val]
  (case mode
    0 (.put table (.get table index) val)
    1 (.put table index) val
    2 (.put table (+ (.get table index) rb) val)))

(defn make-program [program]
  (let [table (program-to-table program)
        *i (atom 0)
        *relative-base (atom 0)]
    (fn [input]
      (let [instruction (parse-instruction (.get table @*i))
            opcode (first instruction)
            m1 (second instruction)
            m2 (nth instruction 2)
            m3 (nth instruction 3)
            get-val (partial get-val-rb @*relative-base)
            put-val (partial put-val-rb @*relative-base)]
        (cond (= opcode 1)
              (let [a (get-val table (inc @*i) m1)
                    b (get-val table (+ 2 @*i) m2)]
                (put-val table (+ 3 @*i) m3 (+ a b))
                (swap! *i #(+ 4 %))
                nil)

              (= opcode 2)
              (let [a (get-val table (inc @*i) m1)
                    b (get-val table (+ 2 @*i) m2)]
                (put-val table (+ 3 @*i) m3 (* a b))
                (swap! *i #(+ 4 %))
                nil)

              (= opcode 3)
              (let [in input]
                (put-val table (inc @*i) m1 in)
                (swap! *i #(+ 2 %))
                nil)

              (= opcode 4)
              (let [v (get-val table (inc @*i) m1)]
                (swap! *i #(+ 2 %))
                v)

              (= opcode 5)
              (let [param1 (get-val table (inc @*i) m1)
                    param2 (get-val table (+ 2 @*i) m2)]
                (if (not (= param1 0))
                  (reset! *i param2)
                  (swap! *i #(+ 3 %)))
                nil)

              (= opcode 6)
              (let [param1 (get-val table (inc @*i) m1)
                    param2 (get-val table (+ 2 @*i) m2)]
                (if (= param1 0)
                  (reset! *i param2)
                  (swap! *i #(+ 3 %)))
                nil)

              (= opcode 7)
              (let [param1 (get-val table (inc @*i) m1)
                    param2 (get-val table (+ 2 @*i) m2)]
                (if (< param1 param2)
                  (put-val table (+ 3 @*i) m3 1)
                  (put-val table (+ 3 @*i) m3 0))
                (swap! *i #(+ 4 %))
                nil)

              (= opcode 8)
              (let [param1 (get-val table (inc @*i) m1)
                    param2 (get-val table (+ 2 @*i) m2)]
                (if (= param1 param2)
                  (put-val table (+ 3 @*i) m3 1)
                  (put-val table (+ 3 @*i) m3 0))
                (swap! *i #(+ 4 %))
                nil)

              (= opcode 9)
              (let [param1 (get-val table (inc @*i) m1)]
                (swap! *relative-base #(+ param1 %))
                (swap! *i #(+ 2 %))
                nil)

              (= opcode 99)
              :finished)))))

(def directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn turn-robot [direction n]
  (let [new-direction ((if (> n 0) inc dec) direction)
        new-direction (mod new-direction 4)]
    new-direction))

(defn solve [robot]
  (let [painted (TreeMap.)
        steps (ArrayList.)
        *paint-counter (atom 0)
        *position (atom [0 0])
        *direction (atom 0)
        *finished (atom false)
        *paint-step (atom true)]
    (.put painted [0 0] 1)
    (while (not @*finished)
      (let [color (.get painted @*position)
            color (if (nil? color) 0 color)]
        (when-let [output (robot color)]
          (if (= output :finished) (swap! *finished not)
              (do
                (if @*paint-step (do (swap! *paint-step not)
                                     (when (not= output color) (swap! *paint-counter inc))
                                     (.put painted @*position output))
                    (do
                      (swap! *paint-step not)
                      (.add steps output)
                      (swap! *direction turn-robot output)
                      (swap! *position matrix/add (nth directions @*direction)))))))))

    (println @*paint-counter)
    (println (count painted))
    painted))

(defn print-panel [panel]
  (doseq [x (range 6)]
    (doseq [y (range -3 100)]
      (let [x (- x )
            color (.get panel [x y])
            color (if (nil? color) 0 color)]
        (if (= color 0) (print " ")
            (print "#"))))
    (println)))

(defn fake-robot [input]
  (println input)
  (get-input))

(defn metasolve [program]
  (let [robot (make-program program)]
    (print-panel (solve robot))))

;; (metasolve (parse-input (slurp "day11/input")))
