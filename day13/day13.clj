(ns day13
  (:require [clojure.string :as string])
  (:import [java.util TreeMap]))

(defn parse-input [input-string]
  (map read-string (string/split input-string #",")))

(-> "day13/input"
    slurp
    parse-input)

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

(defn make-game [program]
  (let [*finished (atom false)]
    (fn []
      (if @*finished :finished
          (let [*result (atom nil)]
            (while (nil? @*result)
              (let [temp (program nil)]
                (when-not (nil? temp)
                  (when (= temp :finished)
                    (reset! *finished true))
                  (reset! *result temp))))
            @*result)))))

(defn make-fake-game [v]
  (let [*iter (atom -1)]
    (fn []
      (nth v (swap! *iter inc)))))

(defn test-game []
  (make-fake-game [1 2 3 6 5 4 :finished]))

(defn display [game]
  (let [*x (atom (game))
        m (TreeMap.)]
    (while (not= @*x :finished)
      (let [x @*x
            y (game)
            tile-id (game)]
        (.put m [x y] tile-id)
        (reset! *x (game))))
    m))

(display (test-game))

(defn real-game []
  (-> "day13/input"
      slurp
      parse-input
      make-program
      make-game))

(println
 (count
  (filter (partial = 2)
          (.values (display (real-game))))))
