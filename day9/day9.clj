(ns day9
  (:require [clojure.core.async :as async]
            [clojure.string :as string])
  (:import java.util.HashMap))

(defn parse-input [input-string]
  (map read-string (string/split input-string #",")))

(parse-input (slurp "day7/input"))

(defn opcode-to-op [n]
  (cond (= n 1) +
        (= n 2) *
        (= n 99) "finish"
        :default (throw (Exception. (str "bad opcode " n)))))

(defn parse-instruction [n]
  (let [opcode (mod n 100)
        m1 (mod (quot n 100) 10)
        m2 (mod (quot n 1000) 10)
        m3 (mod (quot n 10000) 10)]
    [opcode m1 m2 m3]))

(defn program-to-table [program]
  (let [table (HashMap.)
        i (atom 0)]
    (doseq [n program]
      (.put table @i n)
      (swap! i inc))
    table))

(defn get-input []
  (read-string (read-line)))

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

(defn run [program]
  (let [table (program-to-table program)
        *i (atom 0)
        *done (atom nil)
        *relative-base (atom 0)]
    (while (not @*done)
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
                (swap! *i #(+ 4 %)))

              (= opcode 2)
              (let [a (get-val table (inc @*i) m1)
                    b (get-val table (+ 2 @*i) m2)]
                (put-val table (+ 3 @*i) m3 (* a b))
                (swap! *i #(+ 4 %)))

              (= opcode 3)
              (let [in (get-input)]
                (put-val table (inc @*i) m1 in)
                (swap! *i #(+ 2 %)))

              (= opcode 4)
              (let [v (get-val table (inc @*i) m1)]
                (println v)
                (swap! *i #(+ 2 %)))

              (= opcode 5)
              (let [param1 (get-val table (inc @*i) m1)
                    param2 (get-val table (+ 2 @*i) m2)]
                (if (not (= param1 0))
                  (reset! *i param2)
                  (swap! *i #(+ 3 %))))

              (= opcode 6)
              (let [param1 (get-val table (inc @*i) m1)
                    param2 (get-val table (+ 2 @*i) m2)]
                (if (= param1 0)
                  (reset! *i param2)
                  (swap! *i #(+ 3 %))))

              (= opcode 7)
              (let [param1 (get-val table (inc @*i) m1)
                    param2 (get-val table (+ 2 @*i) m2)]
                (if (< param1 param2)
                  (put-val table (+ 3 @*i) m3 1)
                  (put-val table (+ 3 @*i) m3 0))
                (swap! *i #(+ 4 %)))

              (= opcode 8)
              (let [param1 (get-val table (inc @*i) m1)
                    param2 (get-val table (+ 2 @*i) m2)]
                (if (= param1 param2)
                  (put-val table (+ 3 @*i) m3 1)
                  (put-val table (+ 3 @*i) m3 0))
                (swap! *i #(+ 4 %)))

              (= opcode 9)
              (let [param1 (get-val table (inc @*i) m1)]
                (swap! *relative-base #(+ param1 %))
                (swap! *i #(+ 2 %)))

              (= opcode 99)
              (do
                (println (str "result " (.get table 0)))
                (reset! *done true)))))))

(comment

  (run
    (parse-input
     "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
  (run
    (parse-input "1102,34915192,34915192,7,4,7,99,0"))
  (run
    (parse-input "104,1125899906842624,99"))

  (run (parse-input (slurp "day9/input.txt")))
  )
