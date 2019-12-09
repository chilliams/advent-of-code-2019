(ns day7
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

(defn get-val [position-mode index table]
  (if (not position-mode)
    (.get table index)
    (.get table (.get table index))))

(defn run [program input output name finish-fn]
  (let [table (program-to-table program)
        *i (atom 0)
        *done (atom nil)]
    (async/go
      (while (not @*done)
        (let [instruction (parse-instruction (.get table @*i))
              opcode (first instruction)
              m1 (= (second instruction) 0)
              m2 (= (nth instruction 2) 0)
              m3 (= (nth instruction 3) 0)]
          (cond (= opcode 1)
                (let [a (get-val m1 (inc @*i) table)
                      b (get-val m2 (+ 2 @*i) table)
                      out (.get table (+ 3 @*i))]
                  (.put table out (+ a b))
                  (swap! *i #(+ 4 %)))

                (= opcode 2)
                (let [a (get-val m1 (inc @*i) table)
                      b (get-val m2 (+ 2 @*i) table)
                      out (.get table (+ 3 @*i))]
                  (.put table out (* a b))
                  (swap! *i #(+ 4 %)))

                (= opcode 3)
                (let [in (async/<!! input)
                      out (.get table (inc @*i))]
                  (.put table out in)
                  (swap! *i #(+ 2 %)))

                (= opcode 4)
                (let [v (get-val m1 (inc @*i) table)]
                  (async/>!! output v)
                  (swap! *i #(+ 2 %)))

                (= opcode 5)
                (let [param1 (get-val m1 (inc @*i) table)
                      param2 (get-val m2 (+ 2 @*i) table)]
                  (if (not (= param1 0))
                    (reset! *i param2)
                    (swap! *i #(+ 3 %))))

                (= opcode 6)
                (let [param1 (get-val m1 (inc @*i) table)
                      param2 (get-val m2 (+ 2 @*i) table)]
                  (if (= param1 0)
                    (reset! *i param2)
                    (swap! *i #(+ 3 %))))

                (= opcode 7)
                (let [param1 (get-val m1 (inc @*i) table)
                      param2 (get-val m2 (+ 2 @*i) table)
                      param3 (.get table (+ 3 @*i))]
                  (if (< param1 param2)
                    (.put table param3 1)
                    (.put table param3 0))
                  (swap! *i #(+ 4 %)))

                (= opcode 8)
                (let [param1 (get-val m1 (inc @*i) table)
                      param2 (get-val m2 (+ 2 @*i) table)
                      param3 (.get table (+ 3 @*i))]
                  (if (= param1 param2)
                    (.put table param3 1)
                    (.put table param3 0))
                  (swap! *i #(+ 4 %)))

                (= opcode 99)
                (do
                  (finish-fn)
                  (reset! *done true))))))))

(defn chain [program phase-setting]
  (let [in-a (doto (async/chan (async/dropping-buffer 10000))
               (async/>!! (first phase-setting))
               (async/>!! 0))
        in-b (doto (async/chan (async/dropping-buffer 10000))
               (async/>!! (second phase-setting)))
        in-c (doto (async/chan (async/dropping-buffer 10000))
               (async/>!! (nth phase-setting 2)))
        in-d (doto (async/chan (async/dropping-buffer 10000))
               (async/>!! (nth phase-setting 3)))
        in-e (doto (async/chan (async/dropping-buffer 10000))
               (async/>!! (nth phase-setting 4)))
        out (async/chan (async/dropping-buffer 10000))]
    (run program in-a in-b "A" #())
    (run program in-b in-c "B" #())
    (run program in-c in-d "C" #())
    (run program in-d in-e "D" #())
    (run program in-e out "E" #())
    (async/go
      (print (str "got thrust of " (async/<!! out) "\n")))))

(defn feedback [program phase-setting]
  (let [in-a (doto (async/chan (async/dropping-buffer 100000))
               (async/>!! (first phase-setting))
               (async/>!! 0))
        in-b (doto (async/chan (async/dropping-buffer 100000))
               (async/>!! (second phase-setting)))
        in-c (doto (async/chan (async/dropping-buffer 100000))
               (async/>!! (nth phase-setting 2)))
        in-d (doto (async/chan (async/dropping-buffer 100000))
               (async/>!! (nth phase-setting 3)))
        in-e (doto (async/chan (async/dropping-buffer 100000))
               (async/>!! (nth phase-setting 4)))
        out (async/chan (async/dropping-buffer 100000))
        finished (atom 0)
        result (atom nil)]
    (run program in-a in-b "A" #(swap! finished inc))
    (run program in-b in-c "B" #(swap! finished inc))
    (run program in-c in-d "C" #(swap! finished inc))
    (run program in-d in-e "D" #(swap! finished inc))
    (run program in-e out "E" #(swap! finished inc))
    (while (< @finished 5)
      (let [val (async/poll! out)]
        (when val
          (reset! result val)
          (async/>!! in-a val))))
    @result))

(defn no-dups [l]
  (apply distinct? l))

(defn solve-day-7 [input]
  (let [*largest (atom 0)]
    (dotimes [a 5]
      (dotimes [b 5]
        (dotimes [c 5]
          (dotimes [d 5]
            (dotimes [e 5]
              (let [phase-setting (map #(+ % 5) [a b c d e])]
                (when (no-dups phase-setting)
                  (let [out (feedback input phase-setting)
                        best (max out @*largest)]
                    (reset! *largest best)))))))))
    (print @*largest)
    @*largest))

(comment
  (chain (parse-input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") '(4 3 2 1 0))
  (chain (parse-input "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
         '(0 1 2 3 4))
  (chain
   (parse-input
    "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
   '(1 0 4 3 2))

  (feedback
   (parse-input
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
   '(9 8 7 6 5))

  (solve-day-7
   (parse-input
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))

  (feedback
   (parse-input
    "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
   '(9 7 8 5 6))

  (solve-day-7
   (parse-input
    "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))

  (solve-day-7
   (parse-input (slurp "day7/input")))
  )
