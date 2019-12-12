(ns day12
  (:import java.util.HashSet))

(def start-moons (->> [[-1 0 2]
                       [2 -10 -7]
                       [4 -8 8]
                       [3 5 -1]]
                      (map (partial zipmap [:x :y :z]))
                      (map (fn [pos] {:pos pos :vel {:x 0 :y 0 :z 0}}))))

(defn gravity [a b]
  (cond (< a b) 1
        (> a b) -1
        :default 0))

(defn single-dimension [dim moon]
  (-> moon
      (update :pos dim)
      (update :vel dim)))

(defn acceleration [moons {:keys [pos]}]
  (->> moons
       (map :pos)
       (map (partial gravity pos))
       (reduce (partial +))))

(defn accel-step [moons]
  (map (fn [moon]
         (let [new-velocity (acceleration moons moon)]
           (update moon :vel (partial + new-velocity))))
       moons))

(defn velocity-step [moons]
  (map (fn [{:keys [vel] :as moon}]
         (update moon :pos (partial + vel)))
       moons))

(defn step [moons]
  (-> moons accel-step velocity-step))

(let [moons (map (partial single-dimension :x) start-moons)]
  (step moons))

(defn nth-step [moons n]
  (nth (iterate step moons) n))

(nth-step (map (partial single-dimension :y) start-moons) 10)

;; (* 18 28 44)

(defn moon-energy [moon]
  (->> moon
       vals
       (map vals)
       (map (partial map #(Math/abs %)))
       (map (partial apply +))
       (apply *)))

(defn energy [moons]
  (->> moons
       (map moon-energy)
       (reduce +)))

(defn nth-energy [moons n]
  (nth (map energy (iterate step moons)) n))

(moon-energy (first (nth-step start-moons 10)))

(energy (nth-step start-moons 10))

(defn single-dim-step
  )

(map (partial single-dimension :x) start-moons)

(def long-moons (->> [[-8 -10 0]
                      [5 5 10]
                      [2 -7 3]
                      [9 -8 -3]]
                     (map (partial zipmap [:x :y :z]))
                     (map (fn [pos] {:pos pos :vel {:x 0 :y 0 :z 0}}))))

(def puzzle-input (->> [[-1 -4 0]
                        [4 7 -1]
                        [-14 -10 9]
                        [1 2 17]]
                       (map (partial zipmap [:x :y :z]))
                       (map (fn [pos] {:pos pos :vel {:x 0 :y 0 :z 0}}))))

(nth-energy puzzle-input 1000)

(let [dim :z
      moons (map (partial single-dimension dim) puzzle-input)
      seen (HashSet.)
      *n (atom 0)]
  (doseq [state (iterate step moons)]
    (when (.contains seen state)
      (throw (Exception. (str "loop at n=" @*n))))
    (.add seen state)
    (swap! *n inc)))
