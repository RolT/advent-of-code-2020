(ns solution)

(defn parse-in [f]
  (let [puzzle (clojure.string/split-lines (slurp f))]
    {:start-timestamp (Integer/parseInt (first puzzle))
     :bus-ids (map #(Integer/parseInt %) (remove #{"x"} (clojure.string/split (second puzzle) #",")))}))

(defn part1 []
  (let [{:keys [start-timestamp bus-ids]} (parse-in "puzzle.txt")]
    (->> (map (fn [id] {:wait (- id (mod start-timestamp id)) :id id}) bus-ids)
         (apply min-key :wait)
         (vals)
         (apply *))))


(defn next-start [start cycle id pos]
  (loop [x start]
    (if #_(zero? (mod (+ pos x) id))
        (= (- id pos) (mod x id))
        x
        (recur (+ x cycle)))))

(defn compute-timestamp [ids]
  (let [[[_ id] & tail] (remove (fn [[pos id]] (= "x" id)) (map-indexed (fn [i v] [i v]) ids))]
    (loop [start id
           cycle id
           coll tail]
      (if (seq coll)
        (let [[pos id] (first coll)
              _ (println "cycle" cycle "id" id "pos" pos)
              new-start (next-start start cycle id pos)
              _ (println ".")
              new-cycle (- (next-start (+ new-start cycle) cycle id pos) new-start)]
          (println "start" new-start "cycle" new-cycle)
          (recur new-start new-cycle (rest coll)))
        start))))

(defn part2
  []
  (let [puzzle (map #(if (= "x" %) % (Integer/parseInt %)) (clojure.string/split (second (clojure.string/split-lines (slurp "puzzle.txt"))) #","))]
    (compute-timestamp puzzle)))
