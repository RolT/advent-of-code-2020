(defn forms []
  (let [groups (clojure.string/split (slurp "../../adventofcode/2020/06/puzzle.txt") #"\n\n")]
    (->> groups
         (map (fn [s] (set (clojure.string/replace s #"\n" ""))))
         (map count)
         (reduce +))))

(defn forms []
  (let [groups (->> (clojure.string/split (slurp "../../adventofcode/2020/06/puzzle.txt") #"\n\n")
                    (map #(map set (clojure.string/split-lines %))))]
    (->> groups
         (map #(apply clojure.set/intersection %))
         (map count)
         (reduce +))))
