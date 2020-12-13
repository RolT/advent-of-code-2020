(let [boarding-passes (->> (slurp "../../adventofcode/2020/05/puzzle.txt")
                           (clojure.string/split-lines)
                           (map (fn [b]
                                  {:row (subs b 0 7)
                                   :column (subs b 7)}))
                           (map (fn [{:keys [row column]}]
                                  {:row (Integer/parseInt (apply str (conj (map {\F \0 \B \1} row) \0)) 2)
                                   :column (Short/parseShort (apply str (conj (map {\L \0 \R \1} column) \0)) 2)})))]
  (apply max (map (fn [{:keys [row column]}] (+ (* row 8) column)) boarding-passes)))


(let [boarding-passes (->> (slurp "../../adventofcode/2020/05/puzzle.txt")
                           (clojure.string/split-lines)
                           (map (fn [b]
                                  {:row (subs b 0 7)
                                   :column (subs b 7)}))
                           (map (fn [{:keys [row column]}]
                                  {:row (Integer/parseInt (apply str (conj (map {\F \0 \B \1} row) \0)) 2)
                                   :column (Short/parseShort (apply str (conj (map {\L \0 \R \1} column) \0)) 2)})))
      seat-ids (sort (map (fn [{:keys [row column]}] (+ (* row 8) column)) boarding-passes))]
  (first (filter identity (map (fn [prev-seat seat] (when (not= (inc prev-seat) seat) [prev-seat seat])) seat-ids (drop 1 seat-ids)))))
