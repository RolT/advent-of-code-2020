(ns solution)

(def puzzle [11, 0, 1, 10, 5, 19])

(defn next-numbers [turns turn n]
  (let [spoken (if (turns n)
                 (- turn (turns n))
                 0)]
    (lazy-seq
      (cons spoken
            (next-numbers (assoc! turns n turn)
                          (inc turn)
                          spoken)))))

(defn numbers [puzzle]
  (concat puzzle
          (next-numbers (transient (into {} (map-indexed (fn [i x] [x i]) (butlast puzzle))))
                        (dec (count puzzle))
                        (last puzzle))))
