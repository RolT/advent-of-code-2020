(->> (slurp "passwords.txt")
     (clojure.string/split-lines)
     (map (fn [s] (let [[_ min max letter password] (re-matches #"(\d+)-(\d+) (\w): (\w+)" s)
                        min (Integer/parseInt min)
                        max (Integer/parseInt max)]
                    (if (<= min (count (filter #(= letter (str %)) password)) max)
                      1
                      0))))
     (reduce +))

(->> (slurp "../../adventofcode/2020/02/passwords.txt")
     (clojure.string/split-lines)
     (map (fn [s] (let [[_ pos1 pos2 letter password] (re-matches #"(\d+)-(\d+) (\w): (\w+)" s)
                        pos1 (dec (Integer/parseInt pos1))
                        pos2 (dec (Integer/parseInt pos2))
                        letter (get letter 0)]
                    (if (= 1 (count (filter #(= letter (get password %)) [pos1 pos2])))
                      1
                      0))))
     (reduce +))
