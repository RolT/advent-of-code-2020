(let [passports (clojure.string/split (slurp "../../adventofcode/2020/04/puzzle.txt") #"\n\n")
      patterns (map #(re-pattern (str % ":")) ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])]
  (count (filter (fn [p] (= 7 (count (filter #(re-find % p) patterns)))) passports)))

(defn passports
  []
  (let [passports (for [p (clojure.string/split (slurp "../../adventofcode/2020/04/puzzle.txt") #"\n\n")]
                    (->> (clojure.string/split p #"(\n| )")
                         (map #(clojure.string/split % #":"))
                         (map (fn [[k v]] [(keyword k) v]))
                         (into {})))]
    (count (filter (fn [{:keys [byr iyr eyr hgt hcl ecl pid]}]
                     (and byr iyr eyr hgt hcl ecl pid
                          (re-matches #"\d{4}" byr)
                          (<= 1920 (Integer/parseInt byr) 2002)
                          (re-matches #"\d{4}" iyr)
                          (<= 2010 (Integer/parseInt iyr) 2020)
                          (re-matches #"\d{4}" eyr)
                          (<= 2020 (Integer/parseInt eyr) 2030)
                          (or (when-let [[_ height] (re-matches #"(\d+)cm" hgt)]
                                (<= 150 (Integer/parseInt height) 193))
                              (when-let [[_ height] (re-matches #"(\d+)in" hgt)]
                                (<= 59 (Integer/parseInt height) 76)))
                          (re-matches #"#[0-9a-f]{6}" hcl)
                          (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
                          (re-matches #"[0-9]{9}" pid)))
                   passports))))
