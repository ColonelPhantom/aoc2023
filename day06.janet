(def rows
  ~{:row (/ (some (* (number :d+) :s*)) ,array)
    :main (* "Time:" :s+ :row "Distance:" :s+ :row)})

(comment
  (def input (file/read (file/open "inputs/day06.test") :all))
  (peg/match races input)
  (let [[times dists] (peg/match races input)]
    (map |{:time $0 :dist $1} times dists))
  :rcf)

(defn part1 [{:time time :dist dist}]
  (count |(< dist (* $ (- time $))) (range time)))

(defn part2 [{:time time :dist dist}]
  (def bare-minimum (math/ceil (/ dist time)))
  (def mini (first (filter |(< dist (* $ (- time $))) (range bare-minimum time))))
  (def maxi (- time mini))
  #(printf "mini: %p; maxi: %p" mini maxi)
  (+ 1 (- maxi mini)))
  

(defn main [& args]
  (def input (file/read stdin :all))
  (def races (let [[times dists] (peg/match rows input)]
               (map |{:time $0 :dist $1} times dists)))
  (prin "Part 1: ") (print (product (map part1 races)))
  (prin "Part 1: ") (print (product (map part2 races)))

  (def [time dist] (let [[times dists] (peg/match rows input)]
                      [(scan-number (apply string times))
                       (scan-number (apply string dists))]))
  (pp [time dist])
  (prin "Part 2: ") (print (part2 {:time time :dist dist})))
                   
