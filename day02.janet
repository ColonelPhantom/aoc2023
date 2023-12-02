(def parse-game
  ~{:color (/ '(+ "red" "blue" "green") ,keyword) # read color as keyword like :red
    :cubes (/ (* (number :d+) " " :color) ,(fn [n c] {c n})) # put count in struct
    :reveal (/ (some (* :cubes (? ", "))) ,merge) # merge shown cubes
    :main (* "Game " (number :d+) ": " (some (* :reveal (? "; "))))})

(defn filtermap
  "Like map, but only returns truthy values in the output array"
  [f ind & inds]
  (filter truthy? (map f ind ;inds)))

(defn get-reqs
  "Calculate the required cubes of each color in a game"
  [id & reveals]
  (def reqs @{:red 0 :green 0 :blue 0})
  (loop [reveal :in reveals
         color :keys reveal]
    (put reqs color (max (reqs color) (reveal color))))
  [id reqs])

(defn is-valid
  "Check if requirements are low enough for the game to be valid (part 1)"
  [id reqs]
  (and (<= (reqs :red)   12)
       (<= (reqs :green) 13)
       (<= (reqs :blue)  14)
       id))

(defn power
  "Calculate the power of the required cube set (part 2)"
  [id reqs]
  (* (reqs :red) (reqs :green) (reqs :blue)))

(defn main [& args]
  (def input (->> (file/lines stdin)
                  (map |(->> $ (peg/match parse-game)
                               (apply get-reqs)))))
  (prin "Part 1: ") (print (sum (filtermap |(apply is-valid $) input)))
  (prin "Part 2: ") (print (sum (map |(apply power $) input))))
