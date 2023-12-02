(def parse-game
  ~{:color (/ '(+ "red" "blue" "green") ,keyword) # read color as keyword like :red
    :cubes (/ (* (number :d+) " " :color) ,(fn [n c] {c n})) # put count in struct
    :reveal (/ (some (* :cubes (? ", "))) ,merge) # merge shown cubes
    :main (* "Game " (number :d+) ": " (some (* :reveal (? "; "))))})

(defn get-reqs
  "Calculate the required cubes of each color in a game"
  [id & reveals]
  (def reqs @{:red 0 :green 0 :blue 0})
  (loop [reveal :in reveals
         color :keys reveal]
    (put reqs color (max (reqs color) (reveal color))))
  {:id id :reqs reqs})

(defn is-valid
  "Check if requirements are low enough for the game to be valid (part 1)"
  [{:id id :reqs reqs}]
  (and (<= (reqs :red)   12)
       (<= (reqs :green) 13)
       (<= (reqs :blue)  14)))

(defn power
  "Calculate the power of the required cube set (part 2)"
  [{:id id :reqs reqs}]
  (* (reqs :red) (reqs :green) (reqs :blue)))

(defn main [& args]
  (def input (seq [line :in (file/lines stdin)]
               (->> line (peg/match parse-game) (apply get-reqs))))
  (prin "Part 1: ") (print (sum (map |($ :id) (filter is-valid input))))
  (prin "Part 2: ") (print (sum (map power input))))
