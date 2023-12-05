(defn card [id winning mine]
  @{:id id
    :winning winning
    :mine mine
    :winset (tabseq [w :in winning] w true)
    :copies 1})

(def parse-card
  ~{:array (/ (some (* :s* (number :d+) :s*)) ,array)
    :main (/ (* "Card" :s+ (number :d+) ":" :array "|" :array) ,card)})

(defn score [card]
  (match (count |(get (card :winset) $) (card :mine))
    0 0
    1 1
    n (math/pow 2 (- n 1))))

(defn score2 [card cardmap]
  (def {:winset winset :mine mine :id id :copies copies} card) # destructure
  (def wins (count |(get winset $) mine))       # calculate number of wins
  (loop [i :range-to [1 wins]] # add self number of copies to winning cards
    (update (cardmap (+ id i)) :copies |(+ $ copies)))
  copies)

(defn main [& args]
  (def cards (mapcat |(peg/match parse-card $) (file/lines stdin)))
  (prin "Part 1: ") (print (sum (map score cards)))

  (def cardmap (tabseq [c :in cards] (c :id) c))
  (prin "Part 2: ") (print (sum (map |(score2 $ cardmap) cards))))
