(defn card [c]
  (case c
    "A" 14
    "K" 13
    "Q" 12
    "J" 11
    "T" 10
    (scan-number c)))

(defn cards [& cs]
  (map card cs))

(def hand
  ~{:game (/ (some ':w) ,cards)
    :bid (number :d+)
    :main (/ (* :game :s+ :bid) ,|{:game $0 :bid $1})})

(defn compare-hands [{:game a} {:game b}]
  (defn occs [hand]
    (defn of-card [card] (count |(= $ card) hand))
    (sort (values (tabseq [c :in hand]
                          c (of-card c))) >))
  (def occa (occs a))
  (def occb (occs b))
  (printf "part 1 - %P (%P);  %P (%P)" a occa b occb)
  (defn has [n occs]
    (all true? (map = n occs)))
  (defn tie [a b]
    (cond
      (or (empty? a) (empty? b)) false # equal, so whatever
      (< (first a) (first b)) true
      (> (first a) (first b)) false
      (= (first a) (first b)) (tie (array/slice a 1) (array/slice b 1))))
  (defn typecheck [f]
    (cond
      (and (f occa) (f occb)) (tie a b)
      (f occa) false
      (f occb) true
      nil))
  (def types [|(has [5] $) # five of a kind
              |(has [4] $) # four of a kind
              |(has [3 2] $) # full house
              |(has [3] $) # three of a kind
              |(has [2 2] $) # two pairs
              |(has [2] $) # one pair
              (fn [c] true)]) #high card
  (if (deep= a b) false
    (first
      (filter (comp not nil?)
        (map typecheck types)))))

(defn part2 [{:game a} {:game b}]
  (defn occs [hand]
    (def jokers (count |(= $ 11) hand))
    (defn of-card [card] (count |(= $ card) hand))
    (def cards (sort (values (tabseq [c :in hand :unless (= c 11)]
                                     c (of-card c))) >))
    (if (empty? cards) [5]
      (do (+= (cards 0) jokers)
          cards)))
  (def occa (occs a))
  (def occb (occs b))
  (defn has [n occs]
    (all truthy? (map = n occs)))
  (defn tie [a b]
    (cond
      (or (empty? a) (empty? b)) false # equal, so whatever
      (= (first a) (first b)) (tie (array/slice a 1) (array/slice b 1))
      (= (first a) 11) true # jokers are weakest now
      (= (first b) 11) false
      (< (first a) (first b)) true
      (> (first a) (first b)) false))
  (defn typecheck [f]
    (cond
      (and (f occa) (f occb)) (tie a b)
      (f occa) false
      (f occb) true
      nil))
  (def types [|(has [5] $) # five of a kind
              |(has [4] $) # four of a kind
              |(has [3 2] $) # full house
              |(has [3] $) # three of a kind
              |(has [2 2] $) # two pairs
              |(has [2] $) # one pair
              (fn [c] true)]) #high card
  (first
    (filter (comp not nil?)
      (map typecheck types))))

(defn cmp2 [& hands]
  #(prinf "Comparing %p" hands)
  (def result (part2 (hands 0) (hands 1)))
  #(printf " = %p" result)
  result)


(comment
  (def input (file/lines (file/open "inputs/day07.test")))
  (mapcat |(peg/match hand $) input)
  (compare-hands (hands 2) (hands 3))
  (part2 {:game @[11 8 8 8 8]} {:game @[6 6 6 6 6]}) # true
  (part2 {:game @[13 10 11 11 10]} {:game @[13 10 11 11 10]}) #fals?
  (part2 {:game @[14 7 14 7 7]} {:game @[14 9 11 10 14]}) #false
  (part2 {:game @[14 14 8 11 8]} {:game @[2 2 2 4 4]}) # false
   
   
  hands
  (sorted hands cmp-hand)
  (paired @[2 3 4 3 2]))

(defn main [& args]
  (def input (file/lines stdin))
  (def hands (mapcat |(peg/match hand $) input))
  (sort hands compare-hands)
  (printf "%M" hands)
  (def winnings (seq [[i game] :pairs hands]
                  (* (+ i 1) (game :bid))))
  (prin "Part 1: ") (print (sum winnings))

  (sort hands cmp2)
  (printf "%M" hands)
  (def winnings (seq [[i game] :pairs hands]
                  (* (+ i 1) (game :bid))))
  (prin "Part 2: ") (print (sum winnings)))
