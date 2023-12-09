(def parser
  ~{:seeds (/ (* "seeds: " (some (* (number :d+) :s*))) ,array)
    :range (/ (* (number :d+) " " (number :d+) " " (number :d+) "\n") ,|{:dest $0 :src $1 :len $2})
    :map (/ (* (some :S) " map:\n" (some :range) :s*) ,array)
    :main (/ (* :seeds (some :map)) ,|{:seeds $0 :maps $&})})

(defn <<= [from self to] (and (<= from self) (< self to)))

# Part 1
(defn lookup [seed {:src from :len len :dest dest}]
  (if (<<= from seed (+ from len)) (+ dest (- seed from))))

(defn advance1 [seed ranges]
  (or (some |(lookup seed $) ranges) seed))

(defn advance [seed maps]
  (reduce advance1 seed maps))

# Part 2: work by intersecting ranges
(defn split [seed mapping] # Splits a range so that each subrange can be moved entirely
  (def {:from seedfrom :to seedto} seed)
  (def {:src from :len len :dest dest} mapping)
  (def to (+ from len))
  (cond
    (or (<= seedto from) (<= to seedfrom)) [seed] # entirely out of range
    (and (<<= from seedfrom to) (<= seedto to)) [seed] # entirely in range
    (and (<<= seedfrom from to) (<= seedto to)) # seed extends on left only
    [{:from seedfrom :to from}
     {:from from :to seedto}]
    (and (<<= from seedfrom to) (<<= from to seedto)) # seed extends on right only
    [{:from seedfrom :to to}
     {:from to :to seedto}]
    (and (<= seedfrom from) (<= to seedto)) # seed extends on both sides
    [{:from seedfrom :to from}
     {:from from :to to}
     {:from to :to seedto}]
    (error (string/format "Unknown case; seed=%p mapping=%p to=%p" seed mapping to))))

(defn move-range [{:from from :to to} mapping] # Move range when fully in mapping
  (def to1 (- to 1))
  (def froml (lookup from     mapping))
  (def tol   (lookup (- to 1) mapping))
  (if (and froml tol) {:from froml :to (+ 1 tol)}))

(defn advance1-range [seeds ranges] # Move ranges according to mapping
  (var ss seeds)
  (loop [mapping :in ranges]                                  # first split the seed ranges
    (set ss (mapcat |(split $ mapping) ss)))                  # so that theyre fully in/out
  (catseq [s :in ss] (or (some |(move-range s $) ranges) s))) # then move all mapped ones

(defn advance-range [[from len] maps] # Move seed range to location ranges
  (printf "STARTING %p" [from len])
  (reduce advance1-range [{:from from :to (+ from len)}] maps))

(defn main [& args]
  (def input (file/read stdin :all))
  (def [{:seeds seeds :maps maps}] (peg/match parser input))
  (def locations (map |(advance $ maps) seeds))
  (prin "Part 1: ") (print (min-of locations))

  (def locations2 (mapcat |(advance-range $ maps) (partition 2 seeds)))
  (pp locations2)
  (prin "Part 2: ") (pp (min-of (map |($ :from) locations2))))
