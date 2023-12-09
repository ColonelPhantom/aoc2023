(def network-peg
  ~{:instr ':a+
    :node (/ (* ':w+ " = (" ':w+ ", " ':w+ ")\n"), |{:label $0 :left $1 :right $2})
    :main (* :instr "\n\n" (some :node))})

(defn network [s]
  (def @[instr & nodes] (peg/match network-peg s))
  {:instr instr :nodes (tabseq [{:label l :left left :right right} :in nodes] l {:left left :right right})})

(comment 
  (def input (file/read (file/open "inputs/day08.test") :all))
  (network input))

(defn camel [{:instr instr :nodes nodes}]
  (var location "AAA")
  (sum (generate [_ :iterate true :until (= location "ZZZ")
                  i :in instr
                    :until (= location "ZZZ")]
         (printf "Now in state %p" location)
         (set location (case (string/from-bytes i)
                         "L" ((nodes location) :left)
                         "R" ((nodes location) :right)))
         1)))

(defn camel2 [loc {:instr instr :nodes nodes}]
  (var location loc)
  (sum (generate [_ :iterate true :until (= (string/slice location -2) "Z")
                  i :in instr
                    :until (= (string/slice location -2) "Z")]
         (printf "Now in state %p" location)
         (set location (case (string/from-bytes i)
                         "L" ((nodes location) :left)
                         "R" ((nodes location) :right)))
         1)))

(defn ghost [{:instr instr :nodes nodes}]
  (def starting (filter |(= 65 (last $)) (keys nodes)))
  (map |(camel2 $ {:instr instr :nodes nodes}) starting))
  # (sum (generate [_ :iterate true :until (done)
  #                 i :in instr :until (done)]
  #        (printf "Now in states %p" locations)
  #        (set locations (case (string/from-bytes i)
  #                         "L" (map |((nodes $) :left) locations)
  #                         "R" (map |((nodes $) :right) locations)))
  #        1)))


  
           
                  

(defn main [& args]
  (def input (file/read stdin :all))
  (def net (network input))
  (pp net)
  # (prin "Part 1: ") (pp (camel net))
  (prin "Part 2: ") (pp (ghost net)))

