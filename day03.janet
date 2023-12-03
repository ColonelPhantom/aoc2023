(defn create-part [line column number endcolumn]
  {:line (- line 1)        # subtract 1 because slice is 0-indexed
   :column (- column 1)    # but PEG line and column are 1-indexed
   :endcol (- endcolumn 1)
   :number number})
(def partnum
  ~{:number (/ (* (line)       # capture current line
                  (column)     # and first column
                  (number :d+) # as well as the number
                  (column))    # and the column after the number
               ,create-part)   # then put those in a struct
    :main (any (* (any :D) :number))}) # find all numbers

(defn create-gear [line column] {:line (- line 1) :column (- column 1)})
(def gear
  ~{:gear (/ (* (line) (column) "*") ,create-gear)
    :main (any (* (any (if-not "*" 1)) :gear))})

(defn safe-slice
  "Performs slicing safely, by clamping indices to valid (positive) ones."
  "Note that this slice function is inclusive."
  [x start end]
  (defn clamp [i] (max 0 (min i (length x))))
  (slice x (clamp start) (clamp (+ end 1))))

(defn has-symbol [part schematic]
  "Checks if the part has a symbol adjacent and is therefore valid"
  (def symb
    ~{:SYMB (+ :d ".")              # anything that's not a digit or . is a symbol
      :symb (if-not :SYMB 1)        # everything else is!
      :main (* (any :SYMB) :symb)}) # try to find a symb
  (def {:line line :column column :endcol endcol} part) # destructure part into line etc
  (def lines (safe-slice schematic (- line 1) (+ line 1)))   # only neighboring lines
  (def cols (map |(safe-slice $ (- column 1) endcol) lines)) # only neighboring cols
  (some |(peg/match symb $) cols))  # if any lin+col has a symb return true

(defn part-schema
  "Creates a table from coordinates to the number located there"
  [parts]
  (tabseq [part :in parts                              # for every part
           col :range [(part :column) (part :endcol)]] # and every column it occupies
    {:y col :x (part :line)} part))                    # associate coord with part

(defn process-gear [{:line line :column column} part-table]
  (def numbers @{}) # use a hashmap for numbers, automatically dedupes on identity
  (seq [x :range-to [(- line 1) (+ line 1)]       # for every neighbouring x
        y :range-to [(- column 1) (+ column 1)]]  # and y coordinates:
    (def part (part-table {:x x :y y}))             # find the number there
    (if part (put numbers part (part :number))))    # if it exists, add it to hashmap
  (if (= 2 (length numbers))  # check that gear has 2 numbers
    (product numbers)         # product multiplies values inside hashmap
    0))                       # return 0 for invalid so it won't affect sum

(defn main [& args]
  (def input (file/read stdin :all))                        # read all input
  (def schematic (string/split "\n" input))                 # split into lines for 2d
  (def parts (peg/match partnum input))                     # use PEG to find numbers
  (def valids (filter |(has-symbol $ schematic) parts))     # check for symbols
  (prin "Part 1: ") (pp (sum (map |($ :number) valids)))    # sum valid numbers

  (def gears (peg/match gear input))                        # use PEG to find gear pos
  (def part-table (part-schema valids))                     # store valid numbers in tbl
  (def gear-vals (map |(process-gear $ part-table) gears))  # find gear values
  (prin "Part 2: ") (pp (sum gear-vals)))                   # sum gear values

