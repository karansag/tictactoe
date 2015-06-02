(ns tictactoe.core
  (:gen-class))

(defn print-flush [& args] (apply print args) (flush))

(def entry->draw {:x \X :o \O nil \space})

(defn row->draw [r]
  (apply str (interpose " | " (map entry->draw r))))

(defn board->draw [board]
  (let [[f s t] (map row->draw board)
        sample-length (.length ^String f)
        row-sep (clojure.string/join (take sample-length (repeat "-")))]
    (apply str (interpose \newline [f row-sep s row-sep t]))))

(defn win? [[[a b c]
             [d e f]
             [g h i]]]
  (let [three? (fn [x y z] (if (= x y z) x))]
    (some identity (map (partial apply three?) [[a b c] [d e f] [g h i]
                                                [a d g] [b e h] [c f i]
                                                [a e i] [c e g]]))))

(defn full? [board] (every? identity (flatten board)))

(defn compute-player [turn] (if (even? turn) :x :o))

(defn parse-move [^String line]
  (try
    (doall (map #(Integer/parseInt %) (clojure.string/split line #" ")))
    (catch NumberFormatException _ [-1 -1])))

(defn in-bounds? [[x-move y-move]]
  (let [allowed (vec (range 3))] (and (contains? allowed x-move) (contains? allowed y-move))))

(defn read-move [board]
  (let [intended-move (parse-move (read-line))]
    (cond (not (in-bounds? intended-move)) (do (print-flush "That spot is not a valid one :(. Enter again: ") (recur board))
          (get-in board intended-move) (do (print-flush "That spot is already taken. Enter again: ") (recur board))
          :else intended-move)))

(defn print-winner [winner]
  (let [garnish (take 20 (repeat "#"))]
    (apply println garnish) (println (entry->draw winner) " wins!") (apply println garnish)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Welcome to tictactoe. Enter a move by entering a row and column pair separated by a space."
           "Coordinates start at the top left corner and are 0-indexed.")
  (loop [board [[nil nil nil] [nil nil nil] [nil nil nil]] turn 0]
    (newline) (println (board->draw board)) (newline)
    (if (full? board) (do (println "The board is full. Tie!") (System/exit 0)))
    (if-let [winner (win? board)] (do (print-winner winner) (System/exit 0))
                              (let [p (compute-player turn)]
                                (do (print-flush "Enter move for" (str (entry->draw p) ": "))
                                    (let [move (read-move board)]
                                      (recur (assoc-in board move p) (inc turn))))))))
