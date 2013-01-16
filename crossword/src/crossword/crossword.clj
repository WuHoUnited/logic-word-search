(ns ^{:doc "Tiny library using logit programming to put words into a crossword/wordsearch style board (or vice versa)
            There are lots of things it currently doesn't do such as revers or diagonal words.
            Reverse words would be trivial to add. Diagonals would be harder."
       :author "Zachary Miller"}
  crossword.crossword
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

;;;; a board will be a 2 dimensional sequence of characters


(comment " Some of the \"relations\" like mapo, and someo are written clarity"
         "For instance a better definition for mapo may look like"
         (defn mapo [relation]
           (fn m [collection result]
             (matche [collection result]
                     ([[] []])
                     ([[c . cs] [r . rs]]
                        (relation c r)
                        (m cs rs))))))

(defne mapo
  "mapping the relation over collection is result"
  [relation collection result]
  ([_ [] []])
  ([_ [c . cs] [r . rs]]
     (relation c r)
     (mapo relation cs rs)))

(defne someo
  "(relation x) holds for some x in coll"
  [relation coll]
  ([_ [c . _]] (relation c))
  ([_ [_ . cs]] (someo relation cs)))

(defne everyo
  "(relation x) holds for every x in coll"
  [relation coll]
  ([_ []])
  ([_ [c . cs]] (relation c) (everyo relation cs)))

(defn prefixo
  "pre is a prefix of coll"
  [coll pre]
  (fresh [e]
         (appendo pre e coll)))

(defne line-wordo
  "the line contains the word"
  [line word]
  ([_ _] (prefixo line word))
  ([[_ . ls] _] (line-wordo ls word)))

(defn first-columno
  "the first column of matrix is coll"
  [matrix column]
  (mapo firsto matrix column))

(defn transposeo
  "The transpose of matrix-1 is matrix-2.
   This is NOT symetric in the empty case.
   Should probably find a way to make it symetric"
  [matrix-1 matrix-2]
  (conde
   [(emptyo matrix-1) (everyo emptyo matrix-2)]
   [(fresh [m1-row m2-col m1-rest-rows m2-rest-cols] 
           (== m1-row m2-col)
           (firsto matrix-1 m1-row)
           (first-columno matrix-2 m2-col)
           (resto matrix-1 m1-rest-rows)
           (mapo resto matrix-2 m2-rest-cols)
           (transposeo m1-rest-rows m2-rest-cols))]))

(defn horizontalo
  "board contains word horizontally"
  [board word]
  (someo (fn [line]
           (line-wordo line word)) board))

(defn verticalo
  "board contains word vertically"
  [board word]
  (fresh [transposed]
         (transposeo transposed board)
         (horizontalo transposed word)))

(defn board-containso
  "board contains word vertically or horizontally"
  [board word]
  (conde [(horizontalo board word)]
         [(verticalo board word)]))

(defn word-searcho
  "board contains all the words"
  [board words]
  (everyo (fn [word]
            (board-containso board word))
          words))


;;;;We're done with the logic part.
;;;;Now we just need to do some formatting for input/output

(defn prettify-board
  "converts a board with reified symbols and chars into a sequence of strings"
  [board]
  (map (fn [row]
         (->> row
              (map (fn [char]
                     (if (char? char)
                       char
                       \space)))
              (apply str))) board))

(defn to-string
  "neatly print a sequence of strings"
  [board]
  (clojure.pprint/cl-format nil "~%~{~a~%~}" board))

(defn generate-matrix
  "makes a 2-dimensional matrix of lvars"
  [width height]
  (partition width (repeatedly (* width height) lvar)))


;;;; Now actually solve some problems

(defn find-boards
  "convenience function for generating the boards given the words"
  [width height words]
  (->> (run* [q]
             (== q (generate-matrix width height))
             (word-searcho q (map seq words)))
       (map prettify-board)
       (map to-string)))


;;;;The next 2 problems find the ways to put words into a blank board
(->> (find-boards 4 3 (map seq ["dog" "food" "rod" "fad"]))
     (map print)
     dorun)

(->> (find-boards 3 4 (map seq ["eft" "food" "rod" "add" "err"]))
     (map print)
     dorun)


;;;;This problem finds out how to put words into a partially filled board
(let 
    [words ["car" "can" "arc"]
     board (->> ["---"
                 "---"
                 "-.."]
                (map (fn [string]
                       (map (fn [char]
                              (if (= \- char)
                                (lvar)
                                \space))
                            string))))]
  (->> (run 4 [q]
            (== q board)
            (word-searcho q (map seq words)))
       (map prettify-board)
       (map to-string)
       (map print)
       dorun))