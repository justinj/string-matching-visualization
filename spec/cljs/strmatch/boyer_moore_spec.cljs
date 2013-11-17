(ns strmatch.boyer-moore-spec
  (:require-macros [specljs.core :refer [describe it should=]])
  (:require [specljs.core])
  (:use [strmatch.logic.boyer-moore :only (last-occurrence bad-suffix match)]))

(describe "the boyer-moore matcher"
          (describe "last occurrence array"
                    (it "returns a function that returns the last occurrence of a character"
                        (let [last-occ (last-occurrence "abacaba")]
                          (should= 6 (last-occ "a"))
                          (should= 5 (last-occ "b"))
                          (should= 3 (last-occ "c"))
                          ))
                    (it "returns -1 for things not in the string"
                        (let [last-occ (last-occurrence "abacaba")]
                          (should= -1 (last-occ "z"))
                          )))

          (describe "bad suffix array"
                    (it "has the index of the last occurrence of not the current character, but the same suffix"
                        (should= [-4 -3 -2 -1 -2 3 5]
                                 (bad-suffix "abacaba"))
                        (should= [-3 -2 1]
                                 (bad-suffix "abc")))
                    )

          (describe "matching" 
                    (it "gives the index at each match"
                        (should= 
                          [0]
                          (map :index (:animation (match "abc" "abc"))))
                        (should= 
                          [0 1]
                          (map :index (:animation (match "abc" "aabc"))))
                        (should= 
                          [0 1]
                          (map :index (:animation (match "bb" "abb")))))
                    (it "gives the color at each match"
                        (should= 
                          [[{:color :green, :index 2} {:color :green, :index 1} {:color :green, :index 0}]]
                          (map :colors (:animation (match "abc" "abc"))))
                        )
                    ))
