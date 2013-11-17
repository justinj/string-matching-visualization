(ns strmatch.kmp-matcher-spec 
  (:require-macros [specljs.core :refer [describe it should=]])
  (:require [specljs.core])
  (:use [strmatch.logic.kmp-matcher :only (failure-array match)]))


(describe "the kmp matcher"
          (for [[expected input]
                [[[-1] "a"]
                 [[-1 0] "ab"]
                 [[-1 0 0 1] "abaa"]
                 [[-1 0 0 0 0 1 2] "abcdabd"]]]
            (it "calculates the failure array"
                (should= expected (failure-array input))))

          (it "shows each stage of the matching process"
              (should=
                [0 1]
                (map :index (:animation (match "abc" "aabc"))))
              (should=
                [0 2 3]
                (map :index (:animation (match "abc" "abdabc"))))
              (should=
                [0 3 4 8 10 11 15]
                (map :index (:animation (match "abcdabd" "abc abcdab abcdabcdabde")))))
          (it "highlights the letters comparisons were done against"
              (should=
                [[{:color :green, :index 0} {:color :green, :index 1} {:color :green, :index 2} {:color :red, :index 3}]]
                (map :colors (:animation (match "abc" "abc")))))
          )
