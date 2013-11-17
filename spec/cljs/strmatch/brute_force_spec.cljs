(ns strmatch.brute-force-spec
  (:require-macros [specljs.core :refer [describe it should=]])
  (:require [specljs.core])
  (:use [strmatch.logic.brute-force :only (match)]))

(describe "the naive matcher"
          (it "checks against each alignment"
              (should=
                [0 1 2 3]
                (map :index
                     (:animation(match "abc"
                            "aaaabc"))))))
