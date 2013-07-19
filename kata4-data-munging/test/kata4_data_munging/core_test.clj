(ns kata4-data-munging.core-test
  (:use clojure.test
        kata4-data-munging.core))

(def weather-file
  "j:\\Diverses\\programming\\clojure\\codekata\\kata4-data-munging\\weather.dat")
 ;  "/home/schauer/programming/clojure/codekata/kata4-data-munging/weather.dat")

(def soccer-file
  "j:\\Diverses\\programming\\clojure\\codekata\\kata4-data-munging\\football.dat")
  ; "/home/schauer/programming/clojure/codekata/kata4-data-munging/football.dat")

(deftest string-to-int-returns-number
  (is (= 1 (string-to-int "  1")))
  (is (= 99 (string-to-int "  99 abcde"))))

(deftest string-to-int-returns-nil-without-number
  (is (= nil (string-to-int "  a"))))

(deftest first-word-returns-non-whitespace-from-string
  (is (= "foo" (first-word "  foo bar "))))

(deftest parse-day-generates-data
  (let [testline
        "   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5"]
 (is {:AvT 74, :day "1", :MxT 88, :MnT 59} (parse-day testline))))

(deftest parse-day-returns-nil-for-nonday-data
  (is (= nil (parse-day "")))
  (is (= nil (parse-day "This is some weird line"))))

(deftest find-lowest-temperature-yields-result
  (is "14" (find-lowest-temperature weather-file)))

(deftest parse-team-generates-data
  (let [testline
        "    1. Arsenal         38    26   9   3    79  -  36    87"]
    (is {:aval 36, :pos "1", :team "Arsenal", :fval 79}
        (parse-soccer-team testline))))

(deftest team-with-smallest-diff-returns-result
  (is "Aston Villa" (find-minimum-goal-difference soccer-file)))

(deftest parse-line-map-day-pattern-returns-day
  (let [testline
        "   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5"]
 (is {:AvT 74, :day "1", :MxT 88, :MnT 59} (parse-line-map testline day-pattern))))
  

(deftest team-with-smallest-diff-with-fusion-returns-result
  (is "Aston Villa" (find-mingoal-diff-fusion soccer-file)))


(deftest team-with-smallest-diff-map-returns-result
  (is "Aston Villa" 
      (find-mingoal-map soccer-file)))
