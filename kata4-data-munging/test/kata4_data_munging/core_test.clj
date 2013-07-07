(ns kata4-data-munging.core-test
  (:use clojure.test
        kata4-data-munging.core))

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
  (let [weather-file "/home/schauer/programming/clojure/codekata/kata4-data-munging/weather.dat"]
    (is "14" (find-lowest-temperature weather-file))))
