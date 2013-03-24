(ns kata2-chop-binary-search.core-test
  (:use clojure.test
        kata2-chop-binary-search.core))

(deftest chop-returns-minus-one-if-value-not-found
  (is (= -1 (chop(3 []))))
  (is (= -1 (chop(3 [1])))))
  ;; (is (= -1 (chop(0 [1, 3, 5]))))
  ;; (is (= -1 (chop(2 [1, 3, 5]))))
  ;; (is (= -1 (chop(4 [1, 3, 5]))))
  ;; (is (= -1 (chop(6 [1, 3, 5]))))
  ;; (is (= -1 (chop(0 [1, 3, 5, 7]))))
  ;; (is (= -1 (chop(2 [1, 3, 5, 7]))))
  ;; (is (= -1 (chop(4 [1, 3, 5, 7]))))
  ;; (is (= -1 (chop(6 [1, 3, 5, 7])))))

(deftest chop-returns-position-if-found
  (is (= 0 (chop(1 [1])))))

  ;; (is (= 0 (chop(1 [1, 3, 5]))))
  ;; (is (= 1 (chop(3 [1, 3, 5]))))
  ;; (is (= 2 (chop(5 [1, 3, 5]))))

  ;; (is (= 0 (chop(1 [1, 3, 5, 7]))))
  ;; (is (= 1 (chop(3 [1, 3, 5, 7]))))
  ;; (is (= 2 (chop(5 [1, 3, 5, 7]))))
  ;; (is (= 3 (chop(7 [1, 3, 5, 7])))))

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
