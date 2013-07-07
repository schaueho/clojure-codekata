(ns kata4-data-munging.core)
(require ['clojure.java.io :as 'io])

(defn string-to-int [string]
  "Parses a consecutive set of numbers into an integer or return nil"
  (try
    (Integer/parseInt (re-find #"\d+" string))
    (catch Exception e nil)))

(defn first-word [string]
  "Returns first consecutive non-whitespace chars from string"
  (re-find #"\S+" string))

 
(defn parse-line [line pattern]
  (let [parts pattern]
    (loop [remkeys (keys parts) linemap {}]
      (if (empty? remkeys)
        linemap
        (let [key (first remkeys)
              [start end parsefn] (get parts key)
              value (parsefn (try
                               (subs line start end)
                               (catch Exception e nil)))]
          (if value
            (recur (rest remkeys)
                   (conj linemap
                         (hash-map key value)))
            ; silently skip any parsing errors
            nil))))))    

(def day-pattern
  ;this pattern is not complete and could be extended
  (hash-map :day [1 4 #(first-word %)]
            :MxT [5 8 #(string-to-int %)]
            :MnT [9 14 #(string-to-int %)]
            :AvT [15 20 #(string-to-int %)]))
                                       
(defn parse-day [line]
  "Parse a day from a line"
  (parse-line day-pattern))

      
(defn find-lowest-temperature [weatherfile]
  "Return day in weatherfile with the smallest temperature spread"
  (loop [lines (line-seq (io/reader weatherfile)) minday 0 minspread 0]
    (if (empty? lines)
      minday
      (let [{mnt :MnT mxt :MxT curday :day} (parse-day (first lines))            
            curspread (when (and mnt mxt) (- mxt mnt))]
        (if (and curday curspread
                 (or (= minspread 0)
                     (< curspread minspread)))
          (recur (next lines) curday curspread)
          (recur (next lines) minday minspread))))))  