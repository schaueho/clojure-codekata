(ns kata4-data-munging.core)
(require ['clojure.java.io :as 'io])
(require ['clojure.string :as 'string])

(defn substring
  "Returns possibly empty substring from start to end from string"
  [string start end]
  (try
    (subs string start end)
    (catch Exception e "")))

(defn string-to-int
  "Parses a consecutive set of numbers into an integer or return nil"
  [string]
  (try
    (Integer/parseInt (re-find #"\d+" string))
    (catch Exception e nil)))

(defn first-word
  "Returns first consecutive non-whitespace chars from string"
  [string]
  (re-find #"\S+" string))
 
(defn parse-line [line pattern]
  "Parse a line with data in fixed positions using pattern.
Pattern should be a map consisting of a key for the data to return,
a start and end position and a parsing function for each data element.

Returns a map with all extracted data or nil for unparsable lines."
  ; loop solution with accumulator for results
  (loop [remkeys (keys pattern) linemap {}]
    (if (empty? remkeys)
      linemap
      (let [key (first remkeys)
            [start end parsefn] (get pattern key)
            value (parsefn (try
                             (subs line start end)
                             (catch Exception e nil)))]
        (if value
          (recur (rest remkeys)
                 (conj linemap
                       (hash-map key value)))
          ; silently skip any parsing errors
          nil)))))    

(defn parse-line-reduce [line pattern]
  "Parse a line with data in fixed positions using pattern.
Pattern should be a map consisting of a key for the data to return,
a start and end position and a parsing function for each data element.

Returns a map with all extracted data which maybe empty."
  ; map-reduce version
  (reduce #(conj %1 %2)
          (concat [{}]
                (map
                 (fn [[key [start end parsefn]]]
                   {key (parsefn (substring line start end))})
                 (seq pattern)))))

(defn parse-line-map [line pattern]
  "Parse a line with data in fixed positions using pattern.
Pattern should be a map consisting of a key for the data to return,
a start and end position and a parsing function for each data element.

Returns a map with all extracted data which maybe empty."
  (into {}
        (map
         (fn [[key [start end parsefn]]]
           {key (parsefn (substring line start end))})
         (seq pattern))))

(def day-pattern
  ;this pattern is not complete and could be extended
  (hash-map :day [1 4 #(first-word %)]
            :MxT [5 8 #(string-to-int %)]
            :MnT [9 14 #(string-to-int %)]
            :AvT [15 20 #(string-to-int %)]))
                                       
(defn parse-day [line]
  "Parse a day from a line"
  (parse-line line day-pattern))
      
(defn find-lowest-temperature
  "Return day in weatherfile with the smallest temperature spread"
  [weatherfile]
  (with-open [rdr (io/reader weatherfile)]
    (loop [lines (line-seq rdr) minday 0 minspread 0]
      (if (empty? lines)
        minday
        (let [{mnt :MnT mxt :MxT curday :day} (parse-day (first lines))            
              curspread (when (and mnt mxt) (- mxt mnt))]
          (if (and curday curspread
                   (or (= minspread 0)
                       (< curspread minspread)))
            (recur (next lines) curday curspread)
            (recur (next lines) minday minspread)))))))  

(defn abs 
  "Returns the absolute value of x" 
  [x]
  (if (pos? x) x (- x)))

(def soccer-team-pattern
  ; this pattern is not complete
  (hash-map :pos [1 5 #(first-word %)]
            :team [7 22 #(first-word %)]
            :fval [43 45 #(string-to-int %)]
            :aval [50 52 #(string-to-int %)]))

(defn parse-soccer-team
  "Parse a soccer-team from a line"
  [line]
  (parse-line line soccer-team-pattern))

(defn find-minimum-goal-difference 
  "Return team in soccerfile with the smallest difference in for and against goals"
  [soccerfile]
  (with-open [rdr (io/reader soccerfile)]
    (loop [lines (line-seq rdr) minteam 0 mindiff 0]
      (if (empty? lines)
        minteam
        (let [{aval :aval fval :fval curteam :team} 
              (parse-soccer-team (first lines))            
              curdiff (when (and aval fval) (abs (- fval aval)))]
          (if (and curteam curdiff
                   (or (= mindiff 0)
                       (< curdiff mindiff)))
            (recur (next lines) curteam curdiff)
            (recur (next lines) minteam mindiff)))))))

(defn find-some-difference 
  "Return some result from a data file which has some lowest difference"
  [filename parse-pattern resultkey diffn]
  (with-open [rdr (io/reader filename)]
    (loop [lines (line-seq rdr)
           result nil
           mindiff 0]
      (if (empty? lines)
        result
        (let [data-map (parse-line-map (first lines) parse-pattern)
              curresult (get data-map resultkey)
              curdiff (diffn data-map)]
          (if (and curresult curdiff
                   (or (= mindiff 0)
                       (< curdiff mindiff)))
            (recur (next lines) curresult curdiff)
            (recur (next lines) result mindiff)))))))

(defn find-mingoal-diff-fusion
  "Return team in soccerfile with the smallest goal difference, using the fusion fn."
  [soccerfile]
  (find-some-difference soccerfile soccer-team-pattern :team
                        (fn [{aval :aval fval :fval curteam :team}]
                          (when (and aval fval)
                            (abs (- fval aval))))))


(defn sort-diff-map
  "Return some result from a data file which has some lowest difference"
  [filename parse-pattern desiredkeys diffn]  
  (sort-by diffn 
           (filter #(every? (partial get %) desiredkeys)
                   (map #(parse-line-map % parse-pattern)
                        (string/split-lines (slurp filename))))))

(defn find-mingoal-map
  "Return team in soccerfile with the smallest goal difference, using the sort-map fn."
  [soccer-file]
  (get (take 1 
             (sort-diff-map soccer-file soccer-team-pattern 
                            [:team :fval :aval]
                            (fn [{aval :aval fval :fval curteam :team}]
                              (when (and aval fval)
                                (abs (- fval aval))))))
       :team))
