# Introduction to kata4-data-munging

[Kata4](http://codekata.pragprog.com/2007/01/kata_four_data_.html) is concerned with data munging, basically reading some file, pulling out some data and comparing the data in order to determine some result. Actually, it is even simpler than that as in both cases we are asked to determine the minimum of the data set. Given a weather task and a soccer task, we are asked to fuse the resulting solutions and extract the commonalitites and minimize both solutions, basically allowing for code-reuse as much as possible. Unfortunately, I had not read the opening sentences of the kata closely, instead I read over the entire description. Hence I missed the call to solve each part separately and not to read ahead and directly started with this idea of code re-use in mind. However, as we will see, I didn't really notice one aspect of code reuse until I solved the second task (soccer).

The first thing I did was take a closer look at the [data file](http://pragdave.pragprog.com/data/weather.dat). Probably as much as everybody else who had some exposure to Perl, I was initially tempted to approach the data extraction part with regular expressions. But as JWZ said, "some people when faced with a problem say "I know, I'll use regular expressions. Now they have two problems", and indeed this is the case here. This is not to say that the particular problem, fetching the date, minimum and maximum temperature from the provided data file would not be solvable with regular expressions, but more that the data file is really more of a fixed width nature. Unfortunately, there are some irregular elements, e.g. the missing values in the WxType row or the post-fix '*' added to exactly one value in both MnT and MxT rows, likely to mark the absolute minimum and maximum temperature of the month, respectively, which causes the data in the corresponding "cell" to hang-over into the white-space pre-fix area of the next row. The last column is also of interest, as it starts out with "mo" in contrast to the numeric values for the days of the month. It also has rational values for some rows (e.g. for the temperature rows) where all other values have integer values. This looks like a computed result line, showing the averages of the respective values for the entire month and should hence not be part of the computation. And finally, of course, the data file has some completely irrelevant lines, which we need to skip over.

Having come to some idea of how the data file was structured, I came to the conclusion to parse each line using exact positions, e.g. to determine the MnT value by extracting the substring of each line from position 9 to 14. So, the top-down approach is like this: we will open a file and for each line in it and we will try to parse it according to some pattern specification where the pattern specification would specify the positions of some part and a parsing function.

Clojures approach to file handling is a little bit surprising for somebody coming from Common Lisp, which provides three main concepts you need to grasp: filenames aka pathnames, files and streams. Typically, you open a file with `with-open-file` which will guarantee that the file will be closed after you leave the block. Clojures `with-open` macro abstracts this idea of safe file handling to the next level in that it is not restricted to files. However, `with-open` does not simply work with a filename as an argument, you need to pass in a resource which follows the open/close protocoll which a simple filename string does not. That Clojure leverages the Java IO library for this is not surprising, but that it leaks this Java dependency to the users is. I assume this implies that `(with-open (clojure.java.io/reader "/some/filename.txt"))` will not work on ClojureCLR (apparently [`slurp`](http://clojuredocs.org/clojure_core/clojure.core/slurp) does). `read-line` is also a false friend for a Common Lisp programmer, as it can only be used for reading a line from the REPL but not for reading from some stream like in CL. While I appreciate the possibility to call Java methods from Clojure, I prefer using any abstractions that Clojure provides, so I went with `line-seq` instead of calling `.read`. Finally, as Clojure does not allow for re-assignment, so we need a recursive approach with an accumulator to hold intermediate results while looping over the lines. So, the skeleton looks something like this in Clojure:


	 (defn read-some-file
	   "Skeleton for reading some file"
	   [filename]
	   (with-open [rdr (io/reader filename)]
	      (loop [lines (line-seq rdr) result]
	        (if (empty? lines)
	             result
		  ; todo for the weather task: 
		  ; compare the result of parsing a line with the current 'best' 
		  ; i.e. minimum spread value so far and recur, possibly with different data
                  (recur (next lines) result)))))))

This is more or less the equivalent of Perl's `-n` command-line switch or `while (<>) {...}` construct.

The comparison mentioned in the code's comment is actually very easy and what needs to be put in place for the result is also obvious. But in order to do that we have to parse the lines first. Following up on the idea of how to parse a line, the relevant function is `subs` which returns a substring from start to end. Now, we basically need to parse different substrings for the various data fields, quite often returning just an integer. Again, the simplest way of parsing a string to an integer is provided by a thin layer on top of a Java library. Neither  the `re-find` nor the exception handling would be necessary if we could guarantee that the data would always only consist of correct data, this way we just silently skip over invalid data.

	(defn string-to-int
	  "Parses a consecutive set of numbers into an integer or return nil"
	  [string]
	  (try
	    (Integer/parseInt (re-find #"\d+" string))
	    (catch Exception e nil)))
	
`parse-line` is fairly straight-forward: the basic idea is that we have some specification of how a line is structured. This specification is a map of start and end positions plus parsing functions. So for the weather data, the pattern looks like this:

	(def day-pattern
	  ;this pattern is not complete and could be extended
	  (hash-map :day [1 4 #(first-word %)]
	            :MxT [5 8 #(string-to-int %)]
	            :MnT [9 14 #(string-to-int %)]
	            :AvT [15 20 #(string-to-int %)]))
	
`first-word` is another small helper function which basically just retrieves the first continous non-whitespace characters of a string:

	(defn first-word
	  "Returns first consecutive non-whitespace chars from string"
	  [string]
	  (re-find #"\S+" string))
	
`parse-line` than just loops over all parts of a pattern, extracts the substrings and calls the parsing function. It recursively `conj`ures up a hash-map with the extracted data or returns `nil` if some parsing error occurs.

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
	
This then allows us to put things together: we only need to compare the difference between MxT and MnT of the current line with the previous smallest temperature spread. We will use destructuring of the result of hte `parse-day` results to retrieve the needed data, sprinkle in some sanity checks and are done.

	(defn parse-day
	  "Parse a day from a line"
	  [line]
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

When I started working on the second task, solving the soccer issue, I did a simple copy and paste of the `find-lowest-temperature`, added a new pattern for extracting the data and made the small changes to adapt to the different fields. I also understand the comparison requirement to look at the absolute difference.
This leads to the following functions:

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
		
This, of course, led straight to the insight that it should be simple to extract the slight differences and make them parameters to some `find-*-difference` function. The following things are differently: the parsing pattern, the extraction function for the result value and the function used to compute the difference between values. If you would want to it would also be possible to make the comparison function configurable. 

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


There there was another itch I wanted to scratch: the `parse-line` function has some ugliness to it. For starters, it is handling possible exceptions from `subs` directly. It is also checking return values for `nil`. Both cases are what Common Lisp would see as [conditions](http://www.nhplace.com/kent/Papers/Condition-Handling-2001.html) rather than real exceptions -- it is rather unfortunate that Clojure opted for the more simple, although more traditional exception concept from Java. To remedy the uglyness of `parse-line` we can simply replace the direct call to `subs` with a small handcrafted call which manages any exceptions and also change the behavior of `parse-line` to simply return `nil` for all unparsable elements. But there is more that makes `parse-line` ugly: I dislike the recursive nature of the solution and the linear result handover in the `let` declaration (well, this handover was intentional to not have a functional train-wreck of calls). I wanted to see whether I couldn't come up with a more elegant `map/reduce` solution. Here you go:

	(defn substring
	  "Returns substring from start to end from string or nil"
	  [string start end]
	  (try
	    (subs string start end)
	    (catch Exception e "")))

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
	
We are simply mapping over the entire pattern and use argument destructuring again to extract the relevant parts of it, but this time, due to the call to `seq` a pattern part will be a sequence, not a map. The map call with the anonymous function will produce a sequence of hashmaps with key and parsing results, which then gets reduced to a single map. In order to use `reduce`, you have to provide a function taking two arguments: the first will consume the intermediate result, the second will be the next value of the sequence to reduce. This is the reason why we have this ugly `concat [()] ...` in front of the call to `map`: we need to provide the initial value for `reduce` which in this case is an empty hashmap. An even more concise version replaces the call to `reduce` with `into`, resulting in a version which looks pretty idiomatic to me and is also way easier to understand then the lengthy recursive version above.

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
		

Summing up, what have we seen during this kata? Clojure's platform dependent approach to reading files, usage of regular expressions, destructuring (again), anonymous functions (again) and map/reduce. Overall not very exciting. I know it's not a fair comparison, but I would always opt for solving such tasks with Perl, especially if they are so trivial as in this case in which you can solve each task with a one-liner, basically. There is room for using more elaborate languages (e.g. Python, Ruby, Clojure) if parsing and processing become so elaborate that it makes sense to have more structure in the code. But for a task of the size of this kata, the amount of code required is usually not worth the effort.

    

