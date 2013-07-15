# Introduction to kata4-data-munging

Kata4 is concerned with data munging, basically reading some file, pulling out some data and comparing the data in order to determine some result. Actually, it's even simpler than that as in both cases we're asked to determine the minimum of the data set. Given a weather task and a soccer task, we're asked to fuse the resulting solutions and extract the commonalitites and minimize both solutions, basically allowing for code-reuse as much as possible.

Unfortunately, I hadn't read the opening sentences of the kata closely, instead I read over the entire description. Hence I missed the call to solve each part separately and not to read ahead and directly started with this idea of code re-use in mind. However, as we will see, I didn't really notice one aspect of code reuse until I solved the second task (soccer).

The first thing I did was take a closer look at the data files. Probably as much as everybody else who had some exposure to Perl, I was initially tempted to approach the data extraction part with regular expressions. But as JWZ said, "some people when faced with a problem say "I know, I'll use regular expressions. Now they have two problems", and indeed this is the case here. This is not to say that the particular problem, fetching the date, minimum and maximum temperature from the provided data file would not be solvable with regular expressions, but more that the data file is really more of a fixed width nature. Unfortunately, there are some irregular elements, e.g. the missing values in the WxType row or the post-fix '*' added to exactly one value in both MnT and MxT rows, likely to mark the absolute minimum and maximum temperature of the month, respectively, which causes the data in the corresponding "cell" to hang-over into the white-space pre-fix area of the next row. The last column is also of interest, as it starts out with "mo" in contrast to the numeric values for the days of the month. It also has rational values for some rows (e.g. for the temperature rows) where all other values have integer values. This looks like a computed result line, showing the averages of the respective values for the entire month and should hence not be part of the computation. And finally, of course, the data file has some completely irrelevant lines, which we need to skip over.

Having come to some idea of how the data file was structured, I came to the conclusion to parse each line using exact positions, e.g. to determine the MnT value by extracting the substring of each line from position 9 to 14. Let me sketch out the top-down approach I had in mind with some pseudocode in pseudo Common Lisp:

       (pseudocl-defun find-lowest-temperature-diff (weatherfile)
         (let ((pattern some-pattern-specification))
	   (with-open-file (file weatherfile :direction :read)
	      (loop with line from file
	            for each part in pattern
		         collect (parse-line line part) into result)
		    finally result)))

So, the idea is basically, we'll open a file and for each line in it, we'll try to parse it according to some pattern specification. The first stumbling block I had to jump over was the question on how to open a file and read a line from it. I quickly found the 'with-open' macro which abstracts the safe file handling to the next level in that it's not restricted to files. However, 'with-open' does not simply work with a filename as an argument, you need to pass in a resource which follows the open/close protocoll which a simple filename string doesn't. That Clojure leverages the Java IO library for this is not surprising, but that it leaks this Java dependency to the users is. I assume that this implies that '(with-open (clojure.java.io/reader "/some/filename.txt"))' will not work on ClojureCLR. Well, apparently 'slurp' will work on the standard Java-based Clojure as well as on ClojureCLR, but I wanted to stick to the idea outlined above. Having found out about how to open a file, the next thing was how to read a line from it. 'read-line' is a false friend for a Common Lisp programmer as it's really intended for reading a line from the REPL, in contrast to be useful for reading from some stream like in CL. While I appreciate the possibility to call Java methods from Clojure, I prefer using any abstractions that Clojure provides, so I went with 'line-seq' instead of calling '.read'. Finally, Clojure does not allow for re-assignment, so the 'collect into result' from CL's loop macro needs to be replaced with an recursive solution with an accumulator. So, the skeleton looks something like this in Clojure:


    (defn find-lowest-temperature-skeleton
      "Return day in weatherfile with the smallest temperature spread"
         [weatherfile]
	   (loop [lines (line-seq (io/reader weatherfile)) result]
	       (if (empty? lines)
	             result
		  ; todo: compare the result of parsing a line with the current 'best' 
		  ; i.e. minimum spread value so far and recur, possibly with different data
                  (recur (next lines) result))))))

This is more or less the equivalent of Perl's '-n' command-line switch or 'while (<>) {...}' construct.

The comparison is actually very easy and what needs to be put in place for the result is also obvious. But in order to do that we finally have to parse the lines. Following up on the idea of how to parse a line, the relevant function is 'subs' which returns a substring from start to end. Now, we basically need to parse different substrings for the various data fields, quite often returning just an integer. Again, the simplest way of parsing a string to an integer is provided by a thin layer on top of a Java library. Neither  the re-find nor the exception handling would be necessary if we could guarantee that the data would always only consist of correct data, this way we just silently skip over invalid data.

    (defn string-to-int
      "Parses a consecutive set of numbers into an integer or return nil"
      [string]
      (try
          (Integer/parseInt (re-find #"\d+" string))
	  (catch Exception e nil)))

'parse-line' is fairly straight-forward: the basic idea is that we have some specification of how a line is structured. This specification is a map of start and end positions plus parsing functions. So for the weather data, the pattern looks like this:

     (def day-pattern
       ;this pattern is not complete and could be extended
       (hash-map :day [1 4 #(first-word %)]
                 :MxT [5 8 #(string-to-int %)]
                 :MnT [9 14 #(string-to-int %)]
                 :AvT [15 20 #(string-to-int %)]))

'first-word' is another small helper function which basically just retrieves the first continous non-whitespace characters of a string:

     (defn first-word
       "Returns first consecutive non-whitespace chars from string"
       [string]
       (re-find #"\S+" string))

'parse-line' than just loops over all parts of a pattern, extracts the substrings and calls the parsing function. It recursively 'conj'ures up a hash-map with the extracted data or returns 'nil' if some parsing error occurs.

(defn parse-line [line pattern]
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
            nil))))))

This then allows us to put things together: we only need to compare the difference between MxT and MnT of the current line with the previous smallest temperature spread. We'll use destructuring of the result of hte 'parse-day' results to retrieve the needed data, sprinkle in some sanity checks and are done.

     (defn parse-day [line]
       "Parse a day from a line"
        (parse-line line day-pattern))

     (defn find-lowest-temperature
       "Return day in weatherfile with the smallest temperature spread"
       [weatherfile]
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





    

    

