(defun flatten (structure)
  "Transforms any arbitrarily nested list into a flat list"
  (cond ((null structure) nil)
	((atom structure) (list structure))
	(t (mapcan #'flatten structure))))

(defun tack (place obj)
  "Tacks place at the end of obj, returns a proper list"
  (if (and
       (not (listp place))
       (not (listp obj)))
      (list place obj)
      (if (not (listp place))
	  (append (list place) obj)
	  (append place (cons obj nil)))))

(defmacro ntack (place obj)
  "Destructive version of tack"
  `(setf ,place (tack ,place ,obj)))

(defun grow-list! (sequence amount &optional (initial-element 0.0))
  "Destructively appends n additional elements to the end of the given sequence, with given initial element. Returns said sequence."
  (nconc sequence (make-list amount :initial-element initial-element)))

(defun map-over! (sequence function)
  "Destructively applies the given function to each element in the given sequence, returns said sequence."
  (map-into sequence function sequence))

(defun last1 (sequence)
  "Returns the last element of a sequence"
  (car (last sequence)))

(defun same-at-index? (sequence1 sequence2 index)
  "Returns true if the elements at the given index of each sequence are equal."
  (equal (elt sequence1 index) (elt sequence2 index)))

(defun get-element-before-branch (sequence1 sequence2)
  "Scans two lists for their first difference, and returns the item before that.
Eg: (get-element-before-branch '(1 2 6 2) '(1 2 4 9)) => 2"
  (let ((last-common nil))
    (loop for i from 0 to (1- (shortest-length sequence1 sequence2)) do
          (if (same-at-index? sequence1 sequence2 i)
              (setf last-common (elt sequence1 i))
              (return)))
    last-common))

(defun add-margin (box margin)
  "Add some margin to a list of 4 elements, being the x, y, width and height of the box"
  (let ((half-margin (/ margin 2)))
    (list
     (- (elt box 0) half-margin)
     (- (elt box 1) half-margin)
     (+ (elt box 2) margin)
     (+ (elt box 3) margin))))

(defun range (length &optional (start-from 0))
  "Creates a list from start-from to length"
  (declare (fixnum length))
  (loop for i upto (- length 1) nconc (list (+ i start-from))))

;; (defun my-range (length &optional (start-from 0))
;;   (declare (fixnum length))
;;   (declare (fixnum start-from))
;;   (declare (optimize (speed 3) (safety 0)))
;;   (loop for i upto (1- length)
;;         collect (+ i start-from)))

(defun range (length &optional (start-from 0))
  "Creates a list from start-from to length"
  (declare (fixnum length))
  (loop for i upto (- length 1)
        append (list (+ i start-from))))

(defun range-from-list (sequence &optional (start 0))
  "Returns a list of numbers starting from given start to the length of the given sequence. (range-from-list '(2 4 5 7)) => (0 1 2 3)."
  (range (length sequence) start))

(defun shuffle (list)
  "Returns a random permutation of list"
  (let ((temp (copy-tree list)))
    (loop for i from (length temp) downto 2
	  do (rotatef (elt temp (random i))
		      (elt temp (1- i))))
    temp))

;(defun get-unicode (x)
;  (concatenate 'string #\  (write-to-string x)))
;use char-code instead? Read on that

(defun to-hex (number)
  (cond ((= number 0) "0")
	((= number 1) "1")
	((= number 2) "2")
	((= number 3) "3")
	((= number 4) "4")
	((= number 5) "5")
	((= number 6) "6")
	((= number 7) "7")
	((= number 8) "8")
	((= number 9) "9")
	((= number 10) "A")
	((= number 11) "B")
	((= number 12) "C")
	((= number 13) "D")
	((= number 14) "E")
	((= number 15) "F")
	(t number)))

(defun create-uuid (&optional (return-symbol?))
  "Creates a 128-bits universally unique identifier"
  (flet ((make-hex-string (length)
           (let ((result nil))
             (dotimes (i length)
               (setf result (concatenate 'string result (to-hex (random 16)))))
             (return-from make-hex-string result))))
    (let ((result
            (concatenate 'string
                         (make-hex-string 8)
                         "-"
                         (make-hex-string 4)
                         "-4"
                         (make-hex-string 3)
                         "-"
                         (elt (assoc (random 4)
                                     '((0 "8")
                                       (1 "9")
                                       (2 "A")
                                       (3 "B"))) 1)
                         (make-hex-string 3)
                         "-"
                         (make-hex-string 12))))
      (if return-symbol?
          (read-from-string result)
          result))    
    
    ))

(defun lerp (value-0 value-1 percentage)
  "Linear interpolation from value-0 to value-1, based on percentage"
  (+ value-0 (* percentage (- value-1 value-0))))

(defun lerp-number-list (list-0 list-1 percentage)
  "Linearly interpolates between two lists of numbers, based on percentage"
  (mapcar (lambda (x y) (lerp x y percentage)) list-0 list-1))

(defun change-range (input initial-range-start initial-range-end final-range-start final-range-end)
  "Changes the range of the input based on its initial range and its final range"
  (+ final-range-start
     (* (- input initial-range-start)
	(/ (- final-range-end final-range-start)
	   (- initial-range-end initial-range-start)))))

(defun random-between (range-start range-end)
  "Returns a random value between range-start and range-end"
  (change-range (random 1.0) 0 1 range-start range-end))

(defun memoize (fun)
  "Returns a memoized version of fun, in which inputs have their outputs cached for faster retrival of already-computed results"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fun args)))))))

(defun sort-copy (sequence predicate)
  "Sort a sequence based on predicate, the original sequence stays untouched"
  (sort (copy-tree sequence) predicate))

(defun enlist (item)
  "Transforms item into a list if it isn't one already"
  (if (listp item)
      item
      (list item)))

(defun elt-bound (sequence index)
  "Returns nil if elt is out-of-bounds"
  (cond ((> 0 index) nil)
	((< (length sequence) index) nil)
	(t (elt sequence index))))

(defun elt-within (sequence index)
  "Returns the item at the given index, giving the last item in the list if the index is greater than the sequence's size, and the first item if the index is less than 0."
  (elt sequence (bound-number index 0 (1- (length sequence)))))

(defun elt-random (sequence)
  "Selects a random element of sequence"
  (when sequence
    (elt-bound sequence (random (length sequence)))))

(defun elt-random-n (sequence n)
  "Returns a list of size n (up to the length of the given sequence), where each element is a random element from the given sequence."
  (subseq (shuffle sequence) 0 (bound-number n 0 (length sequence))))

(defun elt-random-upto (sequence upto-n)
  "Selects a random element of a sequence, up to the given upto-n boundary."
  (elt sequence (min (random upto-n) (random (length sequence)))))

(defun but-last (sequence)
  "Returns sequence without its last element"
  (when (= 0 (length sequence))
    (return-from but-last (list nil)))
  (subseq sequence 0 (1- (length sequence))))

(defun list-to-string (lst)
  (format nil "~{~A~}" lst))

(defun string-but-last (string)
  "Returns the given string, excluding its last letter"
  (list-to-string (but-last (string-to-list string))))

(defun index-fun (function sequence)
  "Reduces the sequence using the function and gives the index of the result in the sequence"
  (position (reduce function sequence) sequence))

(defun max-index (sequence)
  "Finds the index of the element with the highest value"
  (index-fun #'max sequence))

(defun min-index (sequence)
  "Finds the index of the element with the lowest value"
  (index-fun #'min sequence))

(defun between (number min-bound max-bound)
  "Returns true when number is between min-bound and max-bound, inclusively"
  (when (numberp number)
    (and (>= number min-bound) (<= number max-bound))))

(defun wrap (number min-bound max-bound)
  "Roll-over a number going beyond min-bound and max-bound"
  (if (between number min-bound max-bound)
      number
      (+ (mod number max-bound) min-bound)))

(defun bound-number (number min-bound max-bound)
  "Return number if between min-bound and max-bound, else return max-bound if greater, else min-bound if lesser."
  (cond ((> number max-bound) max-bound)
        ((< number min-bound) min-bound)
        (t number)))

(defun get-moore-neighbors (x y)
  "Returns the coordinates of the moore neighbors (The 8 squares around a square) of a given coordinate)"
  (list
   (list (- x 1) (- y 1)) (list (- x 1) y) (list (- x 1) (+ y 1))
   (list x (- y 1)) (list x (+ y 1))
   (list (+ x 1) (- y 1)) (list (+ x 1) y) (list (+ x 1) (+ y 1))))

(defun hsv-to-rgb (input)
  "Takes a list of 3 values, corresponding respectively to the Hue, saturation and value of a colour, and transforms it into Red, Green and Blue values. The hue must be between 0 and 360, the saturation and value must be between 0.0 and 1.0"
  (let* (
	 (c (* (elt input 2) (elt input 1)))
	 (x (* c (- 1 (abs (- (mod (/ (elt input 0) 60) 2) 1)))))
	 (m (- (elt input 2) c)))
    (let ((temp
	    (cond ((and (>= (elt input 0) 0) (< (elt input 0) 60)) (list c x 0))
		  ((and (>= (elt input 0) 60) (< (elt input 0) 120)) (list x c 0))
		  ((and (>= (elt input 0) 120) (< (elt input 0) 180)) (list 0 c x))
		  ((and (>= (elt input 0) 180) (< (elt input 0) 240)) (list 0 x c))
		  ((and (>= (elt input 0) 240) (< (elt input 0) 300)) (list x 0 c))
		  ((and (>= (elt input 0) 300) (< (elt input 0) 360)) (list c 0 x)))))
      (list (* (+ (elt temp 0) m) 255)
	    (* (+ (elt temp 1) m) 255)
	    (* (+ (elt temp 2) m) 255)))))

;(defun rgb-to-hsv (input)

(defun elt-change (sequence index value)
  "Non-destructively changes an element in a sequence"
  (let ((temp (copy-tree sequence)))
    (append (tack (subseq temp 0 index) value) (subseq temp (+ 1 index)))))

(defun elt-insert (sequence index value)
  "Non-destructively inserts an element in a sequence"
  (let ((temp (copy-tree sequence)) (true-index index))
    (when (< true-index 0)
      (setf true-index 0))
    (when (> true-index (length sequence))
      (setf true-index (length sequence)))	     
    (append (tack (subseq temp 0 true-index) value) (subseq temp true-index))))

(defun elt-remove (sequence index)
  "Non-destructively removes an element in a sequence"
  (let ((temp (copy-tree sequence)))
    (append (subseq temp 0 index) (subseq temp (+ 1 index)))))

(defun repeat (f input times)
  "Repeats the application of the function f times on the input"
  (if (zerop times)
      input
      (repeat f (funcall f input) (- times 1))))

(defun keep-based-on (sequence1 sequence2)
  (map 'list (lambda (a b) (unless (null b) a)) sequence1 sequence2))

(defun keep-if-index (predicate sequence)
  (remove-if #'null (keep-based-on sequence (mapcar predicate (range (length sequence))))))

(defun mapcar-plist (function list)
  "Applies a function over a plist, skipping over the keywords"
  (mapcar (lambda (i) (if (keywordp i) (identity i) (funcall function i))) list))

(defun number-length (number)
  "Number of digits in the supplied number"
  (ceiling (log number 10)))

;defun reverse of explose

(defun explode (number)
  "Creates a list of the digits in a number, little-endian"
  (defun explode-helper (number index)
    (truncate (/ (mod number (expt 10 (+ index 1))) (expt 10 index))))
  (mapcar (lambda (i) (explode-helper number i)) (range (number-length number))))

(defun concatenate-numbers (a b)
  (+ (* a (expt 10 (number-length b))) b))

(defun de-explode (list)
  "Create a number from a little-endian list of numbers"
  (reduce (lambda (a b) (concatenate-numbers b a)) list))

(defun binary-list (number &optional acc)
  "Give the bits of a number in big-endian"
  (cond ((zerop number) (or acc (list 0)))
	((plusp number) (binary-list (ash number -1) (cons (logand 1 number) acc)))
	(t (error "~S: non-negative argument required, got ~s" 'binary-list number))))

(defun left-pad-t (sequence times &optional (padding 0))
  "Pads the sequence on the left times number of times"
  (let ((result (copy-seq sequence)))
    (dotimes (i times)
      (setf result (elt-insert result 0 padding)))
    result))

(defun left-pad-l (sequence desired-length &optional (padding 0))
  "Pads the sequence on the left until its length is equal to desired-length"
  (left-pad-t sequence (max 0 (- desired-length (length sequence))) padding))

(defun left-pad (sequence &key desired-length times (padding 0))
  "Pads the sequence on the left, either with the desired length, or the number of times to pad, not both"
  (if (or (and desired-length times) (and (not desired-length) (not times)))
      (error "Can't set both a :desired-length and :times in left pad")
      (if times
	  (left-pad-t sequence times padding)
	  (left-pad-l sequence desired-length padding))))

(defun right-pad-t (sequence times &optional (padding 0))
  "Pads the sequence on the right times number of times"
  (let ((result (copy-seq sequence)))
    (dotimes (i times)
      (ntack result padding))
    result))

(defun right-pad-l (sequence desired-length &optional (padding 0))
  "Pads the sequence on the right until its length is equal to desired-length"
  (right-pad-t sequence (max 0 (- desired-length (length sequence))) padding))

(defun right-pad (sequence &key desired-length times (padding 0))
  "Pads the sequence on the right, either with the desired length, or the number of times to pad, not both"
  (if (or (and desired-length times) (and (not desired-length) (not times)))
      (error "Can't set both a :desired-length and :times in right pad")
      (if times
	  (right-pad-t sequence times padding)
	  (right-pad-l sequence desired-length padding))))

(defun binary-list-to-decimal (list)
  "Transforms a list of binary numbers into its decimal representation"
  (reduce (lambda (x y) (+ (* 2 x) y)) list))



(defun copy-file-to (from-file to-file)
  "Copies the from-file and writes its contents into the to-file, creating if necessary."
  (with-open-file (input-stream from-file
                                :direction :input
                                :element-type '(unsigned-byte 8))
    (with-open-file (output-stream to-file
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
      (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
        (loop for pos = (read-sequence buf input-stream)
              while (plusp pos)
              do (write-sequence buf output-stream :end pos)))))
  )

(defun save-file (data filename)
  "Saves the Lisp expression 'data' to the path 'filename'"
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print data out))))

(defun load-file (filename)
  "Loads the lisp expression from the file at 'filename'"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(defun files-in-directory (&optional (directory "./"))
  "Returns a list of the files in directory"
  (directory (concatenate 'string directory
			  (unless (eq #\/ (elt directory (1- (length directory))))
			    "/")
			  "*.*")))

(defun is-file? (pathname)
  "Returns t if given pathname points to a file, else nil."
  (when (pathname-name pathname) t))

(defun is-file-in-folder? (pathname folder-name)
  "Returns t if given pathname points to a file that's in the given folder-name."
  (when (is-file? pathname)
    (equal folder-name (last1 (pathname-directory pathname)))))

(defun append-to-file (data filename)
  (with-open-file (out filename :direction :output
				:if-exists :append
				:if-does-not-exist :create)
    "Opens a file to the path 'filaname' and append something to it"
    (format out "~a~%" data)))

(defun sort-with-predicate (sequence predicate)
  "Non-destructively sorts a list via the provided predicate, returns a list.
If a nested list is provided, sorts based on the first entry of each list.
eg: (sort-with-predicate #'> (list 3 1 (list 2 4 9 5))) => (3 (2 4 9 5) 1)"
  (sort-copy sequence (lambda (i j) (funcall predicate (first (enlist i)) (first (enlist j))))))

(defun sort-descend (sequence)
  "Non-destructively sorts a list in descending order, returns a list."
  (sort-with-predicate sequence #'>))

(defun sort-ascend (sequence)
  "Non-destructively sorts a list in ascending order, returns a list."
  (sort-with-predicate sequence #'<))

(defun select-larger-list (sequence1 sequence2)
  "Returns the larger of two lists"
  (if (> (length sequence1) (length sequence2))
      sequence1
      sequence2))

(defun select-smaller-list (sequence1 sequence2)
  "Returns the smaller of two lists."
  (if (< (length sequence1) (length sequence2))
      sequence1
      sequence2))

(defun longest-length (sequence1 sequence2)
  "Returns the length of the largest of the two lists."
  (length (select-larger-list sequence1 sequence2)))

(defun shortest-length (sequence1 sequence2)
  "Returns the length of the smallest of two lists."
  (length (select-smaller-list sequence1 sequence2)))

(defun values-from-index (sequence start-index)
  (let ((values ()))
    (loop for index from start-index to (1- (length sequence)) do
          (ntack values (elt sequence index)))
    values))

(defun smaller-than-index (sequence value &optional (comparaison-function #'<))
  "Finds the index of the first element for which value is smaller than it"
  (let ((length (length sequence)))
    (labels ((smaller-helper (sequence value index)
	       (if (null sequence)
		   length
		   (if (funcall comparaison-function (car sequence) value)
		       (smaller-helper (cdr sequence) value (1+ index))
		       index))))
      (smaller-helper sequence value 0))))

(defun insertion-sort (sequence value &optional (comparaison-function #'<))
  "Inserts an element in a list according to the comparaison-function (Defaults to <)"
  (elt-insert sequence (smaller-than-index sequence value comparaison-function) value))

(defun t-or-nil (value)
  "Returns t if value is not nil, else nil."
  (when value t))

(defun unnest (value)
  "If the given value is a list, and the only element of that list is another list, then it returns the inner list, else returns given value."
  (if (listp value)
      (if (and (listp (first value)) (= (length value) 1))
          (first value)
          value)
      value))

(defun funcall-if (predicate func &rest args)
  "Applies func to args if predicate resolves to true, else returns the args unchanged."
  (if (typecase predicate
        (boolean predicate)
        (t (funcall predicate)))
      (funcall func (unnest args))
      (unnest args)))

(defun funcall-unless (predicate func &rest args)
  "Applies func to args unless predicate resolves to true, else returns the args unchanged."
  (funcall-if (not predicate) func (unnest args)))

(defun ramp-segmentation (sequence)
  "Segments a sequence into equally spaced terms"
  (let ((length (length sequence)))
    (mapcar (lambda (i) (list (+ (/ length) (/ i length)) (elt sequence i))) (range length))))

(defun ramp (sequence value)
  "Associate a value between 0.0 and 1.0 with an element in the sequence based on their order"
  (cond ((< value 0) (car sequence))
        ((>= value 1) (last1 sequence))
        (t (cadr (assoc value (ramp-segmentation sequence) :test #'<)))))

(defun contains? (object sequence &key (test #'eq))
  "Returns t if sequence contains object, else nil."
  (t-or-nil (find object sequence :test test)))

(defun string-contains? (string substring)
  "Returns t if given string contains given substring, else nil."
  (t-or-nil (search substring string)))

(defun string-upcase-if (string predicate)
  "Returns the upcased version of the given string if the given predicate returns T, else returns the original string."
  (if predicate (string-upcase string) string))

(defun string-upcase-unless (string predicate)
  "Returns the upcased version of the given string unless the given predicate returns T, else returns the original string."
  (string-upcase-if string (not predicate)))

(defun string-starts-with? (string substring &optional (case-sensitive? nil))
  "Returns T if the given string begins with the given substring."
  (when (and
         (> (length substring) 0)
         (> (length string) (length substring)))
    (string= (string-upcase-unless string case-sensitive?)
             (string-upcase-unless substring case-sensitive?)
             :end1 (length substring)))
  )

(defun boolp (value)
  (or (equal (type-of value) 'boolean)
      (null value)))

(defun to-string (x)
  (format nil "~a" x))

(defun set! (object value)
  "Use this when setf isn't working inside of a func.
It works because Common Lisp passes everything by value, not by reference, except for car, cdr and some other things which pass places. Rplaca and Rplacd work on places, letting us hack a way to pretend we passed something by reference."
  (rplaca (enlist object) (car (enlist value)))
  (rplacd (enlist object) (cdr (enlist value))))

(defun length= (sequence length)
  "Returns true if the given sequence is of the given length"
  (length-test sequence length #'=))

(defun length<= (sequence length)
  "Returns true if the given sequence is less than or equal to the given length"
  (length-test sequence length #'<=))

(defun length>= (sequence length)
  "Returns true if the given sequence is greater than or equal to the given length"
  (length-test sequence length #'>=))

(defun length< (sequence length)
  "Returns true if the given sequence is less than the given length"
  (length-test sequence length #'<))

(defun length> (sequence length)
  "Returns true if the given sequence is greater than the given length"
  (length-test sequence length #'>))

(defun length-test (sequence length test)
  "Returns true if the given sequence's length passes the given test"
  (when (listp sequence)
    (funcall test (length sequence) length)))

(defun abs- (value1 value2)
  "Returns the absolute difference of the two given values"
  (abs (- value1 value2)))

(defun interleave (a b)
  "Returns an interleaved list, eg: (list a b c) (list 1 2 3) => (a 1 b 2 c 3)"
  (flet ((nil-pad (list on-list)
           (append list (make-list (max 0 (- (length on-list) (length list)))))))
    (loop for x in (nil-pad a b)
          for y in (nil-pad b a)
          append (list x y))))

(defun pair-up (seq1 seq2)
  "Returns a list of pairs, where each item in seq1 is paired to each item in seq2."
  (let ((pairs))
    (dolist (outer seq1)
      (dolist (inner seq2)
        (ntack pairs (list outer inner))))
    pairs))

;; (defun string-to-list (my-cool-string)
;;   "Returns a list of chars from the given string"
;;   (let ((char-list))
;;     (loop for index from 0 to (1- (length my-cool-string)) do
;;           (ntack char-list (char my-cool-string index)))
;;     char-list))

;; (defun intersperse-list (sequence object)
;;   "Places the given object between each item in the given sequence, eg: (1 2 3) / => (1 / 2 / 3 /)"
;;   (let ((object-sequence (make-list (length sequence) :initial-element object)))
;;     (interleave sequence object-sequence)))

(defun intersperse-list (sequence object)
  "Places the given object between each item in the given sequence, eg: (1 2 3) / => (1 / 2 / 3 /)"
  (loop for item in sequence
        nconcing (list item object)))

(defun print-table (data width)
  (format t "~{|~{ ~{~Vd~}~}|~%~}"
          (mapcar #'(lambda (r) (mapcar #'(lambda (v) (list width v)) r)) data) ))

(defun x-if-nil (value x)
  "Returns x when the given value is nil, else returns the given value."
  (when value
    (return-from x-if-nil value))
  x)

(defun zero-if-nil (value)
  "Returns 0 if the given value is nil, else returns given value."
  (x-if-nil value 0))

(defun x-if-y (value x y &key (test #'=))
  "Returns the x if value = y."
  (if (funcall test value y)
      x
      value))

(defun binary-search-list (value sequence &optional (accessor nil) )
  "Returns the index of the given value from the given sequence, comparing the value to items in the list with the given accessor function, otherwise just comparing the items of the list directly to the given value. If not found, returns nil."
  (let ((low 0)
        (high (1- (length sequence)))
        (descending? (> (if accessor
                            (funcall accessor (first sequence))
                            (first sequence))
                        (if accessor
                            (funcall accessor (last1 sequence))
                            (last1 sequence)))))

    ;;reverse sequence if its in descending order
    (when descending?
      (setf sequence (reverse sequence)))

    ;; actual binary search
    (do () ((< high low) nil)
      (let ((middle (floor (+ low high) 2)))
        
        (cond ((> (if accessor
                      (funcall accessor (elt sequence middle))
                      (elt sequence middle))
                  value)
               (setf high (1- middle)))
              
              ((< (if accessor
                      (funcall accessor (elt sequence middle))
                      (elt sequence middle))
                  value)
               (setf low (1+ middle)))
              
              (t (return (if descending?
                             (- (length sequence) middle 1)
                             middle))))))))

(defun to-property (value)
  "Returns the given value as a symbol prefixed with :, eg: 5 => :5, 'cat => :cat,! \"dog\" => :dog"
  (read-from-string (format nil ":~a" value)))

(defun string-to-property (string)
  "Returns a symbol prefixed with :, such that string 'dog' => :dog. Useful for procedurally accessing/setting property lists."
  (to-property string))

(defun get-property (sequence indicator)
  "Returns the property from the given property-list sequence, as indicated by the indicator. (get-property (list :x 10 :y 20) :y) => 20."
  (getf sequence (to-property indicator)))

(defun subseq-after (sequence delimiter &key (test))
  "Returns the given sequence or string, from after the given delimiter such that (subseq-after 'doggy' 'd') => 'oggy'"
  (unless test
    (typecase sequence
      (string (setf test #'string=))
      (list (setf test #'equal))))
  
  (let ((found-pos (position delimiter sequence :test test)))
    (subseq sequence (if found-pos
                         (1+ found-pos)
                         0))))

(defun subseq-before (sequence delimiter &key (test))
  "Returns the given sequence upto the given delimiter, such that (subseq-before (list 1 2 3 4 5) 3) => (1 2)"
  (reverse (subseq-after (reverse sequence) delimiter :test test)))

;; (defun concat-symbols (symbol-list)
;;   "Returns a symbol that is the concatenation of the given symbols, eg (concat-symbols (list 'cat 'dog)) => CATDOG"
;;   (read-from-string (format nil "~{~a~}" symbol-list)))

(defun concat-symbols (symbol-list)
  "Returns a symbol that is the concatenation of the given symbols, eg (concat-symbols (list 'cat 'dog)) => CATDOG"
  (apply #'symb symbol-list))

(defun property-key? (symbol)
  "Returns T if the given symbol is prefixed with :"
  (keywordp symbol)
  ;; (when (symbolp symbol)
  ;;   (eq symbol
  ;;       (read-from-string (format nil ":~a" (symbol-name symbol)))))
  )

(defun extract-property-keys (sequence)
  "Returns a list of symbols that are the keys for the given property list."
  (loop for item in sequence
        when (keywordp item)
        collect item))

;; (defun extract-property-list (sequence)
;;   "Extracts the property-key / value pairs from the given list, returning a property list from a malformed one. Unless you give a list with no property-key/value pairs, in which case it returns nil."
;;   (let ((length (1- (length sequence))))
;;     (loop for item in sequence
;;           for index from 0
;;           when (and
;;                 (keywordp item)
;;                 (< index length))
;;           nconcing (list (elt sequence index)
;;                          (elt sequence (1+ index))))))

(defun extract-property-list (sequence)
  "Extracts the property-key / value pairs from the given list, returning a property list from a malformed one. Unless you give a list with no property-key/value pairs, in which case it returns nil."
  (let ((length (1- (length sequence))))
    (loop for item in sequence
          for index from 0
          when (and
                (keywordp item)
                (< index length))
          collect (elt sequence index)
          and
          collect (1+ index))))

;; (defun extract-property-list (sequence)
;;   "Extracts the property-key / value pairs from the given list, returning a property list from a malformed one. Unless you give a list with no property-key/value pairs, in which case it returns nil."
;;   (when (functionp sequence)
;;     (setf sequence (funcall sequence)))
;;   (let ((key-indices))
;;     (dotimes (index (length sequence))
;;       (when (property-key? (elt sequence index))
;;         (ntack key-indices (elt sequence index))
;;         (ntack key-indices (elt sequence (1+ index)))))
;;     key-indices))

(defmacro toggle! (value)
  "Destructively sets the given boolean value to (not value)"
  `(setf ,value (not ,value)))

;; (defun extract-property-keys (sequence)
;;   "Returns a list of symbols that are the keys for the given property list."
;;   (let ((result)
;;         (flip-flop))
;;     (dolist (item (extract-property-list sequence))
;;       (when (toggle! flip-flop)
;;         (ntack result item)))
;;     result))

;; (defun new-extract-pkeys (sequence)
;;   (loop for item in (extract-property-list sequence)
;;         for counter from 1
;;         for 
;;         when (oddp counter)
;;         collect item))

;; (defun new-extract (sequence)
;;   (loop for item in sequence
;;         when (keywordp item)
;;         collect item))

(defun decimate (num decimal-place)
  "Returns the given number decimated to the nth decimal place, eg (decimate 0.123456789 5) => 0.12345"
  (float (* (truncate num (* 1.0 (/ 1 (expt 10 decimal-place)))) (/ 1 (expt 10 decimal-place)))))

(defun max-list (sequence)
  "Returns the biggest number in the given sequence."
  (reduce #'max sequence))

(defun min-list (sequence)
  "Returns the smallest number in the given sequence."
  (reduce #'min sequence))

(defun elt-n (sequence index &rest other-indices)
  "Returns the elt of the elt of the elt of etc, of the given sequence. Eg: (elt-n my-cool-list 0 1 2) == (third (second (first my-cool-list))) == myCoolList[0][1][2]"
  (let ((indices (append (enlist index) other-indices))
        (current-list (copy-tree sequence)))
    (dolist (current-index indices)
      (setq current-list (elt current-list current-index)))
    current-list))

(defun call-if-func (value)
  "Calls the given value if its a function, else returns nil."
  (when (functionp value)
    (funcall value)))

(defmacro setf-if-nil (place value)
  "Setf's the given place to the given value UNLESS it already has a value. value only evaluates if place is nil."
  `(if ,place
       ,place
       (setf ,place ,value)))

(defmacro setf-unless-nil (place value)
  "Assigns the given value to the given place, unless the value is nil. Returns the value of place."
  `(if ,value
       (setf ,place ,value)
       ,place))

(defmacro mac1 (expr)
  "Pretty prints the macroexpansion-1 of the given expression."
  `(pprint (macroexpand-1 ',expr)))

;; (defun property-to-symbol (property)
;;   (read-from-string (format nil "~a" property)))

(defun property-to-symbol (property)
  (symb property))

(defun symbol-to-string-preserve-quotes (symbol)
  "Returns the given symbol as a string, where double-quotes are turned into \"'s"
  (with-output-to-string (s)
    (print symbol s)))

(defmacro bracketise-properties-as-vars (&body property-list)
  "Returns a list of symbols where each symbol is '(property-key-without-colon value)"
  `(let ((props (extract-property-list ',@property-list))
         (result nil))
     (dolist (item (extract-property-keys props))
       (ntack result `(,(property-to-symbol item) ,(getf props item))))
     result))

(defmacro new-bracketise (&body property-list)
 `(let* ((,(property-to-symbol (elt property-list 0)) ,(elt property-list 1)) (,(property-to-symbol (elt property-list 2)) ,(elt property-list 3)))))

(defmacro property-pair-to-var (&body property-pair)
  "(:x 10) => (x 10)"
  `(,(property-to-symbol (first property-pair)) ,(second property-pair)))


(defmacro new-bracketise (&body property-list)
  (make-list (length property-list) :initial-element (())))

;; (defmacro test-brackets (&rest body)
;;   `(,(first body) ,(second body)))

;; (defmacro make-lots-of-brackets (&rest pairs)
;;   `((let ((result))
;;       (dotimes (pair (length ,pairs))
;;         (ntack result (test-brackets pair)))
;;       result)))

(defmacro extract-plist-macro (&body body)
  "Extracts the property-key / value pairs from the given list, returning a property list from a malformed one. Unless you give a list with no property-key/value pairs, in which case it returns nil."
  `(let ((key-indices))
     (dotimes (index (length (list ,@body)))
       (when (property-key? (elt (list ,@body) index))
         (ntack key-indices (elt (list ,@body) index))
         (ntack key-indices (elt (list ,@body) (1+ index)))))
     key-indices))

(defmacro extract-plist-keys-macro (&body body)
  "Macro that returns the keys and only the keys in the given property list, :prefixes included"
  `(let ((result)
         (flip-flop))
     (dolist (item (extract-plist-macro ,@body))
       (when (toggle! flip-flop)
         (ntack result item)))
     result))

;; (defmacro make-progv (&body property-list)
;;   (let* ((keys `(extract-plist-keys-macro ,@property-list)))
;;     `(progv
;;          (mapcar #'property-to-symbol (extract-plist-keys-macro ,@property-list))
;;          (mapcar (lambda (x) (getf (extract-plist-macro ,@property-list) x)) ,keys)
;;        (list :x x :y y))))

(defmacro bracketise-property-list-string (&body property-list)
  "Returns a string where the property list has been exploded into bracketed paris, (bracketise-property-list-string (:x 10 :y 20) => \"((:x 10) (:y 20))\"" 
  `(symbol-to-string-preserve-quotes (bracketise-properties-as-vars ,@property-list)))

;; (defmacro self-reference-plist (&body property-list)
;;   "Takes a property list as input, and returns a property list where self-references in the property list have been resolved. Eg: (self-reference-plist (list :x 10 :y (* x 2))) => (:x 10 :y 20)"
;;   `(read-from-string (format nil "(let* ~a (list ~a))"
;;                              (bracketise-property-list-string ,@property-list)
;;                              (let ((result ""))
;;                                (dolist (item (extract-property-keys ',@property-list))
;;                                  (setf result (concatenate 'string result (format nil ":~a ~a " (to-property item) (property-to-symbol item)))))
;;                                result))))

;; (defmacro plist* (&body plist)
;;   "Creates and returns a property list where self-references have been resolved. Please remember to list referenced keys before the keys that reference them, eg: (plist* :x 10 :y (* x 2)) = CORRECT, (plist* :y (* x 2) :x 10) = INCORRECT. An example of correct usage: (plist* :x 10 :y (* x 2)) => (:X 10 :Y 20)"
;;   `(self-reference-plist (list ,@plist)))


(defmacro new-plist* (plist)
  "Returns a plist whose arguments have been evaluated sequentially, such that latter items may defined in terms of items listed before itself."
  ;; remove first item in plist if it's not a property
  ;; (unless (eq (first plist) (read-from-string (format nil "~a" (first plist))))
  ;;   (setf plist (subseq plist 1)))
  (setf plist (extract-property-list plist))

  (let
      ;; (:x 10 :y 20 :z (* x y)) => ((x 10) (y 20) (z (* x y)))
      ((quoted-plist (flet ((gather-keys ()
                              (let ((result))
                                (dotimes (index (length plist))
                                  (when (evenp index)
                                    (ntack result `(,(read-from-string (format nil "~a" (elt plist index))) ,(elt plist (1+ index))))))
                                result)))
                       (gather-keys)))

       ;;(:x 10 :y 20 :z (* x y)) => (list :x x :y y :z z)
       (key-pairs (flet ((gather-key-pairs ()
                           (let ((result (list 'list)))
                             (dotimes (index (length plist))
                               (if (evenp index)
                                   (ntack result (elt plist index))
                                   (ntack result (read-from-string (format nil "~a" (elt plist (1- index)))))))
                             result)))
                    (gather-key-pairs))))
    `(let* ,quoted-plist ,key-pairs)))


;; (defmacro lexical-scope-from-plist* (plist &body body)
;;   "Wraps the given body in a lexical scope created from converting the given property list into a sequential property list and defining its properties as bound variables for the body to reference."
;;   (setf plist (extract-property-list plist))
;;   (let
;;       ;; (:x 10 :y 20 :z (* x y)) => ((x 10) (y 20) (z (* x y)))
;;       ((quoted-plist (flet ((gather-keys ()
;;                               (let ((result))
;;                                 (dotimes (index (length plist))
;;                                   (when (evenp index)
;;                                     (ntack result `(,(read-from-string (format nil "~a" (elt plist index))) ,(elt plist (1+ index))))))
;;                                 result)))
;;                        (gather-keys))))
;;     `(let* ,quoted-plist (list ,@body))))

(defmacro lexical-scope-from-new-plist* (plist &body body)
  "Takes a regular property list and a body as input. Creates a sequential lexical scope from the given plist to allow for self reference, then places the body inside that scope so it may refer to the potentially self referential plist's values as if they were bound variables.x"
  ;; this macro is just a manual combination of lexical-scope-from-plist* and new-plist*, because i couldn't for the life of me figure out how to call a macro from within a macro, when it was the lexical binding of a let*.
  ;;;;; setup self referential plist*
  (setf plist (extract-property-list plist))
  (let
      ;; (:x 10 :y 20 :z (* x y)) => ((x 10) (y 20) (z (* x y)))
      ((quoted-plist (flet ((gather-keys ()
                              (let ((result))
                                (dotimes (index (length plist))
                                  (when (evenp index)
                                    (ntack result `(,(read-from-string (format nil "~a" (elt plist index))) ,(elt plist (1+ index))))))
                                result)))
                       (gather-keys)))

       ;;(:x 10 :y 20 :z (* x y)) => (list :x x :y y :z z)
       (key-pairs (flet ((gather-key-pairs ()
                           (let ((result (list 'list)))
                             (dotimes (index (length plist))
                               (if (evenp index)
                                   (ntack result (elt plist index))
                                   (ntack result (read-from-string (format nil "~a" (elt plist (1- index)))))))
                             result)))
                    (gather-key-pairs))))
    `(let* ,quoted-plist ,key-pairs)
    
    ;;;;; use the created self-referential plist* to now create a let* lexical binding
    (let
        ;; (:x 10 :y 20 :z (* x y)) => ((x 10) (y 20) (z (* x y)))
        ((quoted-plist (flet ((gather-keys ()
                                (let ((result))
                                  (dotimes (index (length plist))
                                    (when (evenp index)
                                      (ntack result `(,(read-from-string (format nil "~a" (elt plist index))) ,(elt plist (1+ index))))))
                                  result)))
                         (gather-keys))))
      `(let* ,quoted-plist (list ,@body)))))

(defun call-if-func-recursively (value)
  "Recursively checks if the given value is a function, calling it if so, until the value returned from funcalling is not a function. Returns that value."
  (let ((result (if (functionp value)
                    (funcall value)
                    value)))
    (if (functionp result)
              (call-if-func-recursively result)
              result)))

(defun cifr (value)
  "An abbreviated signature of call-if-func-recursively. Recursively calls the result of the given value if its a function, until a non-function is reached, returns that non-function value."
  (call-if-func-recursively value))

(defun divide-list (sequence width)
  "Returns a list of lists, that have been created by dividing the given sequence into subsequences of max length width."
  (unless (< 0 width)
    (setf width 1))
  (if (> (length sequence) width)
      (append (list (subseq sequence 0 width)) (divide-list (subseq sequence width) width))
      (list sequence)))

(defun nth-char (string n)
  "Returns the nth char in the given string."
  (elt (string-to-list string) n))

(defun first-char (string)
  "Returns the first char of the given string."
  (nth-char string 0))

(defun last-char (string)
  "Returns the last char in the given string."
  (last1 (string-to-list string)))

(defun last-index? (sequence index)
  "Returns t if the given index is equal to the length of the given sequence -1."
  (= index (1- (length sequence))))

(defun trim-left-whitespace (text)
  "Returns the given string with all whitespace prefixing it removed."
  (trim-whitespace text t))

(defun trim-right-whitespace (text)
  "Returns the given string with all whitespace after it removed."
  (trim-whitespace text))

(defun trim-whitespace (text &optional (left? nil))
  "Removes trailing/following whitespace from a string, specify left? as t for left-space, nil for right-space."
  (funcall (if left?
               #'string-left-trim
               #'string-right-trim)
           `(#\Space #\Newline #\Backspace #\Tab 
                     #\Linefeed #\Page #\Return #\Rubout)
           text))

(defun whitespace? (string)
  "Returns t when the given string consists of only whitespace."
  (when (and
         (< 0 (length (string string)))
         (= 0 (length (trim-whitespace (string string)))))
    t))

(defun first-char-whitespace? (string)
  "Returns t when the first char of the given string is whitespace."
  (whitespace? (first-char string)))

(defun last-char-whitespace? (string)
  "Returns t when the last char of the given string is whitespace."
  (whitespace? (last-char string)))

(defun suffix-string (string suffix)
  "Returns the addition of the given suffix to the given string."
  (format nil "~a~a" string suffix))

(defun suffix-string-if (string suffix predicate)
  "Returns the given string with the given suffix appended to it, if the predicate resolves to T, else returns the original string."
  (if (call-if-func-recursively predicate)
      (suffix-string string suffix)
      string))

(defun suffix-string-unless (string suffix predicate)
  "Returns the given string with the given suffix appended to it, unless the predicate resolves to T, else returns the original string."
  (suffix-string-if string suffix (not predicate)))

(defun next-item (sequence current-index)
  "Returns the next item in the given sequence, from the given current-index, bounded so if current-index is greater than or equal to the length of the sequence, it'll return the last item in the sequence instead of error-ing out."
  (elt sequence (bound-number current-index 0 (1- (length sequence)))))

(defun bound-func-result (func min &optional (max nil))
  "Returns a function which returns a bounded result of the given function. So, if you have a func that might return a value outside a bound you want it to, then use this function to essentially cap its results."
  (lambda () (let ((result (call-if-func-recursively func)))
               (cond 
                 ((> min result) min)
                 (max (if (< max result) max result))
                 (t result)))))

;; (defmacro charstring-to-char (charstring)
;;   "Returns the given string of a char converted to the actual char it's describing. eg '#\a' -> #\a"
;;   `(list '# '\ charstring))

(defun remove-property-and-value (sequence property-key)
  "Removes the property and the value of it from a given property list."
  (remove (to-property property-key) (remove (getf sequence (to-property property-key)) sequence)))

(defun remove-properties-and-values (sequence property-keys)
  "Returns the given sequence after removing all properties and their values, as passed in property-keys, from the given sequence."
  (let ((result sequence))
    (dolist (key property-keys)
      (setf result (remove-property-and-value result key)))
    result))

(defun string-insert (destination-string string-to-insert insertion-index)
  "Inserts the given string-to-insert into the destination-string at the given insertion-index."
  (format nil "~a~a~a"
          (subseq destination-string 0 insertion-index)
          string-to-insert
          (subseq destination-string insertion-index)))

(defun string-remove-at-index (string index n-of-chars-to-remove)
  "Removes n-of-chars at the given index in the given string."
  (format nil "~a~a"
          (subseq string 0 index)
          (subseq string (+ index n-of-chars-to-remove))))

(defun tokenise (string delimiter)
  "Splits the string by the given delimiter. its very important the delimiter is a char and not a string ok"
  (loop for start = 0 then (1+ finish)
        for finish = (position delimiter string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defun stepify (value step)
  "Returns the given value rounded down to the value divided by the step. eg: (stepify 65.1235 50) -> 50. (stepify 65.1235 12) -> 60."
  (* (truncate (/ value step)) step))

(defun float-to-percentage (value &key (decimal-places 2) (min-step))
  "Returns the given float (0..1) converted to percentage, with the given number of decimal places and with a minimum step of the given min-step."
  (let ((result (* 100 value)))
    (if (= 0 decimal-places)
        (setf result (truncate result))
        (setf result (decimate result decimal-places)))

    (if min-step
        (stepify result min-step)
        result)))

(defun midpoint (x1 y1 x2 y2)
  "Returns the midpoint of the two points (x1 y1) (x2 y2), as (x-midpoint y-midpoint)"
  (list (* 0.5 (+ x1 x2))
        (* 0.5 (+ y1 y2))))

(defun rad-to-deg (radian-value)
  "Converts radians to degrees."
  (* radian-value (/ 180 pi)))

(defun deg-to-rad (degrees)
  "Converts degrees to radians."
  (* degrees (/ pi 180)))

(defun move-point-in-direction (x y angle distance)
  "Returns a list of x/y coordinates, where the given point (x, y) has moved DISTANCE in direction ANGLE."
  (list
   (+ x (* distance (cos (deg-to-rad angle))))
   (+ y (* distance (sin (deg-to-rad angle))))))

(defun angle-from-point-to-point (x1 y1 x2 y2)
  "Returns the angle from point x1y1 to x2y2."
  (let* ((rise (- y2 y1))
        (run (x-if-y (- x2 x1) 1 0))
        (slope (* 1.0 (/ rise run)))
        )
    (rad-to-deg (atan slope))))

(defun square (x)
  "Returns the square of x"
  (expt x 2))

(defun cube (x)
  "Returns the cube of x"
  (expt x 3))

(defmacro concatenate-non-nils (result-type &body sequences)
  "Concatenates all given items unless an item is nil. An item, if it's a list that contains other values besides nil, will still be concatenated. So long as the total value of any given item is not nil, it will be concatenated."
  (let ((new-seqs (remove-if #'null sequences)))
    `(concatenate ,result-type ,@new-seqs)))

(defun getf-last-in-plist (property-list indicator)
  "(getf-last (list :x 10 :x 20 :x 30)) -> 30."
  (let ((pos (position indicator property-list :from-end t)))
    (when pos
      (elt property-list (1+ pos)))))

(defun remove-duplicate-property-value-pairs (property-list &optional (from-end nil))
  "(list :x 10 :y 20 :x 30) => (list :y 20 :x 30)"
  (let ((keys (remove-duplicates (extract-property-keys property-list)))
        (result))
    (dolist (key keys)
      (ntack result key)
      (ntack result (funcall (if from-end
                                 #'getf-last-in-plist
                                 #'getf)
                             property-list key)))
    result))

(defun new-rotate-point-2d (x y angle)
  (let ((rotation-matrix ))))

(defun choose-property (sequence1 sequence2 property choice-func)
  "Returns the result of applying choice-func to the values of the given property in both given sequences."
  (apply choice-func
         (list
          (getf sequence1 (to-property property))
          (getf sequence2 (to-property property)))))

(defun min-property (sequence1 sequence2 property)
  "Returns the min of the values of the property in both given sequences. eg (min-property (list :x 3) (list :x 5)) => 3."
  (choose-property sequence1 sequence2 property #'min))

(defun max-property (sequence1 sequence2 property)
  "Returns the max of the values of the property in both given sequences. eg (min-property (list :x 3) (list :x 5)) => 5."
  (choose-property sequence1 sequence2 property #'max))

;; (defun equal? (x y)
;;   (choose-property (list :value x) (list :value y) :value (lambda (x y) (if (equal x y) t nil))))

(defun mkstr (&rest args)
  "Returns a string composed of the given args."
  (with-output-to-string (s)
    (dolist (arg args)
      (princ arg s))))

(defun symb (&rest args)
  "Returns a symbol composed of the given args."
  (values (intern (apply #'mkstr args))))

;; (defun collect-odds-between (start end)
;;   "Returns a list of all the odd numbers between start and end."
;;   (loop for i from start upto end
;;         when (oddp i)
;;         collect i))

;; (defun odds-between (start end)
;;   (let ((result))
;;     (dotimes (i (- end start))
;;       (when (oddp (+ start i))
;;         (ntack result (+ start i))))
;;     result))
(defun max-list (sequence)
  (let ((highest (first sequence)))
    (loop for i in sequence
          do (when (> i highest)
               (setf highest i)))
    highest))
