;;;; file-to-c-array.lisp

(in-package #:file-to-c-array)

(defparameter *command-line-args* '(("input" :required)
				    ("output" :required)
				    ("columns" :optional)))
(defconstant +default-num-columns+ 8)

(defun read-stream-into-array (input-stream)
  (let ((array-buffer (make-array (file-length input-stream) :element-type '(unsigned-byte 8) :fill-pointer t)))
    (setf (fill-pointer array-buffer) (read-sequence array-buffer input-stream))
    array-buffer))

(defun get-option-value (name parsed-options &optional default-value)
  (let ((option-value (cdr (assoc name parsed-options :test #'string-equal))))
    (if option-value
	option-value
	default-value)))

(defun write-row (input-subsequence output-stream last-row-p)
  (let ((row-length (length input-subsequence)))
    (format output-stream "    ")
    (loop
       for byte-value across input-subsequence
       for i from 1
       do (format output-stream
		  "0x~X~[~:;,~]"
		  byte-value
		  (- row-length i)))
    (unless last-row-p
      (format output-stream ","))
    (format output-stream " /* ")
    (loop
       for byte-value across input-subsequence
       for i from 1
       do (format output-stream
		  "~A~[ */~%~:;~]"
		  (if (graphic-char-p (code-char byte-value))
		      (code-char byte-value)
		      #\.)
		  (- row-length i)))
    (when last-row-p
      (format output-stream "};~%"))))


(defun write-c-array (input-sequence output-stream num-columns)
  (let ((num-bytes (length input-sequence)))
    (format output-stream "char const unsigned data[~A] = {~%" num-bytes)
    (loop
       for i from 0 to num-bytes by num-columns
       do (write-row
	   (subseq input-sequence i (min (+ i num-columns) num-bytes))
	   output-stream
	   (>= (+ i num-columns) num-bytes)))))

(defun interactive-main (argv command-line-args)
  (multiple-value-bind (args options errors) (getopt:getopt argv command-line-args)
    (format t "args=~A~%options=~A~%errors=~A~%" args options errors)
    (let ((input-filename (get-option-value "input" options))
	  (output-filename (get-option-value "output" options))
	  (num-columns (or (parse-integer (get-option-value "columns" options "") :junk-allowed t)
			   +default-num-columns+)))
      (when (or (null input-filename)
		(null output-filename))
	(return-from interactive-main))
      (with-open-file (input-stream input-filename :direction :input :element-type '(unsigned-byte 8))
	(with-open-file (output-stream output-filename :direction :output :if-exists :supersede)
	  (write-c-array (read-stream-into-array input-stream) output-stream num-columns)))))
  t)
	

(defun print-usage-and-exit (program-name)
  (format t "usage: ~A --input=FILE --output=FILE [--columns=NUM]~%" program-name)
  (format t "options:~%")
  (format t "    -i|--input FILE    Input filename~%")
  (format t "    -o|--output FILE   Output filename~%")
  (format t "    -c|--columns NUM   Number of columns to print (default=8)~%")
  #+sbcl (sb-ext:quit :unix-status 0))

(defun main ()
  (let ((argv #+sbcl sb-ext:*posix-argv*))
    (unless (interactive-main (cdr argv) *command-line-args*)
      (print-usage-and-exit (car argv)))))


