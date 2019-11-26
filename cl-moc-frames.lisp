(require 'asdf)

(defpackage :cl-moccasin
  (:use :common-lisp :uiop)
  (:nicknames :cl-moc)
  (:export :interpreter :hosted-object :initialize :send :receive
	   :lines :ping :wait :peek :kill :psw :interrupt)

(labels ((strcat (&rest strings) (apply #'concatenate (cons 'string strings)))
	 
	 (trim-eol (string)
	   (string-right-trim (format nil "~C" #\return) string))
	 
	 (trim-prompt (string prompt)
	   (let ((slen (length string))
		 (plen (length prompt)))
	     (if (and (> slen (- plen 1))
		      (string-equal prompt (subseq string 0 plen)))
		 (subseq string plen slen)
		 string)))
	 
	 (print-strings (string-list &optional (separator #\newline))
	   (when (not (null string-list))
	     (format t (strcat "~{~A~^" (format nil "~C" separator) "~}")
		     string-list)
	     string-list))

	 (read-line-no-hang (stream)
	   (let ((acc (make-array 0
				  :element-type 'character
				  :fill-pointer 0
				  :adjustable   t))
		 (char (read-char-no-hang stream)))
	     (do ((i 0 (+ 1 i)))
		 ((or (null char) (equal char #\newline))
		  (values (trim-eol acc) (null char)))
	       (vector-push-extend char acc)
	       (setf char (read-char-no-hang stream)))))
	 
	 (read-lines-no-hang (stream)
	   (let ((acc    nil)
		 (line   nil)
		 (isnil  nil)
		 (remain nil))
	     (do ((i 0 (+ 1 i)))
		 (isnil (values (reverse acc) remain))
	       (multiple-value-setq (line isnil) (read-line-no-hang stream))
	       (if (not isnil)
		   (push line acc)
		   (setf remain line))))))
  
  (defun send (frame string)
    "Send the given string to stream, then finish-output."
    (let ((stream (uiop:process-info-input
		   (car (fget-z frame 'process)))))
      (write-line string stream)
      (finish-output stream)))
  
  (defun lines (frame)
    "Retrieve current lines of output buffer from stream."
    (let ((previous-remains (car (fget-z frame 'buffered-output-lines)))
	  (prompt           (car (fget-z frame 'prompt))))
      (multiple-value-bind (lines remains)
	  (read-lines-no-hang (uiop:process-info-output
			       (car (fget-z frame 'process))))
	(if (< 0 (length lines))
	    (progn (when (not (null previous-remains))
		     (setf lines
			   (cons (strcat previous-remains (car lines))
				 (cdr lines)))))
	    (setf remains (strcat previous-remains remains)))
	(fremove frame 'buffered-output-lines '$value previous-remains)
	(when (not (= 0 (length remains)))
	  (fput+ frame 'buffered-output-lines '$value remains))
	(mapcar #'(lambda (x) (trim-prompt x prompt)) lines))))

  (defun receive (frame)
    "Print lines of buffered output from stream"
    (let* ((lines  (fmethod-z frame 'lines))
	   (llen   (length lines))
	   (check  (car (fget-z frame 'identity-expr)))
	   (prompt (car (fget-z frame 'prompt))))
      (when (and (car (fget-z frame 'ping-alive))
		 (< 0 llen)
		 (or (string-equal check (elt lines (- llen 1)))
		     (string-equal check
				   (trim-prompt (elt lines (- llen 1))
						prompt))))
	(setf lines (subseq lines 0 (- llen 1)))
	(fremove+ frame 'ping-alive '$value t))
      (print-strings lines)))

  (defun ping (frame &optional (persist-for 2) (reset nil))
    "Check for a response from the interpreter stream"
    (when reset (fremove+ frame 'ping-alive '$value t))
    (let ((check (car (fget-z frame 'identity-expr)))
          (ping  (car (fget-z frame 'ping-alive))))
      (when (null ping)
	(fmethod-z frame 'send check)
	(fput+ frame 'ping-alive '$value 't))
      (let ((τ (get-internal-real-time))
            (δ (* persist-for internal-time-units-per-second)))
	(do ((response nil)
	     (θ 0))
	    ((or (<=  (+ τ δ) θ) (not (null response))) response)
	  (setf θ (get-internal-real-time))
	  (let* ((lines  (fmethod-z frame 'lines))
		 (llen   (length lines))
		 (prompt (car (fget-z frame 'prompt))))
	    (when (< 0 llen)
	      (when (or (string-equal check (elt lines (- llen 1)))
			(string-equal check
				      (trim-prompt (elt lines (- llen 1))
						   prompt)))
		(setf lines (subseq lines 0 (- llen 1)))
		(setf response 'pong)
		(fremove frame 'ping-alive '$value t))
	      (print-strings lines)))))))

  (defun wait (frame)
    "Read/print lines from stream, blocking until control is restored."
    (let ((lines (fmethod-z frame 'lines))
	  (check (car (fget-z frame 'identity-expr))))
      (fmethod-z frame 'send check)
      (do* (done
	    result
	    (i          0                        (+ i 1))
	    (more-lines (fmethod-z frame 'lines) (fmethod-z frame 'lines))
	    (llen       (length more-lines)      (length more-lines)))
	   (done result)
	(when more-lines
	  (print-strings lines)
	  (if (string-equal check (elt more-lines (- llen 1)))
	      (progn (setf more-lines (subseq more-lines 0 (- llen 1)))
		     (print-strings more-lines)
		     (setf result (append lines more-lines))
		     (setf done t))
	      (setf lines more-lines))))))

  (defun peek (frame)
    "Peek at the contents of last-seen unterminated line in buffer (if any)."
    (let ((buffer (fget-z frame 'buffered-output-lines)))
      (when (not (null buffer))
	(format t "~A" buffer))))

  (defun kill (frame)  ;; TODO: support other platforms
    "Forcibly kill the running process & remove references."
    (let ((pid    (uiop:process-info-pid (car (fget-z frame 'process))))
	  (status 0))
      ;;      (uiop:terminate-process (car (fget-z frame 'process)) :urgent t)
      #-(or win32 windows mswindows)
      (multiple-value-bind (x y st)
	  (uiop:run-program
	   (format nil "pkill TERM -P ~d" pid))
	(declare (ignore x y))
	(setf status st))
      #+(or win32 windows mswindows)
      (multiple-value-bind (x y st)
	  (uiop:run-program ;call     ;tree|force 
	   (format nil "cmd /C taskkill /T /F /PID ~d" pid))
	(declare (ignore x y))
	(setf status st))
      (fremove+i frame 'process)
      status))

  (defun psw (frame string)
    "(Progn (Send (Wait ...)))"
    (fmethod-z frame 'send string)
    (fmethod-z frame 'wait))

  (defun initialize (frame &key monitor-interrupts?)
    "Start the process & streams [& client interrupt monitor thread (opt)]"
    (let* ((process (uiop:launch-program (format nil "~A~{~^ ~A~}"
						 (car (fget-z frame 'path))
						 (car (fget-z frame 'argument)))
					 :input  :stream
					 :output :stream)))
      (fput frame 'process '$value process)
      (when monitor-interrupts?
	(fmethod-z frame 'psw (format nil "
def watch_for_keyboard_interrupt():
    from os import remove
    from sys import version_info
    from time import sleep
    if version_info.major >= 3:
        from _thread import interrupt_main
    else:
        from thread import interrupt_main
    filename = '~A_interrupt.sentinel'
    while True:
        try:
            _interrupt = False
            with open(filename, 'rb') as inf:
                if len(inf.readlines()) > 0:
                    _interrupt = True
            if _interrupt:
                interrupt_main()
                with open(filename, 'wb') as outf:
                    _nil = outf
                remove(filename)
        except Exception:
            pass
        sleep(0.5)
" (string frame)))
	(fmethod-z frame 'psw "from six.moves._thread import start_new_thread")
	(fmethod-z frame 'psw
		   (strcat "_CL_INTERRUPT = start_new_thread("
			   "watch_for_keyboard_interrupt, ())")))
      process))

  (defun interrupt (frame &optional (timeout 5))
    "Signal [using named file] monitor thread to raise KeyboardInterrupt"
    (with-open-file (outf (format nil "~A_interrupt.sentinel"
				  (string frame))
			  :direction :output
			  :if-exists :supersede)
      (format outf "~C" #\newline))
    (fmethod-z frame 'ping timeout t))

  (fassert interpreter 

	   (path                   ($default ( #P"python.exe" )))
	   (argument               ($default ( '("-i"))))
	   (prompt                 ($default ( ">>> ")))
	   (identity-expr          ($default ( "()")))
	   (stream)
	   (process)
	   (buffered-output-lines)
	   (ping-alive)

	   (init                   ($value   ( #'initialize )))
	   (send                   ($value   ( #'send )))
	   (recv                   ($value   ( #'receive )))
	   (lines                  ($value   ( #'lines )))
	   (ping                   ($value   ( #'ping )))
	   (wait                   ($value   ( #'wait )))
	   (peek                   ($value   ( #'peek )))
	   (kill                   ($value   ( #'kill )))
	   (psw                    ($value   ( #'psw )))
	   (interrupt              ($value   ( #'interrupt )))))

(fassert hosted-object
	 (interpreter )
	 (handle      )

	 (acquire     ($value
		       (#'(lambda (self frame &optional (value "None"))
			    (when (fcheck frame 'ako '$value 'interpreter)
			      (let ((handle (gensym "hosted_object_")))
				(fput self 'handle      '$value handle)
				(fput self 'interpreter '$value frame)
				(fmethod-z frame 'psw
					   (format nil "~a = ~a;"
						   handle value))))))))
	 (assign      ($value
		       (#'(lambda (self value)
			    (fmethod-z (car (fget-z self 'interpreter))
				       'psw
				       (format nil "~a = ~a;"
					       (car (fget-z self 'handle))
					       value))))))
	 
	 (retrieve    ($value
		       (#'(lambda (self)
			    (fmethod-z (car (fget-z self 'interpreter))
				       'psw
				       (format nil "~a;"
					       (car (fget-z self
							    'handle)))))))))
