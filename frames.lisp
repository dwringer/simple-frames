(defpackage :simple-frames (:use :common-lisp)
	    (:export :fassert :fexists :fget :fget-v-d :fget-v-d-f
		     :fgetframe :fgetclasses :fgetframe+ :fget-i
		     :fget-n :fget-z :fcheck :fput :fremove :fput+
		     :fremove+ :fput+i :fremove+i :fclamp :fclear
		     :ferase :ferase+ :freplace :freplace+ :fmethod
		     :fmethod-z :fmethod-n :fmethod-args :ako :$value
		     :$default :$if-needed :$if-added :$if-removed))
(in-package :simple-frames)

" A simple frame system implementing parts of the public interface to
    FRL and the interface described in Winston & Horn, _LISP_ (1st ed.)


Ex:
---
(fassert henry (ako        ($value ( man )))
               (height     ($value ( 178 )))
               (weight     ($value ( 75 )))
               (hobbies    ($value ( jogging ) ( swimming ) ( skiing )))
               (occupation ($value ( teaching ))))
"

(defmacro aif (test then &optional else (as 'it))
  "Anaphoric IF macro"
  `(let ((,as ,test))
     (if ,as ,then ,else)))

(defun >> (f &rest args)
  "Call f(args[n-1], f(args[n-2], f(args[...], f(args[0], args[n]))))"
  (let ((arglen (length args)))
    (when (< 1 arglen)
      (let ((next (funcall f (first args) (first (last args)))))
	(cond ((= 2 arglen) next)
	      (t (apply #'>>
			(append (cons f (subseq args 1 (- arglen 1)))
				(list next)))))))))

(defun get-value (symbol indicator &optional default)
  "Directly retrieve value mapped by result from calling GET"
  (rest (get symbol indicator default)))

(defun assoc-value (item alist &rest assoc-args)
  "Directly retrieve value mapped by result from calling ASSOC"
  (rest (apply #'assoc (cons item (cons alist assoc-args)))))

(defun assoc! (item alist &key key test test-not)
  "Performs ASSOC lookup for a given item and adds it if not present"
  (labels ((i-cons (cons i)  ;: Retrieve Ith cons cell of linked list CONS
	     (if (= i 0)
		 cons
		 (i-cons (rest cons) (- i 1)))))
    (let ((found (apply #'assoc
			(append (list item     alist :key      key)
				(when test     (list :test     test))
				(when test-not (list :test-not test-not))))))
      (if found found  ; else:
	  (first (setf (rest (i-cons alist (- (length alist) 1)))
		       (list (cons item nil))))))))

(defun named-assoc (k named-alist) (assoc k (rest named-alist)))

(defun named-assoc! (item named-alist &key key test test-not)
  "Call ASSOC! but treat the first slot of the alist as a name (ignore)"
  (if (< 1 (length named-alist))
      (apply #'assoc!
	     (append (list item (rest named-alist) :key      key)
		     (when test       (list        :test     test))
		     (when test-not   (list        :test-not test-not))))
      (first (setf (rest named-alist)
		   (list (cons item nil))))))

(defun fexists (frame)
  (declare (special frame))
  (not (null (get frame 'frame))))

(defun function-lambda-list (fun)
  (multiple-value-bind (lambda-expr closure-p name)
      (function-lambda-expression fun)
    (declare (ignore closure-p name))
    (when (not (null lambda-expr))
      (if (listp (cadr lambda-expr))
	  (cadr lambda-expr)
	  (caddr lambda-expr)))))

(defmacro fassert (frame &rest slot-expressions)
  "If FRAME exists, merge SLOT-EXPRESSIONS with it; else create it with them."
  (let ((assignment-expressions
	 (mapcan #'(lambda (slot-expr)
	     (aif (cdr slot-expr)
		  (mapcan #'(lambda (facet-expr)
		      (mapcar #'(lambda (value)
			  (append
			   `(fput ',frame
				  ',(car slot-expr)
				  ',(car facet-expr))
			   (if (symbolp (car value))
			       `((locally (declare (special ,(car value)))
				   (if (not (boundp ',(car value)))
				       ',(car value)
				       ,(car value))))
			       `(,(car value)))))
			      (cdr facet-expr)))
			  it)
		  (list `(named-assoc! ',(car slot-expr)
				       (fgetframe ',frame)))))
		 slot-expressions)))
    `(prog1 nil ,@assignment-expressions)))

(defun fget (frame slot facet)
  "Retrieve the value from a frame by specifiying path into frame structure"
  (do* ((values (>> #'assoc-value slot facet (get-value frame 'frame)))
	(remain values (rest remain))
	(result nil))
       ((null remain) (reverse result))
    (push (caar remain) result)))

(defun fget-v-d (frame slot)
  "Return the '$value facet of FRAME->SLOT, if it exists, else '$default facet"
  (aif (fget frame slot '$value)
       it
       (fget frame slot '$default)))

(defun fget-v-d-f (frame slot)
  "Same as fget-v-d, but if it fails, invokes the '$if-needed facet demon"
  (aif (fget frame slot '$value)
       it
       (aif (fget frame slot '$default)
	    it
	    (mapcan #'funcall (fget frame slot '$if-needed)))))

(defun fgetframe (symbol)
  "Retrieve frame assigned to SYMBOL if it exists, else set & return a new one"
  (let ((binding (get symbol 'frame)))
    (if binding binding (setf (get symbol 'frame) (cons symbol nil)))))

(defun fgetclasses (frame &optional _result _front _visited _root)
  "Retrieve a list of all unique frames linked to FRAME via inheritance"
  (when (null _root) (setf _root frame))
  (push frame _visited)
  (let* ((immediate-relatives (fget-v-d-f frame 'ako)))
    (if (null _result)
	(progn (setf _result immediate-relatives)
	       (setf _front  immediate-relatives))
	(dolist (rel immediate-relatives)
	  (if (not (or (member rel _result) (equal rel _root)))
	      (push rel _result))
	  (if (not (member rel _visited))
	      (push rel _front))))
    (if (null _front)
	(reverse _result)
	(fgetclasses (car _front)
		     _result
		     (set-difference _front _visited) 
		     _visited
		     _root))))

(defun fgetframe+ (frame)
  (apply #'append (cons (fgetframe frame)
			(mapcar #'(lambda (fr)
				    (cdr (fgetframe fr)))
				(fgetclasses frame)))))
	  

(defun fget-i (frame slot)
  "Get $value facet of FRAME->SLOT; while not found ascend the AKO hierarchy"
  (aif (fget frame slot '$value)
       it
       (do* ((classlist (fgetclasses frame) (cdr classlist))
	     (next      (fget (car classlist) slot '$value)
			(fget (car classlist) slot '$value)))
	    ((or next (null classlist)) next))))

(defun fget-n (frame slot)
  "Search AKO hierarchy for $value; failing that $default, then ($if-needed)"
  (aif (fget frame slot '$value)
       it
       (aif (do* ((classlist (fgetclasses frame) (cdr classlist))
		  (next      (fget (car classlist) slot '$value)
			     (fget (car classlist) slot '$value)))
		 ((or next (null classlist)) next))
	    it
	    (aif (fget frame slot '$default)
		 it
		 (aif (do* ((classlist (fgetclasses frame) (cdr classlist))
			    (next      (fget (car classlist) slot '$default)
				       (fget (car classlist) slot '$default)))
			   ((or next (null classlist)) next))
		      it
		      (aif (mapcan #'funcall (fget frame slot '$if-needed))
			   it
			   (do* ((classlist (fgetclasses frame) (cdr classlist))
				 (next      (mapcan #'funcall
						    (fget (car classlist) slot
							  '$if-needed))
					    (mapcan #'funcall
						    (fget (car classlist) slot
							  '$if-needed))))
				((or next (null classlist)) next))))))))

(defun fget-z (frame slot)
  "Ascend AKO hierarchy looking for first $value, $default, or ($if-needed)"
  (aif (fget-v-d-f frame slot)
       it
       (do* ((classlist (fgetclasses frame) (cdr classlist))
	     (next      (fget-v-d-f (car classlist) slot)
			(fget-v-d-f (car classlist) slot)))
	    ((or next (null classlist)) next))))

(defun fcheck (frame slot facet value)
  "Returns whether values stored at FRAME->SLOT->FACET contain VALUE"
  (when (member value (fget frame slot facet)) t))

(defun fput (frame slot facet value)
  "Add a value to a frame by specifying path into frame structure"
  (when (not (fcheck frame slot facet value))
    (>> #'named-assoc! slot facet value (fgetframe frame))
    value))

(defun fremove (frame slot &optional facet value)
  "Remove the value stored (if any) in the specified frame/path."
  (labels ((delete-without-warning (item sequence)
	     (let ((deletion-result (delete item sequence)))
	       (unless (listp (delete item sequence))
		 (error "Item deletion error (~A from ~A yields ~A)"
			item sequence deletion-result)))))
    (when (null value)
	(setf (cdr (>> #'named-assoc! slot (if facet facet '$value)
		       (fgetframe frame)))
	      '((t)))
	(setf value t))
    (let* ((slots          (fgetframe frame))
	   (facets         (named-assoc slot  slots))
	   (values         (named-assoc (if facet facet '$value) facets))
	   (item-to-remove (named-assoc value values)))
      (delete-without-warning (if (null item-to-remove)
				  (list value)
				  item-to-remove)
			      values)
      (when (null (rest values)) (delete-without-warning values facets))
      (when (null (rest facets)) (delete-without-warning facets slots))
      (not (null item-to-remove)))))

(defun fput+ (frame slot facet value)
  "Call FPUT, ensuring to call all [inherited] $if-added demons"
  (when (fput frame slot facet value)
    (let ((if-added-demons (fget frame slot '$if-added)))
      (when if-added-demons
	(mapcan #'(lambda (f) (funcall f value frame))
		if-added-demons)))
    value))

(defun fput+i (frame slot facet value)
  (when (fput frame slot facet value)
    (let ((if-added-demons (append (fget frame slot '$if-added)
				   (mapcan #'(lambda (fr) (fget fr slot '$if-added))
					   (fgetclasses frame)))))
      (when if-added-demons
	(mapcan #'(lambda (f) (funcall f value frame))
		if-added-demons)))
    value))
    
(defun fremove+ (frame slot &optional facet value)
  "Call FREMOVE, ensuring to call slot's $if-removed demon(s)"
  (when (fremove frame slot facet value)
    (prog1 t
      (let ((if-removed-demons (fget frame slot '$if-removed)))
	(when if-removed-demons
	  (mapcan #'(lambda (f)
		      (funcall f value frame))
		  if-removed-demons))))))

(defun fremove+i (frame slot &optional facet value)
  "Call FREMOVE, ensuring to call all inherited $if-removed demons"
  (when (fremove frame slot facet value)
    (prog1 t
      (let ((if-removed-demons (append (fget frame slot '$if-removed)
				       (mapcan #'(lambda (fr)
						   (fget fr slot
							 '$if-removed))
					       (fgetclasses frame)))))
	(when if-removed-demons
	  (mapcan #'(lambda (f) (funcall f value frame))
		  if-removed-demons))))))

(defun fclamp (frame from-frame slot)
  "Force FRAME to share same SLOT structure in memory as FROM-FRAME (junction)"
  (setf (rest (named-assoc! slot (fgetframe frame)))
	(rest (named-assoc! slot (fgetframe from-frame)))))

(defun fclear (frame &optional slot facet)
  (if slot
      (if facet
	  (setf (cdr (assoc facet (cdr (assoc slot (cdr (fgetframe frame))))))
		nil)
	  (setf (cdr (assoc slot (cdr (fgetframe frame)))) nil))
      (setf (get frame 'frame) (list frame))))

(defun ferase (frame &optional slot facet)
  (if slot
      (progn
	(setf (cdr (>> #'named-assoc! slot (if facet facet '$value)
		       (fgetframe frame)))
	      '((t)))
	(fremove frame slot (if facet facet '$value) t))
      (setf (get frame 'frame) nil)))

(defun ferase+ (frame &optional slot facet)
  (let* ((frame-obj (fgetframe frame))
	 (slots (if slot
		    (list (named-assoc slot frame-obj))
		    (cdr frame-obj))))
    (dolist (slt slots)
      (let ((facets (if facet
			(list (named-assoc facet slt))
			(cdr slt))))
	(dolist (fct facets)
	  (dolist (value-obj (cdr fct))
	    (fremove+ frame (car slt) (car fct) (car value-obj)))
	  (setf (cdr fct) '((t)))
	  (fremove frame (car slt) (car fct) t))))))

(defun freplace (frame slot facet value)
  (fclear frame slot facet)
  (fput frame slot facet value))

(defun freplace+ (frame slot facet value)
  (ferase+ frame slot facet)
  (fput+ frame slot facet value))

(defun fmethod (frame slot &rest args)
  (apply (car (fget-v-d-f frame slot)) (cons frame args)))

(defun fmethod-args (frame slot)  ;; contraindicated; almost always NIL
  (function-lambda-list (car (fget-z frame slot))))

(defun fmethod-n (frame slot &rest args)
  (apply (car (fget-n frame slot)) (cons frame args)))

(defun fmethod-z (frame slot &rest args)
  (apply (car (fget-z frame slot)) (cons frame args)))
