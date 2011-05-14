;; wizards game

;; define the 3 places (*nodes*) where the games takes palce.
(defparameter *nodes* '((living-room (you are in the living-room. 
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))

;; define a function to describe the location.
(defun describe-location (location node)
  (cadr (assoc location node)))

;; define the paths (*edges*) a player can take from each node.
(defparameter *edges* '((living-room (garden west door)
			             (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; defin a function to describe the path from a given node.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; define a function to describe /all/ the paths from a given node.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; define some objects
(defparameter *objects* '(whiskey bucket frog chain))

;; define where the objectes are
(defparameter *object-locations* '((whiskey living-room)
				  (bucket living-room)
				  (frog garden)
				  (chain garden)))

;; define a function that lists the objects visible from a given location
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; define a function to describe the objects visible at a given location
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; define players location
(defparameter *location* 'living-room)

;; define a look function
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; define a function that lets us walk around
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you can't go that way.))))

;; define a function to pick up items 
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

 ;; define a function to see an inventory
(defun inventory ()
  (cons 'items-  (objects-at 'body *objects* *object-locations*)))

;; define a repl for the game
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; define a custom (read) for our repl
(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; a list of allowed commands
(defparameter *allowed-commands* '(look walk pickup inventory))

;; define a custom (eval) for our repl
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))

;; define a function to tweak text
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;; define a custom (print) for our repl
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))
