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
			             (attic upstars ladder))
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
