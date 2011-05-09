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
