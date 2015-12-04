
(in-package :user)

;(eval-when (:compile-toplevel :load-toplevel :execute)
 (defstruct piece 
   width 
   height 
   position 
   orientation)
 ;)


(setf 

 ;; Satisfaction: does not have a solution.
 p10a '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) )
       (17 6))

  ;; Satisfaction: does not have a solution.
 p10b '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) )
       (17 7))

  ;; Satisfaction: does not have a solution.
 p10c '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) )
       (17 8))




 ;; Satisfaction: does not have a solution.
 p1a '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 8))

 ;; Satisfaction: does have a tight solution.
 p1b '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 9))

 ;; Satisfaction: does have a solution.
 p1c '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 13))

 ;; Satisfaction: does not have a solution.
 p2a '((#S(PIECE :WIDTH 4 :HEIGHT 4 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 9))

 ;; Satisfaction: does have a tight solution.
 p2b '((#S(PIECE :WIDTH 4 :HEIGHT 4 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 10))

 ;; Satisfaction: does have a solution.
 p2c '((#S(PIECE :WIDTH 4 :HEIGHT 4 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 13))

 ;; Satisfaction: does not have a solution.
 p3a '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 12))

 ;; Satisfaction: does have a tight solution.
 p3b '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 13))

 ;; Satisfaction: does not have a solution
 pax '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 16 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 13))

 ;; Satisfaction: does have a solution.
 p3c '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 15))

 ;; Satisfaction: does not have a solution.
 p4a '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 13))

 ;; Satisfaction: does have a tight solution.
 p4b '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 14))

 ;; Satisfaction: does have a solution.
 p4c '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 17))




 ;; Satisfaction: does have a tight solution.
 p25b '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL))
       (15 11))
 
 ;; Satisfaction: does have a solution.
 p25c '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL))
       (15 13))

 ;; Satisfaction: does have a tight solution.
 p20b '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL))
       (14 10))

 ;; Satisfaction: does have a solution.
 p20c '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL))
       (14 12))


p40a '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 19))
       
 
p100a '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL) )
       (20 40))


 )
