; grupo 18 Daniel Brandao 76245 Miguel Duarte 85016

(in-package :user)

(compile-file "procura.lisp")
(load "procura")
(load "PP-examples.lisp") ;--> capaz de dar warning visto que a estrutura piece esta redifinida	

;;;;;;;;;;;;;;;;;;;PROVIDED;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct piece
 width
 height
 position
 orientation) 

(defun piece-h (p)
	(if (equal (piece-orientation p) 'V)
		(piece-height p)
		(piece-width p)
	)
)
(defun piece-w (p)
	(if (equal (piece-orientation p) 'V)
		(piece-width p)
		(piece-height p)
	)
)
(defun piece-y (p)
	(first (piece-position p))
)
(defun piece-x (p)
	(second (piece-position p))
)

(defun make-pos (h v) ;; h-horizontal v-vertical
	(list h v))
(defun pos-h (pos)
	(first pos))
(defun pos-v (pos)
	(second pos))
(defun pos=? (p1 p2)
	(equal p1 p2)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;LIST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun copy-except (l p)
    (let ((newl (copy-list l)))
	(setf newl (delete-first-eq-from-list p newl))))



(defun agregate (l1 l2)
	(cond ((null l1) l2)
		  (T (cons (first l1) (agregate (rest l1) l2)))))
	   
;;;;;;;;;;;;;;;;;;;;;SATISFACTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct rect pecas-i posicoes pecas-f width height)

;estado inicial
(defun inicial (pecas w h)
	(let ((area 0) (pos (list (make-pos 0 0))) )
		(dolist (p pecas)
			(setf area (+ area (* (piece-width p) (piece-height p)))))
		(if (> area (* w h)) (setf pos nil))

	(make-rect :pecas-i pecas
			:pecas-f (list)
			:posicoes pos
			:width w
			:height h)))

;objectivo - todos as pecas tiverem atribuidas			
(defun objectivo (r)
	(null (rect-pecas-i r)))

;encaixa-peca-? - recebe o estado r, peca p, posicao pos e orientacao o
;retorna T or NIL se consegue encaixar a peca ou nao
(defun encaixa-peca-? (r p pos o)
	(let (w h w2 h2)
		(cond ((equal o 'H) (setf w (piece-width p)) (setf h (piece-height p)))
			((equal o 'V) (setf w (piece-height p)) (setf h (piece-width p))))

		;se tiverem fora das dimensoes do rectangulo, falha logo
		(if (>  w (- (rect-width r) (pos-h pos))) (return-from encaixa-peca-? nil))
		(if (>  h (- (rect-height r) (pos-v pos))) (return-from encaixa-peca-? nil))
		
		(dolist (p2 (rect-pecas-f r) T)
			(cond ((equal (piece-orientation p2) 'H) (setf w2 (piece-width p2)) (setf h2 (piece-height p2)))
			((equal (piece-orientation p2) 'V) (setf w2 (piece-height p2)) (setf h2 (piece-width p2))))
			(if (intersect pos w h (piece-position p2) w2 h2)
				(return-from encaixa-peca-? nil)))))


(defun intersect (pos1 w1 h1 pos2 w2 h2)
	(not (or (>= (pos-h pos2) (+ (pos-h pos1) w1))
			(>= (pos-h pos1) (+ (pos-h pos2) w2))
			(>= (pos-v pos2) (+ (pos-v pos1) h1))
			(>= (pos-v pos1) (+ (pos-v pos2) h2)))))
					
					
;encaixa - recebe o estado r, peca p, posicao pos e orientacao o
;retorna o estado apos o encaixe da peca
(defun encaixa (r p pos o)
	(let (w h
	(newpos (copy-except (rect-posicoes r) pos))
	(newpiece (copy-piece p)))
		(cond ((equal o 'H) (setf w (piece-width p)) (setf h (piece-height p)))
			((equal o 'V) (setf w (piece-height p)) (setf h (piece-width p))))
	
	(setf (piece-position newpiece) pos)
	(setf (piece-orientation newpiece) o)
	(if (> (rect-width r) (+ (pos-h pos) w)) (setf newpos (agregate newpos (list (list (+ (pos-h pos) w) (pos-v pos))))))
	(if (> (rect-height r) (+ (pos-v pos) h)) (setf newpos (agregate newpos (list (list (pos-h pos) (+ (pos-v pos) h))))))
	(make-rect :pecas-i  (copy-except (rect-pecas-i r) p)
		:pecas-f (append (rect-pecas-f r) (list newpiece))
		:posicoes (verify r newpos)
		:width (rect-width r)
		:height (rect-height r))))
		
(defun verify (r newpos)
	(dolist (pos newpos newpos)
		(dolist (p (rect-pecas-f r))
			(if (inside pos p) (setf newpos (delete-first-eq-from-list pos newpos))))))


(defun inside (pos p)
	(let (w h (pos2 (piece-position p)))
	(cond ((equal (piece-orientation p) 'H) (setf w (piece-width p)) (setf h (piece-height p)))
		  ((equal (piece-orientation p) 'V) (setf w (piece-height p)) (setf h (piece-width p))))
	(and (<= (pos-h pos2) (pos-h pos)) (< (pos-h pos) (+ (pos-h pos2) w))
		(<= (pos-v pos2) (pos-v pos)) (< (pos-v pos) (+ (pos-v pos2) h)))))
			
;operator - recebe o estado r e retorna todos os estados possiveis derivados deste				
(defun operator (r)
	(let (rs
			(pecas-i (rect-pecas-i r))
			(posicoes (rect-posicoes r)))
		(dolist (p pecas-i rs)
			(dolist (pos posicoes)
				(dolist (o '(H V))
					(cond ((encaixa-peca-? r p pos o) (push (encaixa r p pos o) rs))))))
		(values rs)))

(defun printState(s)
	(when (null s)
		(print NIL)
		(return-from printState NIL)
	)

	(let ((m 
			(make-array (list (rect-width s) (rect-height s) ) :initial-element 0)
		) p px py )
		(loop for i in (range (length (rect-pecas-f s) ) ) do
			(setf p (nth i (rect-pecas-f s)))

			(loop for y in (range (piece-h p)) do
				(loop for x in (range (piece-w p)) do
					(setf px (+ x (piece-x p) ) )
					(setf py (+ y (piece-y p) ) )
					(setf (aref m py px) (1+ i) )
				)
			)
		)
		(print m)
		
	)
		
)

;h-area: h = Area - areas encaixadas
(defun h-area (r)
	(let ((area 0))
		(dolist (p (rect-pecas-f r))
			(setf area (+ area (* (piece-width p) (piece-height p)))))
	(- (* (rect-width r) (rect-height r)) area))) 

;h-comp: h = Area - maiores comprimentos
(defun h-comp (r)
	(let ((comp 0))
		(dolist (p (rect-pecas-f r))
			(setf comp (+ comp (max (piece-width p) (piece-height p)))))
	(- (* (rect-width r) (rect-height r)) comp)))

(defun h-comp2 (r)
	(let ((comp 0))
		(dolist (p (rect-pecas-f r))
			(setf comp (+ comp (piece-width p) (piece-height p))))
	(- (* (rect-width r) (rect-height r)) comp)))
(defun h-pos (r)
	(let ((p 0))
		(dolist (pos (rect-posicoes r))
			(setf p (1+ p)))
		(- (* (rect-width r) (rect-height r)) p)))
	
(defun h-pos2 (r)
	(let ((p 0))
		(dolist (pos (rect-posicoes r))
			(setf p (+ p (first pos) (second pos))))
		(- (* (rect-width r) (rect-height r)) p)))
			
(defun complicated(r)
	(let ((a 0))
		(dolist (pos (rect-posicoes r))
			(dolist (p (rect-pecas-i r))
				(if (or (<(- (rect-width r) (pos-h pos)) (min (piece-width p)(piece-height p)))
						(<(- (rect-height r) (pos-v pos)) (min (piece-width p)(piece-height p))))
						(return-from complicated most-positive-fixnum)
						(setf a (1+ a)))))
		a))

(defun complicated2(r)
	(let ((a 0))
		(dolist (pos (rect-posicoes r))
			(dolist (p (rect-pecas-i r))
				(if (or (<(- (rect-width r) (pos-h pos)) (min (piece-width p)(piece-height p)))
						(<(- (rect-height r) (pos-v pos)) (min (piece-width p)(piece-height p))))
						(return-from complicated2 most-positive-fixnum)
						(setf a (1+ a)))))
		(+ (h-area r) (* 1.3 a))))


(defun comp-state(h a b)
	(< (funcall h a) (funcall h b) )
)



(defun ILDS (state h)
	(let ( (size (length (rect-pecas-i state) ) ) sol)
		(loop for disc in (range (1+ size) ) do
			(print disc)
			(setf sol (LDS state h disc))
			(if sol
				(return-from ILDS sol))
		)
	)(time (procura (cria-problema (inicial (first p1b) (first (second p1b)) (second(second p1b))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-comp2) "a*" :espaco-em-arvore? T))
	NIL
)

(defun LDS (state h rem-disc)
	(let ((succ (operator state)) f sol)
		(if (objectivo state)
			(return-from LDS state)
		)
		(if (null succ) 
			(return-from LDS NIL)
		)

		
		(sort succ (lambda (a b) (comp-state h a b) ) )

		(setf sol (LDS (first succ) h rem-disc))
		(if sol
			(return-from LDS sol))
		(if (> rem-disc 0)
			(loop for child in (cdr succ) do
				(setf sol (LDS child h (1- rem-disc)))
				(if sol
					(return-from LDS sol))
			)
		)

	)
)

(defun samp (state )
	(let ((succ (operator state)) f sol)
		(if (objectivo state)
			(return-from samp state)
		)
		(if (null succ) 
			(return-from samp NIL)
		)

		(return-from samp (samp (nth (random (length succ)) succ)))

	)
)

(defun iterative-sampling(state )
	(let (sol)
		(loop do
			(setf sol (samp state))
			(if sol
				(return-from iterative-sampling sol))
		while T)
	)
)

(setf 

 ;; Satisfaction: does not have a solution.
test1 '((#S(PIECE :WIDTH 3 :HEIGHT 6 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 2 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 1 :HEIGHT 6 :POSITION NIL :ORIENTATION NIL) 
	  #S(PIECE :WIDTH 1 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL))
       (10 4))
)
;;; (load "G018.lisp")
;(time (setf s (procura (cria-problema (inicial (first p1a) (first (second p1a)) (second(second p1a)) ) (list #'operator) :objectivo? #'objectivo :estado= #'equal) "profundidade" :espaco-em-arvore? T)))
;(time (setf s (procura (cria-problema (inicial (first test1) (first (second test1)) (second(second test1)) ) (list #'operator) :objectivo? #'objectivo :estado= #'equal) "profundidade" :espaco-em-arvore? T)))
;(printState (car (last (first s))) )

(setf ini (inicial (first p2c) (first (second p2c)) (second(second p2c)) ))

(printState (ILDS ini #'h-area))
(printState (iterative-sampling ini))




;;; (time (procura (cria-problema (inicial (first p4b) (first (second p4b)) (second(second p4b))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'complicated) "a*" :espaco-em-arvore? T))
;;; (time (procura (cria-problema (inicial (first p1b) (first (second p1b)) (second(second p1b))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-comp2) "a*" :espaco-em-arvore? T))


(defun place-pieces (r p)
	(cond ((equal p "best.approach.satisfaction") T)
		  ((equal p "a*.best.heuristic") (rect-pecas-f (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'complicated2) "a*" :espaco-em-arvore? T))))))
		  ((equal p "a*.best.alternative.heuristic") (rect-pecas-f (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-comp) "a*" :espaco-em-arvore? T))))))
		  ((equal p "iterative.sampling.satisfaction") (rect-pecas-f (iterative-sampling (inicial (first r) (first (second r)) (second(second r))))))
		  ((equal p "ILDS") (rect-pecas-f (ILDS (inicial (first r) (first (second r)) (second(second r))) #'h-area)))
		  ((equal p "best.approach.optimization") T)
		  ((equal p "iterative.sampling.optimization") T)
		  ((equal p "alternative.approach.optimization") T)))








