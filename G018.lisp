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

(defun get-elapsed(start)
	(- (get-universal-time) start)
)

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
(defun inicial (pecas w h &optional (verificapecas nil))
	(let ((area 0) (pos (list (make-pos 0 0))) )
	
	(cond ((not (null verificapecas))
		(dolist (p pecas)
			(setf area (+ area (* (piece-width p) (piece-height p))))
			(if (or (> (max (piece-width p)(piece-height p)) (max w h))
				(> (min (piece-width p)(piece-height p)) (min w h)))
					(setf pos nil)))
		(if (> area (* w h)) (setf pos nil))))
		

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

(defun create-matrix(s)
	(let ((m 
			(make-array (list (rect-width s) (if (> (rect-height s) 1000) 50 (rect-height s)) ) :initial-element 0)
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
		m
	)
)

(defun printState(s)
	(when (null s)
		(print NIL)
		(return-from printState NIL)
	)
	(print (create-matrix s))
	
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
	(-  comp)))

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

(defun piece-playable (r p)
	(let ( (posicoes (rect-posicoes r)) )
		(dolist (pos posicoes)
			(dolist (o '(H V))
				(if (encaixa-peca-? r p pos o)
					(return-from piece-playable T))
			)
		)
		NIL
	)
)

(defun all-pieces-playable (r)
	(let ( (pecas-i (rect-pecas-i r)) )
		(dolist (p pecas-i)
			(if (null (piece-playable r p) ) (return-from all-pieces-playable NIL))
		)
	)
	T
)

(defun complicated(r)
	(let ((a 0))
		(dolist (pos (rect-posicoes r))
			(dolist (p (rect-pecas-i r))
				(if (or (<(- (rect-width r) (pos-h pos)) (min (piece-width p)(piece-height p)))
						(<(- (rect-height r) (pos-v pos)) (min (piece-width p)(piece-height p))))
						(return-from complicated most-positive-fixnum)
						(setf a (1+ a)))))
		a))

(defun best-heuristic(r)
	(if (all-pieces-playable r)
		(h-comp r)
		most-positive-fixnum)
)


; Height heuristic, for optimization problem
(defun h-height(state)
	(let ((m 0) h)
		(loop for p in (rect-pecas-f state) do
			(setf h (+ (piece-x p) (piece-w p) ))
			(if (> h m) 
				(setf m h)
			)
		)
		m
	)
)

(defun floodfill(m x y)
	(if (or (>= x (array-dimension m 1)) (>= y (array-dimension m 0)) (< y 0) (< x 0) )
		(return-from floodfill)
	)
	(when (= (aref m y x) 0)
		(setf (aref m y x) 1)
		(floodfill m (1+ x) y)
		(floodfill m (1- x) y)
		(floodfill m x (1+ y))
		(floodfill m x (1- y))
	)
)

;(m (create-matrix state))
(defun h-hole(state)
	(let ( (m (create-matrix state)) (count 0)) 
		(loop for x in (range (array-dimension m 1) ) do
			(floodfill m (1- (array-dimension m 0)) x )
		)
		(loop for y in (range (array-dimension m 0)) do
			(loop for x in (range (array-dimension m 1)) do
				(if (= (aref m y x) 0)
					(incf count))
			)
		)

		count
	)
)

(defun h-hole-height(state)
	(/ (h-comp state) (* 50 (h-height state) ))
)



(defun comp-state(h a b)
	(< (funcall h a) (funcall h b) )
)


(defun ILDS (state op h)
	(let ( (size (length (rect-pecas-i state) ) ) sol)
		(loop for disc in (range (1+ size) ) do
			(setf sol (LDS state op h disc))
			(if sol
				(return-from ILDS sol))
		)
	)
	NIL
)

(defun LDS (state op h rem-disc)
	(let ((succ (funcall op state)) 
		  (rem (length (rect-pecas-i state)) ) 
		   sol )

		(if (objectivo state)
			(return-from LDS state)
		)
		(if (null succ)
			(return-from LDS NIL)
		)
		
		(sort succ (lambda (a b) (comp-state h a b) ) )

		(when (> rem rem-disc)
			(setf sol (LDS (first succ) op h rem-disc))
			(if sol
				(return-from LDS sol))
		)

		(if (> rem-disc 0)
			(loop for child in (cdr succ) do
				(setf sol (LDS child op h (1- rem-disc)))
				(if sol
					(return-from LDS sol))
			)
		)

	)
)

(defun nbest(current new)
	(when (null current) 
		(return-from nbest new) )
	(if (null new) (return-from nbest new) )

	(if (< (h-height new) (h-height current) )
		new
		current
	)
)

(defun ILDS-opt (state op h)
	(let ( (size (length (rect-pecas-i state) ) ) (start (get-universal-time)) best )
		(loop for disc in (range (1+ size) ) do
			(setf best (LDS-opt state op h disc best start))
			(if (> (get-elapsed start) 30)
				(return-from ILDS-opt best)
			)
		)
		best
	)
)

; State, lambda operator, heuristic, remaining discrepancies, best solution found, start time
(defun LDS-opt (state op h rem-disc best start)
	(let ((succ (funcall op state)) 
		  (rem (length (rect-pecas-i state))) )

		(when (objectivo state)
			(setf best (nbest best state) )
			(return-from LDS-opt best)
		)

		(if (null succ)
			(return-from LDS-opt best)
		)

		(when best
			(if (< (h-height best) (h-height state) )
				(return-from LDS-opt best))

			(setf (rect-height state) (h-height best))
			(when (null (all-pieces-playable state))
				(setf (rect-height state) most-positive-fixnum)
				(return-from LDS-opt best)
			)
			(setf (rect-height state) most-positive-fixnum)
		)
		
		(sort succ (lambda (a b) (comp-state h a b) ) )

		(when (> rem rem-disc)
			(setf best (LDS-opt (first succ) op h rem-disc best start) )
		)

		(if (> rem-disc 0)
			(loop for child in (cdr succ) do
				(if (> (get-elapsed start) 300)
					(return-from LDS-opt best)
				)
				
				(setf best (LDS-opt child op h (1- rem-disc) best start))

			)
		)
		best
	)
)

(defun samp (state op)
	(let ((succ (funcall op state)) )
		(if (objectivo state)
			(return-from samp state)
		)
		(if (null succ)
			(return-from samp NIL)
		)

		(return-from samp (samp (nth (random (length succ)) succ) op ))

	)
)

(defun i-sampling-sat(state op)
	(let (sol)
		(loop do
			(setf sol (samp state op))

			(if sol
				(return-from i-sampling-sat sol))

		while T)
	)
)

(defun i-sampling-opt(state op)
	(let (sol best start)
		(setf start (get-universal-time))
		(loop do
			(setf sol (samp state op))
			(if (null best)
				(setf best sol)
				(if (> (h-height best) (h-height sol) )
					(setf best sol)
				)
			)

		while (< (get-elapsed start) 300) )

		best
	)
)

(setf 

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

;(setf ini (inicial (first p4b) (first (second p4b)) (second(second p4b)) ))
;(printState (ILDS ini #'operator #'complicated))


;(setf ini (inicial (first p4b) (first (second p4b)) most-positive-fixnum ))
;(time (printState (i-sampling-opt ini #'operator)))



(setf ini (inicial (first p10b) (first (second p10b)) most-positive-fixnum ))

;(time (printState (ILDS-opt ini #'operator #'h-height)))

;;; (time (procura (cria-problema (inicial (first p4b) (first (second p4b)) (second(second p4b))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'complicated) "a*" :espaco-em-arvore? T))
;;; (time (procura (cria-problema (inicial (first p1b) (first (second p1b)) (second(second p1b))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-comp2) "a*" :espaco-em-arvore? T))


(defun place-pieces (r p)
	(let ((s))
	(cond ((equal p "best.approach.satisfaction") (setf s (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r)) T) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-comp ) "a*" :espaco-em-arvore? T))))))
		  
		  ((equal p "a*.best.heuristic") (setf s (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r)) T) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-comp) "a*" :espaco-em-arvore? T))))))
		  ((equal p "a*.best.alternative.heuristic") (setf s (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r)) T) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-area) "a*" :espaco-em-arvore? T))))))
		  ((equal p "ILDS") (setf s (ILDS (inicial (first r) (first (second r)) (second(second r)) T) #'operator #'h-comp)))
		  ((equal p "iterative.sampling.satisfaction") (setf s (i-sampling-sat (inicial (first r) (first (second r)) (second(second r)) T) #'operator)))
		  ((equal p "profundidade") (setf s (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r)) T) (list #'operator) :objectivo? #'objectivo :estado= #'equal) "profundidade" :espaco-em-arvore? T))))))
		  
		  ((equal p "profundidade.iterativa") (setf s (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r)) T) (list #'operator) :objectivo? #'objectivo :estado= #'equal) "profundidade-iterativa" :espaco-em-arvore? T))))))
		  ((equal p "ida*.best.heuristic") (setf s (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r)) T) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'complicated2) "ida*" :espaco-em-arvore? T))))))
		  ((equal p "ida*.best.alternative.heuristic") (setf s (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r)) T) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-comp) "ida*" :espaco-em-arvore? T))))))
		  ((equal p "largura") (setf s (first (last (first (procura (cria-problema (inicial (first r) (first(second r)) (second(second r)) T) (list #'operator) :objectivo? #'objectivo :estado= #'equal) "largura" :espaco-em-arvore? T))))))
		  
		  
		  ((equal p "best.approach.optimization") (setf s (ILDS-opt (inicial (first r) (first (second r)) most-positive-fixnum) #'operator #'h-hole-height)))
		  ((equal p "iterative.sampling.optimization") (setf s (i-sampling-opt (inicial (first r) (first (second r)) most-positive-fixnum) #'operator)))
		  ((equal p "alternative.approach.optimization") (setf s (ILDS-opt (inicial (first r) (first (second r)) most-positive-fixnum) #'operator #'h-hole-height)))
		  (T (print "NO STRATEGY FOUND") ))
	
	;(if (null s) nil (rect-pecas-f s))
	;(print s)
	(if s
		(print (h-height s)))
	
))


(time (place-pieces p1c "alternative.approach.optimization"))
(time (place-pieces p20c "alternative.approach.optimization"))
(time (place-pieces p4c "alternative.approach.optimization"))
(time (place-pieces p40a "alternative.approach.optimization"))
(time (place-pieces p50a "alternative.approach.optimization"))

;(time (place-pieces p35 "a*.best.heuristic"))
;(time (place-pieces p35 "a*.best.alternative.heuristic"))
;(time (place-pieces p35 "iterative.sampling.satisfaction"))
;(time (place-pieces p40a "ILDS"))
;(time (place-pieces p35 "profundidade"))


;(time (place-pieces p4c "a*.best.alternative.heuristic"))

;(time (place-pieces p10a "a*.best.heuristic"))
;(time (place-pieces p100a "best.approach.optimization"))
;(time (place-pieces p100a "iterative.sampling.optimization"))
;(time (place-pieces p10c "a*.best.heuristic"))


