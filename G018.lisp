; grupo 18 Daniel Brandao 76245 Miguel Duarte 85016

(in-package :user)

;;;;;;;;;;;;;;;;;;;PROVIDED;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct piece
 width
 height
 position
 orientation) 
 
(defun make-pos (h v) ;; h-horizontal v-vertical
 (list h v))
(defun pos-h (pos)
 (first pos))
(defun pos-v (pos)
 (second pos))
(defun pos=? (p1 p2)
 (equal p1 p2)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;LIST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
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
	(let ((area 0) (pos (list (make-pos 0 0))))
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
	(print (- (* 2 (* (rect-width r) (rect-height r))) comp))
	(print r)
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
			

	
;;; (load "procura.lisp")
;;; (load "G018.lisp")
;;; (load "PP-examples.lisp") --> capaz de dar warning visto que a estrutura piece esta redifinida	
;;; (time (procura (cria-problema (inicial (first p1a) (first (second p1a)) (second(second p1a))) (list #'operator) :objectivo? #'objectivo :estado= #'equal) "profundidade" :espaco-em-arvore? T))
;;; (time (procura (cria-problema (inicial (first p1b) (first (second p1b)) (second(second p1b))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-comp) "a*" :espaco-em-arvore? T))
;;; (time (procura (cria-problema (inicial (first p1b) (first (second p1b)) (second(second p1b))) (list #'operator) :objectivo? #'objectivo :estado= #'equal :heuristica #'h-area) "a*" :espaco-em-arvore? T))