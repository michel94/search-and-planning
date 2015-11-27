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
;;;;;;;;;;;;;;;;;;;;;;;;;;;HASH;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (defun tabela-valor (tabela chave)
  (multiple-value-bind (valor)
      (gethash chave tabela)
    valor))
 
 
 (defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table 
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))
;;;;;;;;;;;;;;;;;;;;;SATISFACTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct rect pecas-i posicoes pecas-f width height rekt)

;estado inicial
(defun inicial (pecas w h)
	(make-rect :pecas-i pecas
			:pecas-f (list)
			:posicoes (list (make-pos 0 0))
			:width w
			:height h
			:rekt (make-hash-table :test 'equal)))

;e objectivo se todos as pecas tiverem atribuidas			
(defun objectivo (r)
	(null (rect-pecas-i r)))

(defun encaixa-peca-? (r p pos o)
	(let (w h)
		(cond ((equal o 'H) (setf w (piece-width p)) (setf h (piece-height p)))
			((equal o 'V) (setf w (piece-height p)) (setf h (piece-width p))))
			
		(if (> (+ w (pos-h pos)) (rect-width r)) (return-from encaixa-peca-? nil))
		(if (> (+ h (pos-v pos)) (rect-height r)) (return-from encaixa-peca-? nil))
		(dotimes (x w T)
			(dotimes (y h)
				(if (not (null (tabela-valor (rect-rekt r) (list (+ x (pos-h pos)) (+ y (pos-v pos)))))) 
					(return-from encaixa-peca-? nil))))))

(defun copy-except (l p)
    (let ((newl (copy-list l)))
	(setf newl (delete-first-eq-from-list p newl))))
	

	
(defun agregate (l1 l2)
	(cond ((null l1) l2)
		;  ((equal 1 (length l1)) (list (first l1) l2))
		  (T (cons (first l1) (agregate (rest l1) l2)))))

	
(defun encaixa (r p pos o)
	(let (w h
	(newhash (copy-hash-table (rect-rekt r)))
	(newpos (copy-except (rect-posicoes r) pos))
	(newpiece (copy-piece p)))
		(cond ((equal o 'H) (setf w (piece-width p)) (setf h (piece-height p)))
			((equal o 'V) (setf w (piece-height p)) (setf h (piece-width p))))
	
	(dotimes (x w T)
			(dotimes (y h)
				(setf (gethash (list (+ x (pos-h pos)) (+ y (pos-v pos))) newhash) T))) 
	(setf (piece-position newpiece) pos)
	(setf (piece-orientation newpiece) o)
	(if (> (rect-width r) (+ (pos-h pos) w)) (setf newpos (agregate newpos (list (list (+ (pos-h pos) w) (pos-v pos))))))
	(if (> (rect-height r) (+ (pos-v pos) h)) (setf newpos (agregate newpos (list (list (pos-h pos) (+ (pos-v pos) h))))))
	(make-rect :pecas-i  (copy-except (rect-pecas-i r) p)
		:pecas-f (append (rect-pecas-f r) (list newpiece))
		:posicoes newpos
		:width (rect-width r)
		:height (rect-height r)
		:rekt newhash)))
		

					
(defun operator (r)
	(let (rs
			(pecas-i (rect-pecas-i r))
			(pecas-f (rect-pecas-f r))
			(posicoes (rect-posicoes r))
			(width (rect-width r))
			(height (rect-height r))
			tabuleiro-novo
			posicoes-novo)
		(dolist (p pecas-i rs)
			(dolist (pos posicoes)
				(dolist (o '(H V))
					(cond ((encaixa-peca-? r p pos o) (push (encaixa r p pos o) rs))))))
		
		(values rs)))


(defun resolve-problema (p l w h)
	(print l)
	(print w)
	(print h)
	(print p)
	(let ((s 
	(first (procura
		(cria-problema
			(inicial l w h)
			(list #'operator)
			:objectivo? #'objectivo
			:estado= #'equal)
		p
		:espaco-em-arvore? T) )))
	(if (null s) s s)))
