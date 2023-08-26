; ******************
;  Maze
; ******************

; VARIABLES
(defparameter *OPEN* '(nil)) ;; Search border
(defparameter *CLOSED* '(nil));; Memory
(defparameter *nNodes* 0) ;; Created nodes
(defparameter *SOLUTION* '(nil));; Store the solution nodes
(defvar *MAZE* (make-array '(100 100) :element-type 'list :initial-element nil ))
(defvar *START* '())
(defvar *GOAL* '())
(defvar *ROWS* 0)
(defvar *COLUMNS* 0)
(defun NextNode()(setq *nNodes* (+ *nNodes* 1)))
(defun read-datafile(path)
	(let ()
		(with-open-file(stream path)
			(setq *ROWS* (read stream nil nil))
			(setq *COLUMNS* (read stream nil nil))
			(setf *START* (list (read stream nil nil) (read stream nil nil)) )
			(setf *GOAL* (list (read stream nil nil) (read stream nil nil)) )
			(setf *MAZE* (adjust-array *MAZE* (list *ROWS* *COLUMNS*)))
			(read-line stream nil nil)
			(loop for row from 0 to (- *ROWS* 1) do
				(loop for column from 0 to (- *COLUMNS* 1) do
					(setf (aref *MAZE* row column) 
							(read stream nil nil) )
					)
				)
			)
	))


(defun Initialize()(setf (first *OPEN*) nil) 
				   (setf (first *CLOSED*) nil) 
				   (setq *nNodes* 0) 
				   (setf (first *SOLUTION*) nil)
  				   (READ-DATAFILE FILE)
;				   (READ-DATAFILE "./Maze3x3.maze")
				   )


;state: (1 2 3 4)
;position: 1 | 2 | 3 | 4
(defun blocked?(state position)(cond ((null state) nil)
										((= (first state) position) T)
										(T (blocked? (rest state) position))
	))

(defun MoveUp(state)(NextNode)
	(if (AND (not (= (nth 0 (nth 1 state)) 0)) 
				(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state))) 1) );;up
				(not (blocked? (aref *MAZE* (- (nth 0 (nth 1 state)) 1) (nth 1 (nth 1 state))) 3) );;down
		)
		(list (nth 4 state) (list (- (nth 0 (nth 1 state)) 1) (nth 1 (nth 1 state)) ) (+ (nth 2 state) 1) 'up *nNodes*)
		nil
	)
)

(defun MoveUpRight(state)(NextNode)
(if (AND (not (= (nth 0 (nth 1 state)) 0))
		 (not (= (nth 1 (nth 1 state)) (- *COLUMNS* 1) ))
	 (OR 
	 	(AND 
			(not (blocked? (aref *MAZE* (- (nth 0 (nth 1 state)) 1) (+ (nth 1 (nth 1 state)) 1) ) 3) );;down
			(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state)) ) 2) )
		)
		(AND
	    	(not (blocked? (aref *MAZE* (- (nth 0 (nth 1 state)) 1) (+ (nth 1 (nth 1 state)) 1) ) 4) );;left
	    	(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state)) ) 1) )
	    )
	  )
	)
		(list (nth 4 state) (list (- (nth 0 (nth 1 state)) 1) (+ (nth 1 (nth 1 state)) 1) ) (+ (nth 2 state) 1) 'up-right *nNodes*)
		nil
	)
)

(defun MoveRight(state)(NextNode)
	(if (AND (not (= (nth 1 (nth 1 state)) (- *COLUMNS* 1) ))
				(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (+ (nth 1 (nth 1 state)) 1)) 4) );;right
		)
		(list (nth 4 state) (list (nth 0 (nth 1 state)) (+ (nth 1 (nth 1 state)) 1) ) (+ (nth 2 state) 1) 'right *nNodes*)
		nil
	)
)

(defun MoveDownRight(state)(NextNode)
(if (AND (not (= (nth 0 (nth 1 state)) (- *ROWS* 1)))
		 (not (= (nth 1 (nth 1 state)) (- *COLUMNS* 1)))
		 (OR 
		 	(AND
			(not (blocked? (aref *MAZE* (+ (nth 0 (nth 1 state)) 1) (+ (nth 1 (nth 1 state)) 1) ) 1) )
			(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state)) ) 2) )
			)
			(AND
			(not (blocked? (aref *MAZE* (+ (nth 0 (nth 1 state)) 1) (+ (nth 1 (nth 1 state)) 1) ) 4) )
			(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state)) ) 3) )
			)
		 )
	)
		(list (nth 4 state) (list (+ (nth 0 (nth 1 state)) 1) (+ (nth 1 (nth 1 state)) 1) ) (+ (nth 2 state) 1) 'down-right *nNodes*)
		nil
	)
)

(defun MoveDown(state)(NextNode)
	(if (AND (not (= (nth 0 (nth 1 state)) (- *ROWS* 1) )) 
				(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state))) 3) );;up
		)
		(list (nth 4 state) (list (+ (nth 0 (nth 1 state)) 1) (nth 1 (nth 1 state)) ) (+ (nth 2 state) 1) 'down *nNodes*)
		nil
	)
)

(defun MoveDownLeft(state)(NextNode)
	(if (AND (not (= (nth 0 (nth 1 state)) (- *ROWS* 1) ))
			 (not (= (nth 1 (nth 1 state)) 0))
			 (OR 
			 	(AND
				(not (blocked? (aref *MAZE* (+ (nth 0 (nth 1 state)) 1) (- (nth 1 (nth 1 state)) 1) ) 1) )
				(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state)) ) 4) )
				)
				(AND
				(not (blocked? (aref *MAZE* (+ (nth 0 (nth 1 state)) 1) (- (nth 1 (nth 1 state)) 1) ) 2) )
				(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state)) ) 3) )
				)
			 )
		)
		(list (nth 4 state) (list (+ (nth 0 (nth 1 state)) 1) (- (nth 1 (nth 1 state)) 1) ) (+ (nth 2 state) 1) 'down-left *nNodes*)
		nil
	)
)

(defun MoveLeft(state)(NextNode)
	(if (AND (not (= (nth 1 (nth 1 state)) 0))
				(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (- (nth 1 (nth 1 state)) 1)) 2) );;der
		)
		(list (nth 4 state) (list (nth 0 (nth 1 state)) (- (nth 1 (nth 1 state)) 1) ) (+ (nth 2 state) 1) 'left *nNodes*)
		nil
	)
)

(defun MoveUpLeft(state)(NextNode)
	(if (AND (not (= (nth 0 (nth 1 state)) 0))
			 (not (= (nth 1 (nth 1 state)) 0))
		 (OR 
		  (AND 
			(not (blocked? (aref *MAZE* (- (nth 0 (nth 1 state)) 1) (- (nth 1 (nth 1 state)) 1) ) 2) );;down
			(not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state)) ) 1) )
			)
		  (AND
		    (not (blocked? (aref *MAZE* (- (nth 0 (nth 1 state)) 1) (- (nth 1 (nth 1 state)) 1) ) 3) );;izq
		    (not (blocked? (aref *MAZE* (nth 0 (nth 1 state)) (nth 1 (nth 1 state)) ) 4) )
		    )
		  )
		)
		(list (nth 4 state) (list (- (nth 0 (nth 1 state)) 1) (- (nth 1 (nth 1 state)) 1) ) (+ (nth 2 state) 1) 'up-left *nNodes*)
		nil
	)
)

(defun CreateState(fatherId currentPosition depth operator)(NextNode)(list fatherId currentPosition depth operator *nNodes*))
(defun InsertNode(node *OPEN* method)
					(cond ( (and (equal method 'depthFirstSearch) ) 
							(setf (first *OPEN*) (append (first *OPEN*) (list node) )) )
						  ( (and (or (equal method 'breadthFirstSearch) (equal method 'star) (equal method 'bestfirst)) ) 
						  	(setf (first *OPEN*) (append  (first *OPEN*) (list node) )) )
))
(defun ExtractNode(*OPEN* method)
					(cond ( (equal method 'depthFirstSearch) (first (first *OPEN*)) )
						  ( (and (null (last (first *OPEN*))) (or (equal method 'breadthFirstSearch) 
						  										  (equal method 'star)
						  										  (equal method 'bestfirst))) (first (first *OPEN*)) )
						  ( (or (equal method 'breadthFirstSearch) (equal method 'star) (equal method 'bestfirst)) (first (last (first *OPEN*))) ); out<-OOO<-in
))

(defun Update (node *OPEN* *CLOSED*)
					(cond ((null node) nil)
						  (t (setf (first *CLOSED*) (append (list node) (first *CLOSED*) ))
							(setf (first *OPEN*) (remove node (first *OPEN*)))
											) ))

;; Returns T if the state is in the list of nodes
;;other way returns nil
(defun remember?(state nodelist)(cond ( (null nodelist) nil)
										( (equal (second state) (second (first nodelist))) T)
									(T (remember? state (rest nodelist)))
))

;; Filter from the list of states those that were already analyzed
(defun Filter(statelist nodelist)(cond ((null statelist) nil)
													((null (first nodelist)) statelist)
													((remember? (first statelist) (first nodelist))
															(Filter (rest statelist) nodelist))
													(T (cons (first statelist)
															(Filter (rest statelist) nodelist)))
))


(defun distance(node state);(if (or (equal (nth 3 node)'up)
	;								 (equal (nth 3 node)'right)
	;								 (equal (nth 3 node)'down)
	;								 (equal (nth 3 node)'left))
	;(+ (abs (+ (- (nth 0 (nth 1 node)) (nth 0 state)  ) 
	;						(-  (nth 1 (nth 1 node)) (nth 1 state) ) )) 2)
	(abs (+ (- (nth 0 (nth 1 node)) (nth 0 state)  ) 
							(-  (nth 1 (nth 1 node)) (nth 1 state) ) ))
	);)

(defun aptitude(node)(distance node *GOAL*))
(defun cost(node)(nth 2 node))

(defun relevance(node)
	(if (not (null node)) (list (nth 0 node) 
							  (nth 1 node) 
							  (nth 2 node) 
							  (nth 3 node) 
							  (nth 4 node)
							  (+ (cost node) 
							  	 (aptitude node)) 
							  	 ;(distance node (nth 1 father)) )
							  	  ) nil ))
(defun relevanceBest(node)
	(if (not (null node)) (list (nth 0 node) 
				  (nth 1 node) 
				  (nth 2 node) 
				  (nth 3 node) 
				  (nth 4 node)
				  (aptitude node) 
			  	  ) nil ))

(defun Expand(node method)
	(cond 
			((equal method 'depthFirstSearch) 
				(list 
					(MoveUp node)
					(MoveUpRight node)
					(MoveRight node)
					(MoveDownRight node)
					(MoveDown node)
					(MoveDownLeft node)
					(MoveLeft node)
					(MoveUpLeft node)
				))
			((equal method 'breadthFirstSearch) 
				(list 
					(MoveUpLeft node)
					(MoveLeft node)
					(MoveDownLeft node)
					(MoveDown node)
					(MoveDownRight node)
					(MoveRight node)
					(MoveUpRight node)
					(MoveUp node)
				  ))

;(sort a #'< :key #'(lambda(x)(second x)))
			((equal method 'star)
				(sort
				(remove nil 
					(list 
						(relevance (MoveUp node))
						(relevance (MoveUpRight node))
						(relevance (MoveRight node))
						(relevance (MoveDownRight node))
						(relevance (MoveDown node))
						(relevance (MoveDownLeft node))
						(relevance (MoveLeft node))
						(relevance (MoveUpLeft node))
					)
				)
				 #'> :key #'(lambda(x)(nth 5 x)) )
				)
			((equal method 'bestfirst)
			(sort
			(remove nil 
				(list 
					(relevanceBest (MoveUp node))
					(relevanceBest (MoveUpRight node))
					(relevanceBest (MoveRight node))
					(relevanceBest (MoveDownRight node))
					(relevanceBest (MoveDown node))
					(relevanceBest (MoveDownLeft node))
					(relevanceBest (MoveLeft node))
					(relevanceBest (MoveUpLeft node))
				)
			)
			 #'> :key #'(lambda(x)(nth 5 x)) )
			)
			  ))



;; Returns the father's id of a give state
;; if not found, returns nil
(defun rememberID?(node nodelist)(cond ( (null nodelist) nil)
										( (equal (nth 0 node) (nth 4 (first nodelist))) (first nodelist))
										(T (rememberID? node (rest nodelist)))
))

;; Adds the node to the solution list
(defun addSolutionNode(node)(setf (first *SOLUTION*) (append (first *SOLUTION*) (list (nth 3 node)))))


;; Tracks the path of a node until the root, and prints one by one
(defun TrackSolution(node *CLOSED*)(cond ((null node) nil)
											(T ;;(print (nth 3 node))
												(addSolutionNode node) 
												(TrackSolution (rememberID? node (first *CLOSED*)) *CLOSED*))
	))

;;Prints a give list (solution?)
(defun printSolution(solution?)
						(cond ((null solution?) nil) 
				 			  (T (print (first solution?)) (printSolution (rest solution?)))
				 		))


(defun Maze(method)
	(let ((node NIL) (successors '()))
		(Initialize)
		(InsertNode (CreateState 0 *START* 0 nil) *OPEN* method)
		;(print *OPEN*)
	
		(loop until (null (first *OPEN*)) do
		;(loop for i from 0 to 30 do
			(setq node (ExtractNode *OPEN* method))
			;(print node)

		(cond 	
			((equal (nth 1 node) *GOAL*)
			(print '***********SOLUTION*********)
			(TrackSolution node *CLOSED*)
			(printSolution (reverse (butlast (first *SOLUTION*))))
			(print (first *SOLUTION*))
			(setf (first *OPEN*) nil)
			 )

		  	(t (setq successors (remove nil (Expand node method)))
		  		;(print "successors")
		  		;(print successors)
				(Update node *OPEN* *CLOSED*)
				;(print "Updated")
				;(print *OPEN*)
		  		;(print *CLOSED*)
				(setq successors (Filter successors *CLOSED*))
				;(print "Filtered")
				;(print successors)
				(setq successors (remove nil successors))
				(dolist (n successors)
				(InsertNode n *OPEN* method))
				(if (equal method 'star)(setf (first *OPEN*) (sort (first *OPEN*) #'> :key #'(lambda(x)(nth 5 x)))))
				;(print "OPEN CLOSED")
		  		;(print successors)
		  		;(print *OPEN*)
		  		;(print *CLOSED*)
			);t-end
		);cond-end

		);loop-end


		(print '**************STATISTICS**************)
     	(print "Created nodes")
		(print *nNodes*)
		(print "Analyzed nodes")
		(print (length (first *CLOSED*)))
		(print "Pending nodes")
		(print (- *nNodes* (length (first *CLOSED*)) ))

        (return-from Maze nil)
	)
)

; SET THE FILE
(defvar FILE "./Maze10x10.maze")

; UNCOMMENT TO TEST 
(Maze 'star)
; (Maze 'depthFirstSearch)
; (Maze 'breadthFirstSearch)
; (Maze 'bestfirst)