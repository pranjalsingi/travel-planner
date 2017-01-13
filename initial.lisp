;; Name - Pranjal Singi

(defun sample-test ()
; This is an example call to TravelPlannerAgent
   (TravelPlannerAgent "denver" "westpalm" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP))

; States are represented as a string giving the name of the city which one is at
;
; Nodes in the search tree will be represented by atoms 
;     A node for a state will be represented by turning the 
;     string "Node-" concatenated with a string giving the city 
;     (reresenting the state) into an atom.  
;     This can be done via the intern function.  Thus a node for the 
;     state that is the city denver will be gotten via
;     (intern(concatenate 'string "Node-" "denver"))
;     and will appear as |Node-denver| if printed.
; Nodes have the following properties:
;  state - The string that is the name of the city that is the 
;          current state of the node.  
;          For example, the node |Node-denver| will have as its state  
;          property the string "denver". This is the node's state, 
;          since it is the current location of the agent.
;  parent - The node that is the predecessor of the node on the best 
;           path that has been found so far from start city to the  
;           city represented by the node.
;  action - The action, such as ("baltimore" fly) that was used to 
;           get to the node, meaning fly to Baltimore
;  best-path-cost - The cost of the best known path from the initial 
;                   state to the node (this is g)
;  cost-to-goal-estimate - The estimate of the cost to a goal from 
;          the state represented by this  node (this is h)
;  least-cost-estimate - The overall estimate of the cost from the
;           initial state to goal going through this node (this is f)


;
; TravelPlannerAgent takes five problem-dependant arguments:
;
;     start-city - a string giving the name of the city from which to 
;                  start the search. 
;     goal-city -  a string giving the name of the city that one 
;                  wishes to reach
;     goal-test? -  a predicate that returns true for goal nodes 
;                   and false for non-goal nodes.
;     get-successors -  a function to compute successors of a state
;                   represented by a node.  The successors are each
;                   represented as (new-state means arc-cost) triples,
;                   such as ("miami" fly 2776)  
;     get-goal-estimate - a function which takes a string giving the
;                   name of a city and returns an estimate of the cost 
;                   of getting to the goal city

; TravelPlannerAgent returns a 2-tuple whose first element is an 
; optimal path from the start city to the goal city represented as  
; a list of actions that are performed to get from the start city to 
; the goal city and whose second element is the cost of this path.
;

 (defun TravelPlannerAgent
  (start-city  goal-city goal-test? get-successors get-goal-estimate) 
;create a node for start-city, and find the path to goal-city 
;   using Algorithm A*
  (defun search-graph (open-closed)
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; search-graph is the function that iterates through the open list.
  ;     It selects the front node on open list and tests whether 
  ;     its state is the goal.  If so, it gets the best path that has 
  ;     been found to this node, along with the path cost and returns 
  ;     it.  Otherwise it recursively calls search-graph with the new 
  ;     open and closed lists that result from expanding the graph 
  ;     with the successors of the selected node.
  ; returns a 2-element list, containing the sequence of actions
  ;     leading to the goal and the total cost of the path;
	;(break "entering search-graph")
       (cond((null (car open-closed)) nil)
          (t (let((selected-node (caar open-closed)))
                 (terpri)
         
                 (format t 
                    "The nodes, f-values, and actions on open list are ~A" 
                     (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (car open-closed)))
                 (terpri)
                 (format t 
                     "The nodes, f-values, and actions on closed list are ~A" 
                      (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (cadr open-closed)))
                 (terpri) (terpri)
                 (format t "Select a new node from open list")
                 (terpri) 
                (format t "The selected node is ~A" 
                          (caar open-closed))
                (terpri)
                (format t "Check if this node is the goal node")
                (terpri)
                (cond((funcall goal-test? selected-node goal-city)
                          (terpri)
                          (format t "This is the goal node")
                          (terpri)
                          (format t "Here is the list of actions and total path cost in the solution")
                          (terpri)
                          (get-path-and-total-cost selected-node))
                     (t (let ((successors (funcall get-successors
                                                   selected-node)))
                        (format t "This is NOT the goal node")
                        (terpri)
                        (format t "Its successors (and their arc costs) are ~A"
                                  successors)
                        (terpri)

                        (search-graph
                           (expand-graph 
                             successors
                             selected-node
                             (list (cdr (car open-closed))
                                   (cons selected-node 
                                         (cadr open-closed)))
                             get-successors
                             get-goal-estimate 
                             goal-city)))))))))
                         
; create a node for start-city and begin the search
  (search-graph 
   (list(list (create-node start-city nil nil 0 
                           get-goal-estimate goal-city))
   nil)))
      
 (defun expand-graph
   (succs parent-node open-closed succ-fn est-goal goal-city)
        ;; succs is the list of sucessors of parent-node
        ;; each element of succs is a tuple of the form 
        ;;    (new-state means arc-cost) triples such as 
        ;;    ("miami" fly 2776).
	;; expand-graph adds the list of successors of parent to 
        ;;    the graph and to open list.
	;; It must make sure that a successor has not already 
        ;;    been encountered (ie., is not already on open 
        ;;    or closed) and must check for updating the 
        ;;    shortest path if the state has been encountered 
        ;;    before
        ;; returns the resulting 2-tuple giving the open 
        ;;    and closed lists
	;(break "entering expand-graph")
   (cond ((null succs) open-closed)
	 (t 
;         process the next successor
           (let* ((state (caar succs))
                  (node-name 
                      (intern (concatenate 'string 
                                 "Node-" state)))
		  (arccost (caddar succs))
                  (action (list (caar succs) (cadar succs)))
		  (cost (+ (get parent-node 'best-path-cost)
			    arccost)))
              (format t "     The next successor is ~A" (car succs))
              (terpri)
              ;(break "in expand-graph")
              (cond ((and (not (state-on state (car open-closed)))
			  (not (state-on state (cadr open-closed))))
; this successor is not on open or closed list
                       (format t "this successor is not on open or closed list") 
                       (terpri)    
                       (expand-graph (cdr succs)
                                      parent-node
                                     (list (add-to-open 
                                           (create-node (caar succs) 
                                                     action
                                                     parent-node 
                                                     cost 
                                                     est-goal 
                                                     goal-city)
                                            (car open-closed))
                                         (cadr open-closed))
                                      succ-fn
                                      est-goal
                                      goal-city))
		    ((and (state-on state (car open-closed))
                          (< cost (get node-name 'best-path-cost)))
; this successor is already on open list and we have
;    found a better path to it
                     (format t "**** ON OPEN AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                   (update-node-open node-name
                                                      parent-node
                                                      succ-fn
                                                      cost
                                                      action
                                                      open-closed)
                                    succ-fn
                                    est-goal
                                    goal-city))
                     ((and (state-on state (cadr open-closed))
                           (< cost (get node-name 'best-path-cost)))
; this successor is already on closed list and we have
;    found a better path to it
                     (format t "*** ON CLOSED AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                    (update-node-closed node-name
                                                        parent-node
                                                        succ-fn
                                                        cost
                                                        action
                                                        open-closed)
                                    succ-fn
                                    est-goal
                                    goal-city))
		    (t 
; this successor is already on open or closed and the new path
;   to the node is not better than the existing path
                      (format t "this successor is on open or closed but path is not better")
                      (terpri)
                      (expand-graph (cdr succs)
				    parent-node
				    open-closed 
				    succ-fn
				    est-goal
                                    goal-city)))))))

(defun update-node-open 
  (n parent successor-fn cost-of-short-path action open-closed )
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; node n is on the open list.
  ; a new shortest path from the initial state to node n has 
  ;   been found.
  ; parent is the parent node of node n on this new path.
  ; action is the action that moved from parent to node n.  

  ; cost-of-short-path is the cost of this new path from the
  ;   initial state to node n and goes through parent. 
  ; successor-fn is the parameter giving the function for
  ;   computing successors 
  ; update the properties of node n and, if necessary, its position
  ;  on open list
  ; return the adjusted open-closed list

    ;(break "entering update-node-open")
	(setf (get n 'parent) parent)
	(setf (get n 'action) action)
	(setf (get n 'best-path-cost) cost-of-short-path)
	(setf (get n 'least-cost-estimate) (+ cost-of-short-path (get n 'cost-to-goal-estimate)))
	(list (adjust-open n (car open-closed)) (cdar open-closed))
)


(defun update-node-closed (n parent successor-fn cost-of-short-path 
                           action open-closed)
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; node n is on the closed list.
  ; a new shortest path from the initial state to node n has 
  ;   been found.
  ; parent is the parent node of node n on this new path.
  ; action is the action that moved from parent to node n.  
  ; cost-of-short-path is the cost of this new path from the
  ;   initial state to node n and goes through parent.  
  ; successor-fn is the parameter giving the function for
  ;   computing successors
  ; update the properties of node n and, if necessary, its
  ;   descendants on open and closed lists.
  ; return the adjusted open-closed list

	;(break "entering updated-node-closed")
	(setf (get n 'parent) parent)
	(setf (get n 'action) action)
	(setf (get n 'best-path-cost) cost-of-short-path)
	(setf (get n 'least-cost-estimate) (+ cost-of-short-path (get n 'cost-to-goal-estimate)))
	(let ((succ (funcall successor-fn n)))
		(defun iter (succ open-closed)
		    ;(break "entering iter")
			(cond ((null succ) open-closed)
				((state-on (caar succ) (car open-closed)) (iter (cdr succ)(update-node-open 
																	(intern (concatenate 'string "Node-" (caar succ))) 
																	n 
																	successor-fn 
																	(+ cost-of-short-path (caddar succ)) 
																	(list (caar succ) (cadar succ)) 
																	open-closed)))
				((state-on (caar succ) (cadr open-closed)) (iter (cdr succ) (update-node-closed 
																	(intern (concatenate 'string "Node-" (caar succ))) 
																	n 
																	successor-fn 
																	(+ cost-of-short-path (caddar succ)) 
																	(list (caar succ) (cadar succ))
																	open-closed)))
				(t (iter (cdr succ) open-closed))))
		(iter succ open-closed)
))
                   
(defun state-on (state lst)
; state is a city represented as a string such as "denver"
; lst is an open or closed list
; return true if a node on lst has this city as its state

	;(break "entering state-on")
	(cond ((null lst) nil)
		((equal (get (car lst) 'state) state) t)
		(t (state-on state (cdr lst)))))
       
(defun add-to-open (n open)
; n is a node and open is the open list
; add n to the open list in the correct position 
; return the new open list

	;(break "entering add-to-open")
	(cond ((null open) (list n))
		((< (get n 'least-cost-estimate) (get (car open) 'least-cost-estimate)) (cons n open))
		(t (cons (car open) (add-to-open n (cdr open))))))

(defun adjust-open (n open)
; n is a node and open is the open list
; make sure that n is in its proper position on open list, and if not
;   move it to the proper position
; the reason that n may not be in its proper position is that a better
;   path to it may have been found, thereby changing its f value
; return the new open list

	;(break "entering adjust-open")
	(add-to-open n (delete-node-open n open)))
	
(defun delete-node-open (n open)
    ;(break "entering delete-node-open")
	(cond ((equal (car open) n) (cdr open))
		(t (cons (car open) (delete-node-open n (cdr open))))))


(defun create-node 
  (city action parent cost-of-short-path est-goal goal-city)
  ; city is a string representing the name of a city.
  ;   Create a new node with this city as its state and
  ;   with the appropriate properties
  ; action is the action that moved from parent to city.  
  ; parent is the parent node.
  ; cost-of-short-path is the cost of the path from the
  ;   initial state to the state represented by this new
  ;   node and goes through parent.
  ; goal-city is a string representing the goal city
  ; est-goal is a parameter giving the function for estimating
  ;   the cost of getting to the goal from this new node 
  ; create a new node with the appropriate properties
  ; return the created node.
(let ((node (intern (concatenate 'string 
                                 "Node-" 
                                 city
                                 ))))
  (setf (get node 'state) city)
  (setf (get node  'parent) parent)
  (setf (get node 'action) action)
  (setf (get  node 'best-path-cost) cost-of-short-path)
  (setf (get node 'cost-to-goal-estimate) (funcall est-goal city goal-city)) 
  (setf (get  node `least-cost-estimate)
        (+ cost-of-short-path (get node 'cost-to-goal-estimate)))
  node))
    
(defun get-path-and-total-cost (node)
; node is a node in the graph
; return a list consisting of two elements: the path (in terms of 
;    a list of successive actions) that was taken to get to node   
;    and cost of that path

	;(break "entering get-path-and-total-cost")
	(list (get-path node) (get node 'best-path-cost))
)

(defun get-path (node)
    ;(break "entering get-path")
	(cond ((null (get node 'parent)) ())
		(t (append (get-path (get node 'parent)) (list (get node 'action))))))

(defun successors-TP (cnode)
;(break "entering successors-TP")
; cnode is a node 
;   return a list of the successors of cnode, with each successor 
;   given as
;   (city  means arc-cost) triples, such as ("baltimore" fly 2426)

	(let ((city (get cnode 'state)))
		(generate-succ city (list 'fly 'take-train 'take-bus)))
)

(defun generate-succ (city lst)
	;(break "entering generate-succ")
	(cond ((null lst) ())
		(t (append (generate-more-succ city (car lst)) (generate-succ city (cdr lst))))))
		
(defun generate-more-succ (city prop)
	;(break "entering generate-more-succ")
	(let ((lst-propcit (get (intern city) prop))
			(lst-citdist (get (intern city) 'distance)))
				(gen-more-succ lst-citdist lst-propcit prop)))
				
(defun gen-more-succ (lst-citdist lst-propcit prop)
    ;(break "entering gen-more-succ")
	(cond ((null lst-propcit) ())
		(t (append (last-succ-gen lst-citdist (car lst-propcit) prop) (gen-more-succ lst-citdist (cdr lst-propcit) prop)))))
		
(defun last-succ-gen (lst-citdist city prop)
    ;(break "entering last-succ-gen")
	(cond ((equal (caar lst-citdist) city) (list (list (caar lst-citdist) prop (cadar lst-citdist))))
		(t (last-succ-gen (cdr lst-citdist) city prop))))


(defun goal-test-TP? (node goal-city)
;(break "entering goal-test-TP?")
; node is a node and goal-city is a string giving the name of the 
;    goal city
; return True if the city for this node is goal-city

	(equal (get node 'state) goal-city))

(defun get-goal-estimate-TP (city goal-city)
; city and goal-city are both strings giving city names
; return an estimate of the cost of getting from city to goal-city

;(break "entering get-goal-estimate-TP")
	(let ((lst (get (intern city) 'distance)))
		(check-distance lst goal-city)))

(defun check-distance (lst goal-city)
    ;(break "entering check-distance")
	(cond ((null lst) 0)
	((equal (caar lst) goal-city) (cadar lst))
		(t (check-distance (cdr lst) goal-city))))
