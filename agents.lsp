;; agents.lsp (Trees representing Boolean conditions)
;; Evolutionary programming in LISP
;; educational materials inspired by Nils. J. Nilsson
;; March 16, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0

;; Notes
;; 1. The trees obtained through the crossover and mutation operations
;;    are getting bigger and bigger. (This is not necessarily bad.)
;; 2. Nilsson, in his book, uses a different representation of an agent 
;;    which seems to give the agents memory of past steps.

;;-----------------------------------------------------------------------------

;; Our and-or-trees may have atoms
;; or negated atoms as leaves, 
;; (not *e*) is considered a single node.

;; Nodes in our trees are numbered using the depth-first left-to-right
;; method, with root having number 1.

;;=============================================================================

;; count-nodes <tree>
;; Returns the number of nodes in <tree>
;; Could be re-implemented as tail-recursive.

(defun count-nodes (tree)
  (if
    (or (atom tree) (eq (first tree) 'not))
    1
    (+ 1 (count-nodes (second tree)) (count-nodes (third tree)))
  )
)

;;-----------------------------------------------------------------------------

(defun replace-subtree (tree node replacement)
  (cond 
    ((or (atom tree) (eq (first tree) 'not)) replacement)
    ((eq node 1) replacement
    )
    ((<= (1- node) (count-nodes (second tree))) 
      `(,(first tree) 
        ,(replace-subtree (second tree) (1- node) replacement)
        ,(third tree)
       )
    )
    (T 
      `(,(first tree) 
        ,(second tree) 
        ,(replace-subtree (third tree) 
                          (- node 1 (count-nodes (second tree)))
                          replacement
         )
       )
    )
  )
)

;;-----------------------------------------------------------------------------

(defun nth-subtree (n tree)
  (cond
    ((eq n 1) tree)
    ((<= (1- n) (count-nodes (second tree))) 
       (nth-subtree (1- n) (second tree))
    )
    (T (nth-subtree (- n 1 (count-nodes (second tree))) (third tree)))
  )
)

;;-----------------------------------------------------------------------------

;; This function returns a pair: 
;; node number and the subtree starting at that node.

(defun random-subtree (tree)
  (let
     ;; local variable
     ((node (1+ (random (count-nodes tree)))))
     ;; body
     `(,node ,(nth-subtree node tree))     
  )
)

;;=============================================================================

;; random-leaf
;; *e* means that the way to the east is free; similarly for *ne*, *w*, etc.
;; The GCL compiler requires that macros definitions are given before calls.
;; Note this macro is called in random-tree.

(defmacro random-leaf ()
  '(case (random 16)
     ( 0 '*e*)
     ( 1 '*se*)
     ( 2 '*s*)
     ( 3 '*sw*)
     ( 4 '*w*)
     ( 5 '*nw*)
     ( 6 '*n*)
     ( 7 '*ne*)
     ( 8 '(not *e*))
     ( 9 '(not *se*))
     (10 '(not *s*))
     (11 '(not *sw*))
     (12 '(not *w*))
     (13 '(not *nw*))
     (14 '(not *n*))
     (15 '(not *ne*))
   )
)

;;-----------------------------------------------------------------------------

;; Precondition: max-height is a positive integer
;; Postcondition: return a random tree of hight <= max-hight
;; built of and's, or's and leafs.
;; The distribution is uniform for max-height <= 2 and
;; not uniform for higher trees -- it prefers short trees/branches.

(defun random-tree (max-height)
  (case max-height
    (1 (random-leaf))
    (2 (let ;; all trees of height <=2 are generated with the same probability
         ;; local variables
         ((random (random 33)) ;; there are 16*33 trees of height <=2
          (new-height (1- max-height))
         )
         ;; body
         (cond
           ((= random 0) (random-leaf))
           ((<= random 16) ;; 16 = (33-1)/2
              `(and ,(random-tree new-height) ,(random-tree new-height)))
           (T `(or ,(random-tree new-height) ,(random-tree new-height)))
         )
       )
    )
    (otherwise
      (let 
         ;; local variables
         ((random (random 101)) ;; There are 16*1057 trees of height <=3
                                ;; We are using 101 instead of 1057
                                ;; thus giving preference to shorter trees.
          (new-height (1- max-height))
         )
         ;; body
         (cond
           ((= random 0) (random-leaf))
           ((<= random 50) ;; 50 = (101-1)/2
              `(and ,(random-tree new-height) ,(random-tree new-height)))
           (T `(or ,(random-tree new-height) ,(random-tree new-height)))
         )
       )
    )
  )
)

;;-----------------------------------------------------------------------------

;; This function replaces randomly selected subtree by a new random tree.
;; Notice that the resulting tree may exceed *agent-size*
(defun tree-mutation (tree mutation-size)
  (replace-subtree tree 
     (first (random-subtree tree)) 
     (random-tree mutation-size)
  )
)

;;-----------------------------------------------------------------------------

;; This function returns a pair of trees resulting
;; from a swap of randomly selected subtrees.

(defun tree-crossover (tree1 tree2)
  (let*
     ((sub1 (random-subtree tree1))
      (sub2 (random-subtree tree2))
      (new1 (replace-subtree tree1 (first sub1) (second sub2)))
      (new2 (replace-subtree tree2 (first sub2) (second sub1)))
     )
     `(,new1 ,new2)
  )
)

;;=============================================================================

;; An agent is represented by three Boolean expressions, called
;; main-condition, e-w-condition and s-n-condition.
;; The agent will move according to the following program
;;      (if main-condition
;;        (if e-w-condition (try-to-move-east) (try-to-move-west))
;;        (if s-n-condition (try-to-move-south) (try-to-move-north))
;;      )

(defun random-agent1 ()
  "(if c1 (if c2 #\e #\w) (if c3 #\s #\n))"
  `(if ,(random-tree *agent-size*)
     (if ,(random-tree *agent-size*) #\e #\w)
     (if ,(random-tree *agent-size*) #\s #\n)
   )
)

;;-----------------------------------------------------------------------------

(defmacro cadaddr (list)
  `(cadr (caddr ,list))
)

(defmacro cadadddr (list)
  `(cadr (cadddr ,list))
)

;;-----------------------------------------------------------------------------

;; select a random node and replace the corresponding subtree
;; by a random tree of height <= *agent-size*.
;; Notice that agent most likely will grow higher than *agent-size*.
(defun mutation1 (agent)
  (let*
    ((treeA (cadr  agent))
     (treeB (cadaddr agent))
     (treeC (cadadddr agent))
     (i (random 3))
     (new-tree
       (case i
         (0 (tree-mutation treeA *agent-size*))
         (1 (tree-mutation treeB *agent-size*))
         (2 (tree-mutation treeC *agent-size*))
       )
     )
    ) ;; body
    (case i
      (0 `(if ,new-tree (if ,treeB #\e #\w) (if ,treeC #\s #\n)))
      (1 `(if ,treeA (if ,new-tree #\e #\w) (if ,treeC #\s #\n)))
      (2 `(if ,treeA (if ,treeB #\e #\w) (if ,new-tree #\s #\n)))
    )
  )
)

;;-----------------------------------------------------------------------------

;; Returns a list with two agents.
(defun crossover1 (agent1 agent2)
  (let*
    ;; Local variables.
    ((tree1a (cadr  agent1))
     (tree1b (cadaddr agent1))
     (tree1c (cadadddr agent1))
     (tree2a (cadr  agent2))
     (tree2b (cadaddr agent2))
     (tree2c (cadadddr agent2))
     (i (random 3))
     (tree-pair
       (case i
         (0 (tree-crossover tree1a tree2a))
         (1 (tree-crossover tree1b tree2b))
         (2 (tree-crossover tree1c tree2c))
       )
     )
     (new-tree1 (first  tree-pair))
     (new-tree2 (second tree-pair))
    ) 
    ;; Body.
    (case i
      (0 
        `(
       (if ,new-tree1 (if ,tree1B #\e #\w) (if ,tree1C #\s #\n))
       (if ,new-tree2 (if ,tree2B #\e #\w) (if ,tree2C #\s #\n))
         )
      )
      (1 
        `(
       (if ,tree1A (if ,new-tree1 #\e #\w) (if ,tree1C #\s #\n))
       (if ,tree2A (if ,new-tree2 #\e #\w) (if ,tree2C #\s #\n))
         )
      )
      (2 
        `(
       (if ,tree1A (if ,tree1B #\e #\w) (if ,new-tree1 #\s #\n))
       (if ,tree2A (if ,tree2B #\e #\w) (if ,new-tree2 #\s #\n))
         )
      )
    )
  )
)

;;=============================================================================

(defun random-agent2 ()
  "(cond (c1 #\e) (c2 #\s) (c3 #\w) (t #\n))"
  `(cond 
     (,(random-tree *agent-size*) #\e)
     (,(random-tree *agent-size*) #\s)
     (,(random-tree *agent-size*) #\w)
     (T #\n)
   )
)

;;-----------------------------------------------------------------------------

;; select a random node and replace the corresponding subtree
;; by a random tree of height <= *agent-size*.
;; Notice that agent most likely will grow higher than *agent-size*.
(defun mutation2 (agent)
  (let*
    ((treeA (caadr  agent))
     (treeB (caaddr agent))
     (treeC (car (cadddr agent)))
     (i (random 3))
     (new-tree
       (case i
         (0 (tree-mutation treeA *agent-size*))
         (1 (tree-mutation treeB *agent-size*))
         (2 (tree-mutation treeC *agent-size*))
       )
     )
    ) ;; body
    (case i
      (0 
       `(cond (,new-tree #\e) (,treeB #\s) (,treeC #\w) (t #\n))
      )
      (1 
       `(cond (,treeA #\e) (,new-tree #\s) (,treeC #\w) (t #\n))
      )
      (2 
       `(cond (,treeA #\e) (,treeB #\s) (,new-tree #\w) (t #\n))
      )
    )
  )
)

;;-----------------------------------------------------------------------------

;; Returns a list with two agents.
(defun crossover2 (agent1 agent2)
  (let*
    ;; Local variables.
    ((tree1a (caadr  agent1))
     (tree1b (caaddr agent1))
     (tree1c (car (cadddr agent1)))
     (tree2a (caadr  agent2))
     (tree2b (caaddr agent2))
     (tree2c (car (cadddr agent2)))
     (i (random 3))
     (tree-pair
       (case i
         (0 (tree-crossover tree1a tree2a))
         (1 (tree-crossover tree1b tree2b))
         (2 (tree-crossover tree1c tree2c))
       )
     )
     (new-tree1 (first  tree-pair))
     (new-tree2 (second tree-pair))
    ) 
    ;; Body.
    (case i
      (0 
       `(
      (cond (,new-tree1 #\e) (,tree1B #\s) (,tree1C #\w) (t #\n))
      (cond (,new-tree2 #\e) (,tree2B #\s) (,tree2C #\w) (t #\n))
        )
      )
      (1 
       `(
      (cond (,tree1A #\e) (,new-tree1 #\s) (,tree1C #\w) (t #\n))
      (cond (,tree2A #\e) (,new-tree2 #\s) (,tree2C #\w) (t #\n))
        )
      )
      (2
       `( 
      (cond (,tree1A #\e) (,tree1B #\s) (,new-tree1 #\w) (t #\n))
      (cond (,tree2A #\e) (,tree2B #\s) (,new-tree2 #\w) (t #\n))
        )
      )
    )
  )
)

;;=============================================================================
