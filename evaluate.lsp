;; evaluate.lsp
;; Evolutionary programming in LISP 
;; educational materials inspired by Nils. J. Nilsson
;; March 20, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0

;;=============================================================================

;; Evaluating fitness of an agent and displaying its movements.

;; Special variables 
;; used for efficiency -- do not copy parameters of frequently used functions.

(defvar *row*)      ;; Current position of the current agent in *world*:
(defvar *column*)   ;; 0..*max-rows*-1, 0..*max-columns*-1.

(defvar *e* ) ;; True if the place to the east of current position is free
(defvar *se*) ;; otherwise false,
(defvar *s* ) ;; etc.
(defvar *sw*)
(defvar *w* )
(defvar *nw*)
(defvar *n* )
(defvar *ne*)

(defvar *came-from* nil) ;; One of #\e, #\s, #\w, #\n where agent came from.

(defvar *score-s*) ;; Count of steps which ended at the wall. (s is for steps).

(defvar *normalization-factor-s*) ;; For evaluate-s
(defvar *normalization-factor-c*) ;; For evaluate-c

;;-----------------------------------------------------------------------------

(defun set-world-related-parameters ()
  (setq *world* (prepare-world *grid-world-file*))
  (setq *max-rows* (array-dimension *world* 0))
  (setq *max-columns* (array-dimension *world* 1))
  (setq *at-the-wall*
    (count-at-the-wall-places *world* *max-rows* *max-columns*)
  )
  (setq *normalization-factor-s* (/ 100 *tours* *tour-length*))
  (setq *normalization-factor-c*
    (/ 100 *tours* (min *tour-length* *at-the-wall*))
  )
)

;;-----------------------------------------------------------------------------

(defmacro reset-sensors ()
 '(setq *e* T *se* T *s* T *sw* T *w* T *nw* T *n* T *ne* T)
)

;;-----------------------------------------------------------------------------

;; Update sensory data: *e*, *se*, ... .
(defmacro read-sensors ()
 '(progn
    (setq *e*  (aref *world* *row*      (1+ *column*)))
    (setq *se* (aref *world* (1+ *row*) (1+ *column*)))
    (setq *s*  (aref *world* (1+ *row*) *column*)     )
    (setq *sw* (aref *world* (1+ *row*) (1- *column*)))
    (setq *w*  (aref *world* *row*      (1- *column*)))
    (setq *nw* (aref *world* (1- *row*) (1- *column*)))
    (setq *n*  (aref *world* (1- *row*) *column*)     )
    (setq *ne* (aref *world* (1- *row*) (1+ *column*)))   
  )
)

;;-----------------------------------------------------------------------------

;; The following code defines how fitness is evaluated.
;; Tries to move, updates sensory data, *came-from*, *score-s*,
;; if the new position is at the wall, marks it by a +
;; (these +'s will be counted by evaluate-c).
;; If the agent pushes against a wall, 
;; no movement takes place and score-s stays the same.
;; If the agent's new position is far from the wall, score-s stays the same.
;; If the agent goes where it came from, score-s stays the same.
;; Otherwise score-s is incremented by 1.
;; If the move was not successful, nil is returned, otherwise a true value.

(defmacro east ()
 '(when *e* ;; if the way to the east is free ...
    (incf *column*) ;; Set new position.
    (cond
      ((characterp (aref *world* *row* *column*)) ;; If at the wall ...
        (setq *sw* *s*) ;; *sw* is now what *s* used to be ...
        (setq *w*  T)
        (setq *nw* *n*)
        (setq *s*  *se*)
        (setq *n*  *ne*)
        (setq *ne* (aref *world* (1- *row*) (1+ *column*)))   
        (setq *e*  (aref *world* *row*      (1+ *column*)))
        (setq *se* (aref *world* (1+ *row*) (1+ *column*)))
        (setf (aref *world* *row* *column*) #\+) ;; Mark cell being visited.
        (unless (eq *came-from* #\e) (incf *score-s*)) ;; Increment *score-s*
      )
      (T ;; If far from the wall ...
        (setq *e* T *se* T *s* T *sw* T *w* T *nw* T *n* T *ne* T)
      )
    )
    (setq *came-from* #\w)
  )
)

(defmacro south ()
 '(when *s* ;; if the way to the south is free ...
    (incf *row*)
    (cond
      ((characterp (aref *world* *row* *column*))
        (setq *nw* *w*)
        (setq *n*  T)
        (setq *ne* *e*)
        (setq *w*  *sw*)
        (setq *e*  *se*)
        (setq *se* (aref *world* (1+ *row*) (1+ *column*)))
        (setq *s*  (aref *world* (1+ *row*) *column*)     )
        (setq *sw* (aref *world* (1+ *row*) (1- *column*)))
        (setf (aref *world* *row* *column*) #\+)
        (unless (eq *came-from* #\s) (incf *score-s*))
      )
      (T
        (setq *e* T *se* T *s* T *sw* T *w* T *nw* T *n* T *ne* T)
      )
    )      
    (setq *came-from* #\n)
  )
)

(defmacro west ()
 '(when *w* ;; if the way to the west is free ...
    (decf *column*) ;; decrement by 1
    (cond 
      ((characterp (aref *world* *row* *column*))
        (setq *ne* *n*)
        (setq *e*  T)
        (setq *se* *s*)
        (setq *s*  *sw*)
        (setq *n*  *nw*)
        (setq *sw* (aref *world* (1+ *row*) (1- *column*)))
        (setq *w*  (aref *world* *row*      (1- *column*)))
        (setq *nw* (aref *world* (1- *row*) (1- *column*)))
        (setf (aref *world* *row* *column*) #\+)
        (unless (eq *came-from* #\w) (incf *score-s*))
      )
      (T
        (setq *e* T *se* T *s* T *sw* T *w* T *nw* T *n* T *ne* T)
      )
    )      
    (setq *came-from* #\e)
  )
)

(defmacro north ()
 '(when *n* ;; if the way to the north is free ...
    (decf *row*)
    (cond 
      ((characterp (aref *world* *row* *column*))
        (setq *se* *e*)
        (setq *s*  T)
        (setq *sw* *w*)
        (setq *e*  *ne*)
        (setq *w*  *nw*)
        (setq *nw* (aref *world* (1- *row*) (1- *column*)))
        (setq *n*  (aref *world* (1- *row*) *column*)     )
        (setq *ne* (aref *world* (1- *row*) (1+ *column*)))   
        (setf (aref *world* *row* *column*) #\+)
        (unless (eq *came-from* #\n) (incf *score-s*))
      )
      (T
        (setq *e* T *se* T *s* T *sw* T *w* T *nw* T *n* T *ne* T)
      )
    )      
    (setq *came-from* #\s)
  )
)

;;-----------------------------------------------------------------------------

(defun evaluate-c (agent)
  "count different visited Cells at the wall." ;; Documentation.
  (setq *score-s* 0) ;; the value does not matter in evaluate-c
  (let 
    ((score-c 0)) ;; Count of different visited cells at the wall. 
    ;; Body
    (dotimes (i *tours* nil)
      ;; Set random position
      (loop 
        (setq *row*    (random *max-rows*))
        (setq *column* (random *max-columns*))
        (when (characterp (aref *world* *row* *column*)) (return))
      )
      (read-sensors)
      ;; Make one tour
      (dotimes (i *tour-length* nil) 
        (case (eval agent)
          (#\e (east))
          (#\s (south))
          (#\w (west))
          (#\n (north))
        )
      )
      (incf 
        score-c (count-erase-visited-places *world* *max-rows* *max-columns*)
      )
    )
    (round (* score-c *normalization-factor-c*))  
  )
)

;;-----------------------------------------------------------------------------

(defun evaluate-s (agent)
  "count steps which ended at the wall." ;; Documentation.
  (setq *score-s* 0)
  (dotimes (i *tours* nil)
    ;; Set random position
    (loop 
      (setq *row*    (random *max-rows*))
      (setq *column* (random *max-columns*))
      (when (characterp (aref *world* *row* *column*)) (return))
    )
    (read-sensors)
    (setq *came-from* nil) 
    (dotimes (i *tour-length* nil)
      (case (eval agent)
        (#\e (east))
        (#\s (south))
        (#\w (west))
        (#\n (north))
      )
    )
  )
  (round (* *score-s* *normalization-factor-s*))
)

;;-----------------------------------------------------------------------------

;; A function to display given character at agent's position.
(defun display-position (char)
  (move-cursor (1+ *row*) (1+ *column*)) ;; *row* = 0 means screen row = 1
  (write-char char)
  (delay *step-delay*)
)

;;=============================================================================

;;  #..   .#.   ..#   ...   ...   ...   ...   ...
;;  .x.   ... - .x.   .x# - .x.   .x. - .x.   #x. -
;;  ...   ...   ...   ...   ..#   .#.   #..   ...
;;   |     |     |     |     |     |     |     |
;;  ##.   .##   ..#   ...   ...   ...   ...   #..
;;  .x.   .x.   .x#   .x#   .x.   .x.   #x.   #x.
;;  ...   ...   ...   ..#   .##   ##.   #..   ...
;;   |     |     |     |     |     |     |     |
;;  ###   .##   ..#   ...   ...   ...   #..   ##.
;;  .x.   .x#   .x#   .x#   .x.   #x.   #x.   #x.
;;  ...   ...   ..#   .##   ###   ##.   #..   ...
;;   |     |     |     |     |     |     |     |
;;  ###   .##   ..#   ...   ...   #..   ##.   ###
;;  .x#   .x#   .x#   .x#   #x.   #x.   #x.   #x.
;;  ...   ..#   .##   ###   ###   ##.   #..   ...
;;    \   /       \   /       \   /       \   /
;;     ###         ..#         #..         ###
;;     .x#         .x#         #x.         #x. 
;;     ..#         ###         ###         #..

;; evaluate-d
;; Precondition:  sensors *e*, ..., *ne* must be all set to t;
;;    non-local variables ... must be set.
;; Postcondition: sensors *e*, ..., *ne* are all set to t;
;;    the score 0..100 is returned

;; The following macro is used only in test-agent.
;; It uses variables agent, clockwise-along-the-wall, counter-clockwise ...
(defmacro test-agent0 (primary secondary &rest away)
 `(case (eval agent)
    (,primary   (incf clockwise-along-the-wall))
    (,secondary (incf counter-clockwise-along-the-wall))
    (,away      (incf away-from-the-wall))
    (otherwise  (incf against-the-wall))
  )
)       

(defmacro test-agent ()
 '(let
    ((clockwise-along-the-wall 0)
     (counter-clockwise-along-the-wall 0)
     (away-from-the-wall 0)
     (against-the-wall 0)
    )
    ;; Body.
    (setq *nw* nil) (test-agent0 #\n #\w #\s #\e)
    (setq *n*  nil) (test-agent0 #\e #\w #\s)
    (setq *ne* nil) (test-agent0 #\e #\w #\s)
    (setq *e*  nil) (test-agent0 #\s #\w)
    (setq *se* nil) (test-agent0 #\s #\w)
    (setq *nw* T)   (test-agent0 #\s #\w)
    (setq *se* T)   (test-agent0 #\s #\w)
    (setq *e*  T)   (test-agent0 #\e #\w #\s)
    (setq *ne* T)   (test-agent0 #\e #\w #\s)
    (setq *n*  T)
    (setq *ne* nil) (test-agent0 #\e #\n #\w #\s)
    (setq *e*  nil) (test-agent0 #\s #\n #\w)
    (setq *se* nil) (test-agent0 #\s #\n #\w)
    (setq *s*  nil) (test-agent0 #\w #\n)
    (setq *sw* nil) (test-agent0 #\w #\n)
    (setq *ne* T)   (test-agent0 #\w #\n)
    (setq *sw* T)   (test-agent0 #\w #\n)
    (setq *s*  T)   (test-agent0 #\s #\n #\w)
    (setq *se* T)   (test-agent0 #\s #\n #\w)
    (setq *e*  T)
    (setq *se* nil) (test-agent0 #\s #\e #\n #\w)
    (setq *s*  nil) (test-agent0 #\w #\e #\n)
    (setq *sw* nil) (test-agent0 #\w #\e #\n)
    (setq *w*  nil) (test-agent0 #\n #\e)
    (setq *nw* nil) (test-agent0 #\n #\e)
    (setq *se* T)   (test-agent0 #\n #\e)
    (setq *nw* T)   (test-agent0 #\n #\e)
    (setq *w*  T)   (test-agent0 #\w #\e #\n)
    (setq *sw* T)   (test-agent0 #\w #\e #\n)
    (setq *s*  T)
    (setq *sw* nil) (test-agent0 #\w #\s #\e #\n)
    (setq *w*  nil) (test-agent0 #\n #\s #\e)
    (setq *nw* nil) (test-agent0 #\n #\s #\e)
    (setq *n*  nil) (test-agent0 #\e #\s)
    (setq *ne* nil) (test-agent0 #\e #\s)
    (setq *sw* T)   (test-agent0 #\e #\s)
    (setq *ne* T)   (test-agent0 #\e #\s)
    (setq *n*  T)   (test-agent0 #\n #\s #\e)
    (setq *nw* T)   (test-agent0 #\n #\s #\e)
    (setq *w*  T)
    (values 
      clockwise-along-the-wall
      counter-clockwise-along-the-wall
      away-from-the-wall
      against-the-wall
    )
  )
)

;------------------------------------------------------------------------------

(defun evaluate-d (agent)
  "evaluate decisions made by the agent in 36 possible situations."
  (multiple-value-bind 
    ;; Local variables.
    (clockwise          ;; Number of clockwise moves along the wall.
     counter-clockwise  ;; Number of counter clockwise moves along the wall.
     away               ;; Number of moves stepping away from the wall.
     against            ;; Number of moves when agent pushes against the wall. 
    )
    ;; Form which initializes the first four local variables.
    (test-agent) ;; This macro uses the variable agent.
    ;; Body
    (values 
      (round 
        (funcall (symbol-function *score-calculation*) 
          clockwise counter-clockwise away against
        )
      )
    )
  )
)

;;-----------------------------------------------------------------------------

;; Updates *row* *column* to a random position at the wall.
;; (Any agent which is far from walls will move in a straight line
;;  so a starting position far from walls would not help in evaluation.)
;; Then allows the agent to walk in the grid world and evaluates fitness.
;; (This function uses global variables *row* *column*, *e*, ..., *score*)

(defun run-agent (agent)
  (let 
    ((score-c 0) ;; Count of visited places at the wall. (v is for visited.)
     clockwise
     counter-clockwise
     away
     against
    )
    ;; Body
    (set-world-related-parameters)
    (clear-screen)
    (setq *score-s* 0)
    (dotimes (i *tours* nil)
      ;; Set random position
      (loop 
        (setq *row*    (random *max-rows*))
        (setq *column* (random *max-columns*))
        (when (characterp (aref *world* *row* *column*)) (return))
      )
      (read-sensors)
      (setq *came-from* nil) 
      (move-cursor 1 1)
      (display-world *world*)
      (display-position #\x)
      (dotimes (i *tour-length* nil)
        ;; make one step  
        (case (eval agent)
          (#\e (east))
          (#\s (south))
          (#\w (west))
          (#\n (north))
        )
        (display-position #\.)
      )
      (incf 
        score-c (count-erase-visited-places *world* *max-rows* *max-columns*)
      )
      (finish-output)
      (delay 1) ;; Delay between tours.
    )
    (move-cursor (1+ *max-rows*) 1)
    (reset-sensors)
    (multiple-value-setq (clockwise counter-clockwise away against) 
      (test-agent)
    )
    (format t
"
This agent, in 36 possible situations makes:
~2d moves clockwise along the wall,
~2d moves counter-clockwise along the wall,
~2d moves getting away from the wall, and
~2d moves trying to push against the wall.

Fitness on scale 0..100: 
~3d -- counting different cells at the wall the agent visited (evaluate-c),
~3d -- counting steps which ended at the wall (evaluate-s).
~3d -- ~a
~3d -- ~a
~3d -- ~a
~3d -- ~a"
      clockwise
      counter-clockwise
      away
      against
      (round (* score-c   *normalization-factor-c*))
      (round (* *score-s* *normalization-factor-s*)) 
      (values (round (score1 clockwise counter-clockwise away against)))
      (documentation 'score1 'function)
      (values (round (score2 clockwise counter-clockwise away against)))
      (documentation 'score2 'function)
      (values (round (score3 clockwise counter-clockwise away against)))
      (documentation 'score3 'function)
      (values (round (score4 clockwise counter-clockwise away against)))
      (documentation 'score4 'function)
    )
    ;; Add score computed by *score-calculation* if not listed above.

    (values)
  )
)

;;-----------------------------------------------------------------------------

;; Perfect agent. (for testing purposes)

(defparameter *perfect-agent*
 '(cond 
    ((and *e* (or (not *n*) (not *ne*))) #\e)
    ((and *s* (or (not *e*) (not *se*))) #\s)
    ((and *w* (or (not *s*) (not *sw*))) #\w)
    (t #\n)
  )
)

;;-----------------------------------------------------------------------------

