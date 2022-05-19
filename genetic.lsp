;; genetic.lsp
;; Generic genetic/evolutionary programming environment
;; Evolutionary programming in LISP 
;; educational materials inspired by Nils. J. Nilsson
;; March 23, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0

;;=============================================================================

;; Global variables which allow the user to pause, experiment with this data, 
;; and then restart main.
;; (To keep program clear, only function evolution modifies these variables.)
(defvar *previous-generation*) ;; Array of agents.
(defvar *current-generation*)  ;; Array of agents.
(defvar *fitness-data*)        ;; Array of integers.
(defvar *generation-number*)   ;; Number of the current generation.
(defvar *start-time*) ;; Time when we start creating generation 0.
(defvar *time-spent*) ;; Internal time spent on evolution before a pause.

;;=============================================================================

(defun print-gp-header (&optional (stream t))
  (format stream
"

 Gen    Time  Prf Avg   100  90  80  70  60  50  40  30  20  10   0"
  )
  (when (eq stream T) (force-output))
)

;;-----------------------------------------------------------------------------

(defun print-gp-statistics
  ;; Parameters.
  (generation-number time-spent perfect sum fitness-data &optional (stream t))
  ;; Body.
  (format stream
    "~&~4d ~7,2f ~4d ~3d   ~3d ~3d ~3d ~3d ~3d ~3d ~3d ~3d ~3d ~3d ~3d"
    generation-number
    (/ time-spent internal-time-units-per-second)
    perfect
    (round (/ sum *population-size*)) ;; Average fitness.
    (aref fitness-data (round (* 0 (/ *population-size* 10))))
    (aref fitness-data (round (* 1 (/ *population-size* 10))))
    (aref fitness-data (round (* 2 (/ *population-size* 10))))
    (aref fitness-data (round (* 3 (/ *population-size* 10))))
    (aref fitness-data (round (* 4 (/ *population-size* 10))))
    (aref fitness-data (round (* 5 (/ *population-size* 10))))
    (aref fitness-data (round (* 6 (/ *population-size* 10))))
    (aref fitness-data (round (* 7 (/ *population-size* 10))))
    (aref fitness-data (round (* 8 (/ *population-size* 10))))
    (aref fitness-data (round (* 9 (/ *population-size* 10))))
    (aref fitness-data (1- *population-size*)) ;; last element
  )
  (when (eq stream T) (force-output))
)

;;-----------------------------------------------------------------------------

;; print-gp-agent <index> [<stream>]
;; Prints to the output stream (file) a list with
;; the string containing date and time when the experiment begun,
;; the generation number of the agent, the score of the agent and the agent.

(defun print-gp-agent (index &optional (stream t))
  (pprint
    (list
      *start-time*
      *generation-number*
      (aref *fitness-data* index)
      (aref *current-generation* index)
    )
    stream
  )
  (terpri stream)
)

(defun save-agent (agent)
  (with-open-file 
    (agents-stream "log.dat" :direction :output
      :if-exists :append :if-does-not-exist :create
    ) 
    (pprint
      (list
        *start-time*
        "Saved by the user after generation:"
        *generation-number*
        (apply (symbol-function *evaluation-method*) agent nil)
        agent
      )
      agents-stream
    )
    (terpri agents-stream)
  )
)

;;=============================================================================

;; tournament
;; This function returns the index of the most fit among 
;; *tournament-size* randomly chosen agents.
(defun tournament (fitness-data)
  (let
    ((best (random *population-size*)) ;; index of best so far. 
     current  ;; index of current agent
    ) ;; Body
    (dotimes (i (1- *tournament-size*) best)
      (setq current (random *population-size*))
      (when (> (aref fitness-data current) (aref fitness-data best))
         (setq best current)
      )
    )
  )
)

;;-----------------------------------------------------------------------------

;; mysort
;; Sorting function.
;; key-array and info-array are of the same length.
;; key-array contains integers (from the range 0..100 and 
;; its initial fragment is sorted in a decreasing order, although it is
;; not guaranteed that this initial segment contains the biggest numbers
;; of this array).
;; The task is to sort key-array in a decreasing order, 
;; simultaneously performing the same swaps in info-array
(defun mysort (key-array info-array)
  (let*
    ((length (array-dimension key-array 0))
     (aux-array (make-array length))
    )
    (dotimes (i length nil)
      (setf 
        (aref aux-array i) 
        (cons (aref key-array i) (aref info-array i))
      ) 
    )
    (sort aux-array #'> :key #'car) ;; sort is built in
    (dotimes (i length nil)
      (setf (aref key-array  i) (car (aref aux-array i)))
      (setf (aref info-array i) (cdr (aref aux-array i)))
    )
    nil
  )
)

;;-----------------------------------------------------------------------------

(defun next-generation (previous-generation fitness-data new-generation)
  (let*
    ;; Local variables
    ( (new-index 0)
      (crossovers (round (/ (* *crossover-percent* *population-size*) 200)))
              ;; that many crossovers, each producing two agents.
      (survivors (round (/ (* *survive-percent*  *population-size*) 100)))
      (mutations (- *population-size* (* 2 crossovers) survivors))
      agent1
      agent2
      two-agents
    )
    ;; Body.
    ;; Perform crossovers.
    (dotimes (i crossovers nil)
      (setq agent1 (aref previous-generation (tournament fitness-data)))
      (setq agent2 (aref previous-generation (tournament fitness-data)))
      (setq two-agents 
        (apply (symbol-function *crossover-operation*) agent1 agent2 nil)
      )
      (setf (aref new-generation new-index) (first two-agents))
      (incf new-index) ;; increment
      (setf (aref new-generation new-index) (second two-agents))
      (incf new-index) 
    )
    ;; Perform random mutations.
    (dotimes (i mutations nil)
      (setf 
        (aref new-generation new-index) 
        (apply (symbol-function *mutation-operation*) 
          (aref previous-generation (tournament fitness-data)) nil
        ) 
      )
      (incf new-index) 
    )
    ;; Allow some agents from previous-generation to survive in new-generation.
    (dotimes (i survivors nil)
      (setf (aref new-generation new-index)
        (aref 
          previous-generation 
          (if *only-best-survive* i (tournament fitness-data)) ;; i or random
        )
      )
      (incf new-index) 
    )
  )
)

;;-----------------------------------------------------------------------------

(defun evolution ()
  ;; See the note about global variables, at the beginning of this file.
  (let* 

    ;; Local variables.
    (start-time
     temp
     sum ;; Sum of fitness values.
     perfect ;; Count of perfect (100) scores.
     count
    )

    ;; Body.
    (clear-input) 

    (with-open-file 
      (results-stream "log.txt" :direction :output
         :if-exists :append :if-does-not-exist :create
      )
      (with-open-file 
        (agents-stream "log.dat" :direction :output
           :if-exists :append :if-does-not-exist :create
        )
        (if (eq *generation-number* -1)
          (tee-n (print-gp-header results-stream))
          (print-gp-header)
        )

        (loop
          ;; Get CPU time.
          (setq start-time (get-internal-run-time)) 
 
          ;; Create a generation of agents.
          (incf *generation-number*)
          (cond 
            ((eq *generation-number* 0) ;; Create generation 0.
              (dotimes (i *population-size* nil)
                (setf 
                  (aref *current-generation* i) 
                  (apply (symbol-function *random-agent-generator*) nil)
                )
              )
            )
            (T ;; Otherwise create next generation.
              (setq temp *previous-generation*)
              (next-generation *current-generation* *fitness-data* temp)
              (setq *previous-generation* *current-generation*)
              (setq *current-generation* temp)
            )
          )

          ;; Evaluate all agents.
          (dotimes (i *population-size* nil)
            (setf (aref *fitness-data* i) 
              (apply (symbol-function *evaluation-method*) 
                (aref *current-generation* i) nil
              )
            )
          )

          ;; Sort agents by fitness values.
          (mysort *fitness-data* *current-generation*)

          ;; Find the sum of fitness values.
          (setq sum 0)
          (dotimes (i *population-size* nil)
            (incf sum (aref *fitness-data* i))
          )

          ;; Save best agents.
          (dotimes (i *min-best-agents-saved* nil)
            (print-gp-agent i agents-stream)
          )          

          ;; Find the number of perfect (100) scores and save perfect agents.
          (setq perfect 0)
          (loop
            (when (< (aref *fitness-data* perfect) 100) (return))
            (when 
              (and 
                (< perfect *max-perfect-agents-saved*)
                (>= perfect *min-best-agents-saved*)
              )
              (print-gp-agent perfect agents-stream)            
            )
            (incf perfect)
          )


          ;; Update the total time spent on evolution.
          (incf *time-spent* (- (get-internal-run-time) start-time))

          ;; Print statistics.
          (tee-n 
            (print-gp-statistics *generation-number* 
             *time-spent* perfect sum *fitness-data* 
             results-stream
            )
          )

          ;; Return from loop if user pressed enter.
          (setq count 0)
          (when
            (loop
              (when (listen) (clear-input) (return T))
              (when (>= count *generation-delay*) (return nil))
              (incf count) ;; increment
              (sleep 1)
            )
            (return)
          )

        ) ;; End loop.

        (tee-n (terpri results-stream))

      ) ;; End with-open-file.
    ) ;; End with-open-file.

    (values) ;; Return 0 values.

  )
)  

;;=============================================================================


