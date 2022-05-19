;; interface.lsp
;; Evolutionary programming in LISP
;; educational materials inspired by Nils. J. Nilsson
;; March 27, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0

;;-----------------------------------------------------------------------------

(defun initialize-global-variables ()
  (setq *previous-generation* (make-array *population-size*))
  (setq *current-generation* (make-array *population-size*))
  (setq *fitness-data* (make-array *population-size*))
  (setq *generation-number* -1)
  (setq *time-spent* 0)
  (setq *start-time* (time-string))
  (setf (get '*evaluation-method* 'old-value) 'no-value)
  (setf (get '*grid-world-file* 'old-value) 'no-value)
  (setf (get '*tours* 'old-value) 'no-value)
  (setf (get '*tour-length* 'old-value) 'no-value)
  (setf (get '*score-calculation* 'old-value) 'no-value)
  (setf (get '*random-agent-generator* 'old-value) 'no-value)
  (setf (get '*agent-size* 'old-value) 'no-value)
  (setf (get '*population-size* 'old-value) 'no-value)
  (setf (get '*tournament-size* 'old-value) 'no-value)
  (setf (get '*survive-percent* 'old-value) 'no-value)
  (setf (get '*crossover-percent* 'old-value) 'no-value)
  (setf (get '*mutation-percent* 'old-value) 'no-value)
  (setf (get '*only-best-survive* 'old-value) 'no-value)
  (setf (get '*crossover-operation* 'old-value) 'no-value)
  (setf (get '*mutation-operation* 'old-value) 'no-value)
)
  
;;-----------------------------------------------------------------------------

(defun print-parameter (name stream)
  (let ((value (symbol-value name)))
    (unless (eq value (get name 'old-value))
      (setf (get name 'old-value) value) 
      (tee-2 (format stream "~%~a = ~a" (string-downcase name) value))
      (cond
        ((and (functionp value) (documentation value 'function))
         (tee-2 (format stream "~&  -- ~a" (documentation value 'function)))
        )
        ((and (symbolp value) (documentation value 'variable))
         (tee-2 (format stream "~&  -- ~a" (documentation value 'variable)))
        )
      )
    )
  )
)

;;-----------------------------------------------------------------------------

(defun check-parameters (stream)
  (when 
    (or 
      (eq *evaluation-method* 'evaluate-c) (eq *evaluation-method* 'evaluate-s)
    )
    (set-world-related-parameters)
    (tee-n (terpri stream))
    (tee-n (display-world *world* stream))
    (print-parameter '*evaluation-method* stream)
    (print-parameter '*grid-world-file* stream)
    (tee-2
      (format stream "~%  -- shown above, ~ax~a, with ~a places at the wall."
        *max-rows* *max-columns* *at-the-wall*
      )
    )
    (print-parameter '*tours* stream)
    (print-parameter '*tour-length* stream)
    (tee-2 
      (format stream 
        "~%  -- total number of steps agent takes during evaluation = ~a."
        (* *tours* *tour-length*)
      )
    )
  )
  (when (eq *evaluation-method* 'evaluate-d)
    (print-parameter '*evaluation-method* stream)
    (print-parameter '*score-calculation* stream)
  )
  (print-parameter '*random-agent-generator* stream)
  (print-parameter '*agent-size* stream)  
  (print-parameter '*population-size* stream)
  (print-parameter '*tournament-size* stream)
  (print-parameter '*survive-percent* stream)
  (print-parameter '*crossover-percent* stream)
  (print-parameter '*mutation-percent* stream)
  (unless (zerop *survive-percent*)
    (print-parameter '*only-best-survive* stream)
  )
  (unless (zerop *crossover-percent*)
    (print-parameter '*crossover-operation* stream)
  )
  (unless (zerop *mutation-percent*) 
    (print-parameter '*mutation-operation* stream)
  )
)

;;=============================================================================

(defun start (&optional (anew t))
  (let
    ((log-existed (probe-file "log.txt")))
    (with-open-file
      (results-stream "log.txt" :direction :output
         :if-exists :append :if-does-not-exist :create
      )
      (unless log-existed
        (print-introduction results-stream)
        (print-gp-explanations results-stream)
      )
      (if anew
        (tee-n (print-gp-experiment results-stream))
        (dotimes (i 80 nil) (write-char #\-))
      )
      (when anew 
        (initialize-global-variables)
      )
      (check-parameters results-stream)
      (print-gp-explanations)
      (print-gp-instructions)
    )
  )
  (reset-sensors) ;; Important for evaluate-d.
  (evolution)
  (format t "~%To see hints how to continue, type: (h)")
  (values)
)

;;-----------------------------------------------------------------------------

(defun re-start ()
  (start nil)
)

;;=============================================================================

(defun agent (&optional (n 0))
  (cond
    ((zerop n) 
      (format t "Perfect agent provided for testing purposes.")
      '(cond
         ((and *e* (or (not *n*) (not *ne*))) #\e)
         ((and *s* (or (not *e*) (not *se*))) #\s)
         ((and *w* (or (not *s*) (not *sw*))) #\w)
         (t #\n)
       )
    )
    ((minusp n) (aref *previous-generation* (- -1 n)))
    ((plusp n)  (aref *current-generation*  (1- n)))
  )
)
