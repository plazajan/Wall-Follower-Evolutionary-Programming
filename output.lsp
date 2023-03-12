;; print.lsp 
;; Printing messages related to gp.lsp.
;; Evolutionary programming in LISP
;; educational software inspired by Nils. J. Nilsson
;; March 16, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza

;; This file is part of LISP-Evolutionary-Programming-Wall-Follower. 
;; LISP-Evolutionary-Programming-Wall-Follower is free software: 
;; you can redistribute it and/or modify it under the terms of
;; the GNU General Public License as published by the Free Software Foundation, 
;; either version 3 of the License, or (at your option) any later version. 
;; LISP-Evolutionary-Programming-Wall-Follower is distributed in the hope 
;; that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY 
;; or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU General Public License for more details. 
;; You should have received a copy of the GNU General Public License 
;; along with LISP-Evolutionary-Programming-Wall-Follower. 
;; If not, see https://www.gnu.org/licenses/.

;;=============================================================================

(defun print-introduction (&optional (stream t))
  (format stream
"

   EVOLUTIONARY PROGRAMMING EXPERIMENT
   An agent in a grid world repeatedly senses its immediate surroundings,
   has no memory of earlier positions, and makes a step. This program tries to 
   evolve an agent which finds a wall and continues moving along the wall.
"
  )
)

;;-----------------------------------------------------------------------------

(defun print-gp-experiment (&optional (stream t))
  (terpri stream)
  (terpri stream)
  (dotimes (i 80 nil) (write-char #\= stream))
  (format stream
"
NEW EXPERIMENT

~a

PARAMETERS"
    (time-string)
  )
)

;;-----------------------------------------------------------------------------

(defun print-gp-explanations (&optional (stream t)) 
  (format stream
"

   ABOUT OUTPUT DATA
   Agents are evaluated on scale 0..100. The following listing gives 
   generation number, CPU time, number of agents with the perfect score of 100,
   average score, and percentiles from 100 (best score) to 0 (worst score):
   for instance, 53 under the header of 70 means that 
   an agent with a score 53 is better than 70% of agents. "
  )
)

;;-----------------------------------------------------------------------------

(defun print-gp-instructions (&optional (stream t)) 
  (format stream
"

   INSTRUCTIONS
   To pause, press enter and wait until program completes current cycle.
   You will be able to look at the current generation and then restart."
  )
)

;;-----------------------------------------------------------------------------

(defun h (&optional (stream t))
  (format stream
"
The values of parameters of this experiment and its statistics are saved 
in file log.txt. If parameters *min-best-agents-saved* and 
*max-perfect-agents-saved* are non-zero, another file, log.dat contains best 
agents, together with the starting time of the experiment, generation numbers 
and fitness scores. The starting time can be used for cross referencing.

While still in the Lisp listener you can test the data produced by this 
evolution process. Arrays *previous-generation* and *current-generation* 
contain agents sorted with respect to fitness results, from highest to lowest. 
Array *fitness-data* corresponds to *current-generation*;
sorry, no fitness results for previous generation. 
To see agents from *current-generation*, type (agent N) where N is 1, 2,...
To see agents from *previous-generation*, type replace N by -1, -2, ...
To run the best agent, type:  (run-agent (agent 1))
To save the best agent in file log.dat, type: (save-agent (agent 1))

You can change the parameters of this process either by using setq in the Lisp
listener or by calling function (e), editing the file and reloading.
If *population-size* has been increased, restart option is not available.

To restart the evolution process, type: (re-start)
To start a new experiment, type: (start)

"   
  )
  (values)
)

;;=============================================================================
