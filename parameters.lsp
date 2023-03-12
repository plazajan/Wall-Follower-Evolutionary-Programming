;; parameters.lsp
;; Evolutionary programming in LISP 
;; educational software inspired by Nils. J. Nilsson
;; March 17, 2000
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


;; You can modify parameters in this file.
;; For advanced experiments, modify agents.lsp.

;; To run this program invoke lisp and type: (load "main")

;;=============================================================================

(defparameter *evaluation-method* 'evaluate-d)
;; Use 'evaluate-c or 'evaluate-s or 'evaluate-d. 
;; 'evaluate-c counts different visited *c*ells at the wall. 
;; 'evaluate-s counts *s*teps which ended at the wall. See evaluate.lsp.
;; 'evaluate-d evaluates *d*ecisions made by agent in 36 possible situations.

(defparameter *grid-world-file* "world1.dat")
;; Use "world1.dat" or "world2.dat" or create your own grid world file.
;; This is used by evaluate-c, evaluate-s and run-agent.

(defparameter *tours* 1) 
;; Use a positive integer.
;; That many tours of an agent will count while calculating agent's fitness.
;; This is used by evaluate-c, evaluate-s and run-agent.

(defparameter *tour-length* 56) 
;; Use positive integer.
;; This is used by evaluate-c, evaluate-s and run-agent.

(defparameter *score-calculation* 'score2)
;; Use 'score1, 'score2, 'score3, 'score4 or define your own method.
;; This is used with evaluate-d.

;; To calculate the score we use the following parameters.
;; clockwise         - number of clockwise moves along the wall.
;; counter-clockwise - number of counter clockwise moves along the wall.
;; away              - number of times agent moved away from the wall.
;; against           - number of moves when agent pushes against the wall.
;; Their values are obtained by testing the agent in 36 possible situations.
;; The functions below turn these 4 values into a single score 0..100.
;; (The second line in every function definition is a documentation string.)

(defun score1 (clockwise counterclockwise away against)
  "normalized( clockwise - counterclockwise )"
  (declare (ignore away against))
  (/ (+ 36 (- clockwise counterclockwise)) 0.72)
)

(defun score2 (clockwise counterclockwise away against)
  "normalized( | clockwise - counterclockwise | )"
  (declare (ignore away against))
  (/ (abs (- clockwise counterclockwise)) 0.36)
)

(defun score3 (clockwise counterclockwise away against)
  "normalized( clockwise - counterclockwise - away - against )"
  (/ (+ 36 (- clockwise counterclockwise away against)) 0.72)
)

(defun score4 (clockwise counterclockwise away against)
  "normalized( | clockwise - counterclockwise | - away - against )"
  (/ (+ 36 (- (abs (- clockwise counterclockwise)) away against)) 0.72)
)

;;-----------------------------------------------------------------------------

(defparameter *random-agent-generator* 'random-agent2)
;; Use 'random-agent1 or 'random-agent2 defined in agents.lsp
;; or define your own function.

(defparameter *agent-size* 3) 
;; Use positive integer.
;; This is max height of randomly generated trees representing
;; Boolean expressions which are used in the definition of an agent.

;;-----------------------------------------------------------------------------

(defparameter *population-size* 5000) 
;; Use positive integer.

;;-----------------------------------------------------------------------------

(defparameter *tournament-size* 7) 
;; Use positive integer. 
;; Agent is always selected in a tournament with that many random participants.

;;-----------------------------------------------------------------------------

(defparameter *survive-percent*   10)
(defparameter *crossover-percent* 89)
(defparameter *mutation-percent*   1)
;; Use non-negative numbers which add up to 100.

(defparameter *only-best-survive* nil) 
;; Use nil or t. 
;; If nil, winers of tournaments with randomly selected agents will survive.

(defparameter *crossover-operation* 'crossover2)
;; Use 'crossover1 or 'crossover2 defined in agents.lsp
;; or define your own function.

(defparameter *mutation-operation* 'mutation2)
;; Use 'mutation1 or 'mutation2 defined in agents.lsp
;; or define your own function.

;; Make sure that you are using operations compatible with the
;; structure of the agents produced by *random-agent-generator*.

;;-----------------------------------------------------------------------------

(defparameter *step-delay* 0.05) 
;; Use non-negative float.
;; Delay in seconds between steps displayed by run-agent. 

(defparameter *generation-delay* 1) 
;; Use non-negative float.
;; Represents length of period, in seconds, after statistics are printed 
;; when you can press enter to pause evolution.

;;-----------------------------------------------------------------------------

(defparameter *min-best-agents-saved* 0)
;; Use a natural number not bigger than *population-size*.

(defparameter *max-perfect-agents-saved* 1)
;; Use a natural number not bigger than *population-size*.

;; This program saves top agents in file log.dat.
;; For every generation, the program saves *min-best-agents-saved* best agents;
;; if all these agents have a perfect score of 100 and there are more such 
;; agents, the program will continue saving them up to a total of
;; *max-perfect-agents-saved*  

;; Notice that different evaluation functions may disagree on what agent
;; deserves the score of 100, so the meaning of "perfect" is relative.

;;=============================================================================




