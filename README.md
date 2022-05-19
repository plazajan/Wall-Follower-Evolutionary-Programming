# LISP-Evolutionary-Programming-Wall-Following

;; Evolutionary programming in LISP
;; educational materials inspired by Nils. J. Nilsson
;; March 16, 2000
;; https://github.com/plazajan
;; (c) 2000 Jan A. Plaza
;; Use under Creative Commons Attribution Share-Alike International License 4.0

-------------------------------------------------------------------------------

Copy this entire directory to your account.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

If on a UNIX/Linux system with GNU Common LISP (clisp) installed,
in a shell, change to this directory and type type at the shell prompt:

	start

then read carefully and follow instructions on the screen.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

You can also start the program from any Common LISP listener:

     > (load "main")

-------------------------------------------------------------------------------

The program is inspired by the discussion of evolutionary programming in
Nils J. Nilsson, Artificial Inteligence: A New Synthesis.

The program provides multiple options for setting up different evolutionary
programming experiments and observing the pace of evolution.

-------------------------------------------------------------------------------

An agent in a grid world senses its immediate surroundings (in 8 directions)
but has no memory of the past,
and performs a step in one of the 4 cardinal directions.
Goal: evolve an agent which finds a wall and continues moving
following the wall.

More informtion in README.md and in messages the program prints while runing.

-------------------------------------------------------------------------------

Here is a sample session.

linux$ start

   EVOLUTIONARY PROGRAMMING EXPERIMENT
   An agent in a grid world repeatedly senses its immediate surroundings,
   has no memory of earlier positions, and makes a step. This program tries to 
   evolve an agent which finds a wall and continues moving along the wall.


  To change the evolutionary process parameters, edit parameters.lsp - type: (e)
  To start the evolution, type: (start)

[1]> (start)

================================================================================
NEW EXPERIMENT

2022-05-19 16:51:13

PARAMETERS
*evaluation-method* = EVALUATE-D
*score-calculation* = SCORE2
*random-agent-generator* = RANDOM-AGENT2
*agent-size* = 3
*population-size* = 5000
*tournament-size* = 7
*survive-percent* = 10
*crossover-percent* = 89
*mutation-percent* = 1
*only-best-survive* = NIL
*crossover-operation* = CROSSOVER2
*mutation-operation* = MUTATION2

   ABOUT OUTPUT DATA
   Agents are evaluated on scale 0..100. The following listing gives 
   generation number, CPU time, number of agents with the perfect score of 100,
   average score, and percentiles from 100 (best score) to 0 (worst score):
   for instance, 53 under the header of 70 means that 
   an agent with a score 53 is better than 70% of agents. 

   INSTRUCTIONS
   To pause, press enter and wait until program completes current cycle.
   You will be able to look at the current generation and then restart.

 Gen    Time  Prf Avg   100  90  80  70  60  50  40  30  20  10   0
   0    0.23    0  13    69  31  22  17  14  11   8   6   3   0   0
   1    0.43    0  27    81  44  39  33  31  28  22  19  17   8   0
   2    0.65    0  37    75  56  50  44  42  39  36  33  25  17   0
   3    0.87    0  46    92  64  58  56  53  50  44  42  33  22   0


To see hints how to continue, type: (h)

[2]> (h)

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

[3]> (agent 1)

(COND
 ((AND (AND (AND (OR (NOT *N*) (NOT *NE*)) (OR *SE* (NOT *SW*))) *SE*)
   (OR *E* (NOT *SE*)))
  #\e)
 ((AND *S* (AND *W* (NOT *E*))) #\s)
 ((AND (OR (OR (AND (NOT *N*) *SE*) (OR (NOT *E*) *W*)) *S*)
   (OR (NOT *N*) *NW*))
  #\w)
 (T #\n))
 
[4]> (run-agent (agent 1))

#############
###   #   ###
##    #    ##
#           #
#           #
#           #
###.      ###
#...        #
#.         x#
#...........#
##    #    ##
###   #   ###
#############

This agent, in 36 possible situations makes:
33 moves clockwise along the wall,
 0 moves counter-clockwise along the wall,
 1 moves getting away from the wall, and
 2 moves trying to push against the wall.

Fitness on scale 0..100: 
 21 -- counting different cells at the wall the agent visited (evaluate-c),
 21 -- counting steps which ended at the wall (evaluate-s).
 96 -- normalized( clockwise - counterclockwise )
 92 -- normalized( | clockwise - counterclockwise | )
 92 -- normalized( clockwise - counterclockwise - away - against )
 92 -- normalized( | clockwise - counterclockwise | - away - against )

[5]> (re-start)

--------------------------------------------------------------------------------

   ABOUT OUTPUT DATA
   Agents are evaluated on scale 0..100. The following listing gives 
   generation number, CPU time, number of agents with the perfect score of 100,
   average score, and percentiles from 100 (best score) to 0 (worst score):
   for instance, 53 under the header of 70 means that 
   an agent with a score 53 is better than 70% of agents. 

   INSTRUCTIONS
   To pause, press enter and wait until program completes current cycle.
   You will be able to look at the current generation and then restart.

 Gen    Time  Prf Avg   100  90  80  70  60  50  40  30  20  10   0
   4    1.10    0  52    92  69  67  64  61  56  53  47  39  25   0
   5    1.33    0  57    97  75  72  69  67  64  61  53  42  28   0
   6    1.59    0  63    97  83  78  75  72  69  67  58  50  33   0
   7    1.83    0  69    97  89  86  83  81  78  72  64  53  33   0
   8    2.08    1  74   100  92  89  89  86  83  78  69  58  39   0


To see hints how to continue, type: (h)

[6]> 




===============================================================================


