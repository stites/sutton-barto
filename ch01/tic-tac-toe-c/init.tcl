#######################################################################
# Tcl/tk Tic-Tac-Toe player                                           #
#  written by Amy McGovern amy@cs.umass.edu 09/96                     #
#  permission given to modify the code as long as you keep            #
#   my name at the top                                                #
#######################################################################

#######################################################################
# this file contains initialization functions                         #
#######################################################################

#######################################################################
# init the game and reset all game dependent vars                     #
#######################################################################
proc init_game {} {
    global board
    global positions
    global info
    
    if [winfo exists .game] {
	foreach row {{top ul uc ur} {mid cl cc cr} {bot ll lc lr}} {
	    set f .game.[lindex $row 0]
	    foreach pos [lrange $row 1 3] {
		$f.$pos configure -bg $info(color)
	    }
	}
    }
    
    foreach pos $positions {
	set board($pos) " "
    }

    set info(turn) $info(human)
}

#######################################################################
# performs one time only initializations on globals                   #
#######################################################################
proc init_globals {} {
    global positions
    global info

    set positions {ul uc ur cl cc cr ll lc lr}
    set info(xwins) 0
    set info(xloss) 0
    set info(xtie) 0
    set info(owins) 0
    set info(oloss) 0
    set info(otie) 0

    set info(epsilon) 0.0
    set info(alpha) 0.05
    set info(gamma) 1

    set info(human) "X"
    set info(computer) "O"
    set info(xplayer) "human_player"
    set info(oplayer) "td_player"
    set info(learning) "on"
    set info(training) 0
    set info(iters) 100
    set info(run) 0

    srand [pid]

}
