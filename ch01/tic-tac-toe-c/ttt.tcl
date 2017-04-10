#!/exp/rcf/share/bin/wish4.0 -f

#######################################################################
# Tcl/tk Tic-Tac-Toe player                                           #
#  written by Amy McGovern amy@cs.umass.edu 09/96                     #
#  permission given to modify the code as long as you keep            #
#   my name at the top                                                #
#######################################################################

#######################################################################
# this is the main file - invoke ./ttt.tcl to run the program         #
#######################################################################

#######################################################################
# returns a unique string for each baord position                     #
#######################################################################
proc map_board {} {
    global board
    global positions

    foreach pos $positions {
	if {$board($pos) == " "} {
	    set map [append map "B"]
	} else {
	    set map [append map $board($pos)]
	}
    }	
    return $map
}

#######################################################################
# prints out the current board configuration in a readable form       #
#######################################################################
proc print_board {} {
    global board

    set b [map_board]
    puts "[string range $b 0 2]\n[string range $b 3 5]\n[string range $b 6 8]\n"
}

#######################################################################
# runs one game (with training)                                       #
#######################################################################
proc play_game {} {
    global board
    global info
    global positions
    global V

    set news " "
    while {[set winner [game_over]] == -1} {
	if {$info(turn) == "X"} {
	    set move [$info(xplayer)]
	    set board($move) "X"
	} else {
	    if {[set winner [game_over]] != -1} {
		if {$winner == "X"} {
		    set reward -1
		    break
		} 
	    } else {
		set move [$info(oplayer)]
		set board($move) $info(turn)
		set reward [get_reward $info(turn)]
	    }
	    if {$info(learning) == "on"} {
		set news [map_board]
		if {[info exists olds]} {
		    if {![info exists V($olds)]} { set V($olds) 0}
		    if {![info exists V($news)]} { set V($news) 0}
		    set V($olds) [expr $V($olds) + [expr $info(alpha) * [expr $reward + [expr [expr $info(gamma) * $V($news)] - $V($olds)]]]]
		}
		set olds $news
	    }
	}

	# make it the next person's turn
	if {$info(turn) == "X"} {set info(turn) "O"} else {set info(turn) "X"}
    }

    if {$info(learning) == "on"} {
	set V($news) [get_reward $info(computer)]
	#    puts "updating last state *$news* with reward [get_reward $info(computer)]"
    }

    # update the scoreboard
    if {$winner == "X"} {
	incr info(xwins)
	incr info(oloss)
    } elseif {$winner == "O"} {
	incr info(owins)
	incr info(xloss)
    } else {
	incr info(otie)
	incr info(xtie)
    }

    # and write out the wins to a file
    if {[info exists info(fp)]} {
	puts $info(fp) "$info(run) [get_reward $info(computer)]"
    }
    incr info(run)
}


#######################################################################
# main                                                                #
#######################################################################
source init.tcl
source gui.tcl
source players.tcl
source random.tcl
source game.tcl
# source optimalV

init_globals
init_game
init_gui
play_game

