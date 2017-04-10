#######################################################################
# Tcl/tk Tic-Tac-Toe player                                           #
#  written by Amy McGovern amy@cs.umass.edu 09/96                     #
#  permission given to modify the code as long as you keep            #
#   my name at the top                                                #
#######################################################################

#######################################################################
# this file contains code for each of the available opponents         #
#######################################################################

#######################################################################
# returns the first postion free starting from the upper left corner  #
# and continuing to the lower right                                   #
#######################################################################
proc first_available_player {} {
    global board
    global positions

    foreach pos $positions {
	if {$board($pos) == " "} {
	    return $pos
	}
    }
    return -1
}

#######################################################################
# randomly returns a free spot                                        #
#######################################################################
proc random_player {} {
    global positions
    global board

    set tmp [expr [rand] % 9]
    while {$board([lindex $positions $tmp]) != " "} {
	set tmp [expr [rand] % 9]
    }

    return [lindex $positions $tmp]
}

#######################################################################
# returns the best move according to V                                #
#######################################################################
proc td_player {} {
    global V
    global board
    global positions
    global info

    set max_v -100000

    # return a random move with probability epsilon
    if {[expr [expr [rand] % 10000] / 10000.0] < $info(epsilon)} {
	return [random_player]
    } 

    # return the best move according to V
    foreach pos $positions {
	if {$board($pos) == " "} {
	    set board($pos) $info(turn)
	    set maps [map_board]
	    if {![info exists V($maps)]} {
		set V($maps) 0
	    }
	    if {$V($maps) >= $max_v} {
		set max_v $V($maps)
#		puts "max_v: $max_v on $pos"
		set moves [lappend moves $pos]
	    }
	    set board($pos) " "
	}
    }
    
    # choose randomly among equal V's
    #     puts "choosing from $moves"
    return [lindex $moves [expr [rand] % [llength $moves]]]
}

#######################################################################
# returns a winning move, if that is possible.  else returns the move #
# most likely to win                                                  #
#######################################################################
proc optimal_search {turn} {
    global board
    global info
    global optimalV
    global positions

    # are we at a leaf node?
    set winner [game_over]
    if {$winner == $info(turn)} {
	set optimalV([map_board]) 1
	return 1
    } elseif {$winner == "TIE"} {
	set optimalV([map_board]) 0
	return 0
    } elseif {$winner != -1} {
	set optimalV([map_board]) -1
	return -1
    }

    # otherwise, recurse (using caching if possible)
    if {$turn == "X"} {set opponent "O"} else {set opponent "X"}
    foreach pos $positions {
	if {$board($pos) == " "} {
	    set board($pos) $turn
	    if {![info exists optimalV([map_board])]} {
		set r($pos) [optimal_search $opponent]
	    } else {
		set r($pos) $optimalV([map_board])
	    }
	    set board($pos) " "
	} 
    }    

    # now pick the max or min (depending on whose turn it is)
    if {$turn == $info(turn)} {
	set max_r -10000
	foreach pos $positions {
	    if {([info exists r($pos)]) && ($r($pos) >= $max_r)} {
		set max_r $r($pos)
	    }
	}
	if {![info exists optimalV([map_board])]} {
	    set optimalV([map_board]) $max_r
	}
    } else {
	set min_r 10000
	foreach pos $positions {
	    if {([info exists r($pos)]) && ($r($pos) <= $min_r)} {
		set min_r $r($pos)
	    }
	}
	if {![info exists optimalV([map_board])]} {
	    set optimalV([map_board]) $min_r
	}
    }
    return $optimalV([map_board])
}


#######################################################################
# calling function for optimal_player                                 #
#######################################################################
proc optimal_player {} {
    global info
    global optimalV
    global positions
    global board

    optimal_search $info(turn)
    set max -10000
    foreach pos $positions {
	if {$board($pos) == " "} {
	    set board($pos) $info(turn)
	    if {$optimalV([map_board]) > $max} {
		set move $pos
		set max $optimalV([map_board])
	    }
	    set board($pos) " "
	}
    }
    return $move
}


#######################################################################
# either waits for the human player to make a move or returns a move  #
# as specified by the training method                                 #
#######################################################################
proc human_player {} {
    global info
    global board
    
    tkwait variable info(playermoved)
    set board($info(move)) $info(turn)
    set info(playermoved) 0
}

#######################################################################
# returns 1 if the given player won, -1 if the given player           #
# lost, or 0 if it is a tie or the game is not over                   #
#######################################################################
proc get_reward {player} {
    global board

    set winner [game_over]
    if {$player == "X"} {
	set opponent "O"
    } else {
	set opponent "X"
    }

    if {$winner == $player} {
	return 1
    } elseif {$winner == $opponent} {
	return -1
    } else {
	return 0
    }
}

#######################################################################
# runs info(iters) training iterations on the player specified by     #
# info(player)                                                        #
#######################################################################
proc train_system {} {
    global info

    set info(fp) [open "training/$info(xplayer).[pid]" "w"]
     
    destroy .opt
    set info(training) 1
    puts "training for $info(iters) iterations"
    for {set iters 0} {$iters < $info(iters)} {incr iters} {
	init_game
	play_game
    }
    puts "done training"
}

