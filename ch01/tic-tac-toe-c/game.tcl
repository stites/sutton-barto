#######################################################################
# Tcl/tk Tic-Tac-Toe player                                           #
#  written by Amy McGovern amy@cs.umass.edu 09/96                     #
#  permission given to modify the code as long as you keep            #
#   my name at the top                                                #
#######################################################################

#######################################################################
# returns the winner of the current game, if there is one.  returns   #
# TIE if no one won.  If the game is not over, returns -1             #
#######################################################################
proc game_over {} {
    global board
    global positions

    # is there a straight line of X's or O's?
    if {($board(ur) != " ") && ($board(ur) == $board(uc)) && \
	    ($board(uc) == $board(ul))} {
	highlight .top.ur .top.uc .top.ul
#	puts "top row"
	return $board(ur)
    } elseif {($board(cr) != " ") && ($board(cr) == $board(cc)) && \
		  ($board(cc) == $board(cl))} {
	highlight .mid.cr .mid.cc .mid.cl
#	puts "middle row"
	return $board(cr)
    } elseif {($board(lr) != " ") && ($board(lr) == $board(lc)) && \
		  ($board(lc) == $board(ll))} {
	highlight .bot.lr .bot.lc .bot.ll
#	puts "last row"
	return $board(lr)
    }

    # is there a straight line down?
    if {($board(ur) != " ") && ($board(ur) == $board(cr)) && \
	    ($board(cr) == $board(lr))} {
	highlight .top.ur .mid.cr .bot.lr
#	puts "first col"
	return $board(ur)
    } elseif {($board(uc) != " ") && ($board(uc) == $board(cc)) && \
		  ($board(cc) == $board(lc))} {
	highlight .top.uc .mid.cc .bot.lc
#	puts "mid col"
	return $board(uc)
    } elseif {($board(ul) != " ") && ($board(ul) == $board(cl)) && \
		  ($board(cl) == $board(ll))} {
	highlight .top.ul .mid.cl .bot.ll
#	puts "last col"
	return $board(ul)
    }

    # is there a winner on the diagonals?
    if {($board(ur) != " ") && ($board(ur) == $board(cc)) && \
	    ($board(cc) == $board(ll))} {
	highlight .top.ur .mid.cc .bot.ll
#	puts "right diag"
	return $board(ur)
    } elseif {($board(ul) != " ") && ($board(ul) == $board(cc)) && \
		  ($board(cc) == $board(lr))} {
	highlight .top.ul .mid.cc .bot.lr
#	puts "left diag"
	return $board(ul)
    }
    
    # is it a draw?
    foreach pos $positions {
	if {$board($pos) == " "} {
	    return -1
	}
    }

    # else return TIE
#    puts "TIE"
    return "TIE"
}

