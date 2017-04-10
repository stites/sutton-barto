#######################################################################
# Tcl/tk Tic-Tac-Toe player                                           #
#  written by Amy McGovern amy@cs.umass.edu 09/96                     #
#  permission given to modify the code as long as you keep            #
#   my name at the top                                                #
#######################################################################

#######################################################################
# this file contains all the gui related code                         #
#######################################################################

#######################################################################
# initialize the gui                                                  #
#######################################################################
proc init_gui {} {
    global board
    global info

    # setup a frame for the game and a frame for the info
    frame .game -width 200 -height 300 -relief sunken -bd 2
    frame .info -width 120 -height 300 
    pack .game .info -side left
    
    # set up the 3x3 grid by making 3 frames and then 3 buttons in each frame
    foreach name {top mid bot} {
	frame .game.$name -height 30 -width 200 -relief sunken -bd 2
    }
    pack .game.top .game.mid .game.bot -side top 
    
    # make the 3 buttons in each frame
    foreach row {{top ul uc ur} {mid cl cc cr} {bot ll lc lr}} {
	set f .game.[lindex $row 0]
	foreach pos [lrange $row 1 3] {
	    button $f.$pos -width 3 -height 2 -textvariable board($pos) \
		-bd 2 -font -*-times-*-*-*-*-40-*-*-*-*-*-*-* \
		-command "make_move $f.$pos $pos"
	}
	pack $f.[lindex $row 1] $f.[lindex $row 2] \
	    $f.[lindex $row 3] -side left
    }

    set info(color) [lindex [.game.top.ur configure -bg] 4]

    # make the restart and quit buttons and the score board
    button .info.restart -text "New Game" -bd 2 -command restart_game
    button .info.options -text "Options" -bd 2 -command Options
    button .info.viewv -text "View V" -bd 2 -command ViewV
    button .info.quit -text "Quit" -bd 2 -command Quit
    place .info.restart -relx 0 -rely 0
    place .info.options -relx 0.1 -rely 0.1
    place .info.viewv -relx 0.1 -rely 0.2
    place .info.quit -relx 0.2 -rely 0.3

    label .info.score -text "Score"
    place .info.score -relx 0.3 -rely 0.5
    label .info.x -text "X"
    label .info.o -text "TD"
    place .info.x -relx 0.5 -rely 0.6
    place .info.o -relx 0.7 -rely 0.6

    label .info.wins -text "Wins"
    label .info.loss -text "Losses"
    label .info.tie -text "Ties"
    place .info.wins -relx 0 -rely 0.7
    place .info.loss -relx 0 -rely 0.8
    place .info.tie -relx 0 -rely 0.9
    
    label .info.xwins -textvariable info(xwins)
    label .info.owins -textvariable info(owins)
    label .info.xloss -textvariable info(xloss)
    label .info.oloss -textvariable info(oloss)
    label .info.xtie -textvariable info(xtie)
    label .info.otie -textvariable info(otie)
    
    place .info.xwins -relx 0.5 -rely 0.7
    place .info.owins -relx 0.7 -rely 0.7
    place .info.xloss -relx 0.5 -rely 0.8
    place .info.oloss -relx 0.7 -rely 0.8
    place .info.xtie -relx 0.5 -rely 0.9
    place .info.otie -relx 0.7 -rely 0.9

}

#######################################################################
# highlights the given entries in the game board                      #
#######################################################################
proc highlight {p1 p2 p3} {
    .game$p1 configure -bg pink
    .game$p2 configure -bg pink
    .game$p3 configure -bg pink
}

#######################################################################
# Brings up the Options window                                        #
#######################################################################
proc Options {} {
    global info
    
    toplevel .opt
    frame .opt.top -height 100 -width 100
    frame .opt.sp1 -height 25 -width 100
    frame .opt.mid1 -height 100 -width 100
    frame .opt.sp2 -height 25 -width 100
    frame .opt.mid2 -height 100 -width 100
    frame .opt.sp3 -height 25 -width 100
    frame .opt.bot -height 25 -width 100
    frame .opt.sp4 -height 25 -width 100
    frame .opt.bot2 -height 25 -width 100
    pack .opt.top .opt.sp1 .opt.mid1 .opt.sp2 .opt.mid2 .opt.sp3 .opt.bot .opt.sp4 .opt.bot2 -side top

    label .opt.top.title -text "Training Opponent (X)"
    pack .opt.top.title
    radiobutton .opt.top.first -text "First Available Move" \
	-variable info(xplayer) -value "first_available_player"
    radiobutton .opt.top.rand -text "Random" -variable info(xplayer) \
	-value "random_player"
    radiobutton .opt.top.optimal -text "Optimal" -variable info(xplayer) \
	-value "optimal_player"
    radiobutton .opt.top.td -text "TD" -variable info(xplayer) \
	-value "td_player"
    radiobutton .opt.top.human -text "Human" -variable info(xplayer) \
	-value "human_player"
    pack .opt.top.first .opt.top.rand .opt.top.optimal .opt.top.td .opt.top.human -side left

    label .opt.mid1.title -text "Computer opponent (O)"
    pack .opt.mid1.title
    radiobutton .opt.mid1.tdon -text "TD - learning on" \
	-variable info(learning) -value "on"
    radiobutton .opt.mid1.tdoff -text "TD - learning off" \
	-variable info(learning) -value "off"
    pack .opt.mid1.tdon .opt.mid1.tdoff -side left

    label .opt.mid2.title -text "Training Iterations: "
    entry .opt.mid2.entry -textvariable info(iters)
    button .opt.mid2.train -text "Train" -command train_system
    pack .opt.mid2.title .opt.mid2.entry .opt.mid2.train -side left

    label .opt.bot.title -text "Load a V table from a file: "
    entry .opt.bot.entry -textvariable info(vfile)
    button .opt.bot.train -text "Load" -command load_v
    pack .opt.bot.title .opt.bot.entry .opt.bot.train -side left
    
    button .opt.bot2.close -text "Close" -command {destroy .opt}
    pack .opt.bot2.close
}

#######################################################################
# saves the current value table so it can be read back in             #
#######################################################################
proc Quit {} {
    global V
    global info
    global optimalV

    # save current training state
    set fname "V/$info(xplayer).[pid]"
    set fp [open $fname "w"]
    foreach index [array names V] {
	if {$V($index) != 0} {
	    puts $fp "set V($index) $V($index)"
	}
    }
    close $fp
    
    # save the optimal state
    set fname "V/optimalV.[pid]"
    set fp [open $fname "w+"]
    foreach index [array names optimalV] {
	puts $fp "set optimalV($index) $optimalV($index)"
    }
    close $fp

    # close the winning info (if it exists)
    if {[info exists info(fp)]} { close $info(fp)}

    exit
}
#######################################################################
# Shows you all the non zero values of V in a toplevel window         #
#######################################################################
proc ViewV {} {
    global V

    toplevel .v
    foreach index [array names V] {
	if {$V($index) != 0} {
	    regsub -all " " $index "B" newindex
	    set labname [string tolower $newindex]
	    set lab "[string range $newindex 0 2]\n[string range $newindex 3 5]\n[string range $newindex 6 8]\n"
	    label .v.$labname -text "$lab$V($index)" -font -*-courier-medium-r-*-*-10-*-*-*-*-*-*-* -relief sunken -bd 2
	    pack .v.$labname
	}
    }
    button .v.close -text "Close" -command {destroy .v}
    pack .v.close
}

#######################################################################
# loads a previously saved v into memory                              #
#######################################################################
proc load_v {} {
    global info
    global V

    source "/anw/amy/Classes/683/ass4/V/$info(vfile)"
    destroy .opt
}

#######################################################################
# restart the game and reset all game dependent vars                  #
#######################################################################
proc restart_game {} {
    global info

    set info(training) 0
    init_game
    play_game
}

#######################################################################
# see if the click was a valid move, if so move the user.  If not,    #
# ignore it (or generate an error message                             #
#######################################################################
proc make_move {path pos} {
    global board
    global info
    
    if {$board($pos) == " "} {
	set info(move) $pos
	set info(playermoved) 1
    }
}

