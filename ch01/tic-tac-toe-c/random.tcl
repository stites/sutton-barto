#######################################################################
# Tcl/tk Tic-Tac-Toe player                                           #
#  written by Amy McGovern amy@cs.umass.edu 09/96                     #
#  permission given to modify the code as long as you keep            #
#   my name at the top                                                #
#######################################################################

#######################################################################
# this file contains code for random number generation                #
#######################################################################

#######################################################################
# seeds the random number generator (from K&R)                        #
#######################################################################
proc srand {seed} {
    global next
    puts "using seed $seed"
    
    set next $seed
}

#######################################################################
# random number generator (from K&R)                                  #
#######################################################################
proc rand {} {
    global next

    set next [expr $next * 1103515245 + 12345]
    return [expr [expr $next / 65536] % 32768]
}
