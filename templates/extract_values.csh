#!/bin/csh -f

if ( $#argv < 2 ) then
    echo "Usage: set_it.sh file shell_type val*"
    exit 1
endif

set file = $argv[1]

set shell_type = $argv[2]

set cmds = 

switch ($shell_type)
    case csh:
	foreach f ( $argv[3-] )
	    set sedscr = '/^'$f'/\!d'
	    set val = ( `sed $sedscr $file` )
	    set cmds = "$cmds setenv $val[1] '$val[2-]';"
	end
        breaksw
    case sh:
        foreach f ( $argv[3-] )
	    set sedscr = '/^'$f'/\!d'
	    set val = ( `sed $sedscr $file` )
	    set cmds = "$cmds export $val[1]; $val[1]='$val[2-]';"
        end
        breaksw
    default:
        echo "Unknown shell $shell_type"
	exit 1
endsw

echo $cmds
