#!/bin/csh -f

# The arguments will look like this:

# archos num-substs subname-1 sub-1 ... target: source deps-w-\

# So we need to do a few things:

# (1) get rid of the slashes
# (2) replace archos with ARCHOS
# (3) replace the substs 
# (4) add TRUE_OBJDIR and the dir to the target

set archos = $argv[1]
@ numsubsts = $argv[2]
@ endsubsts = 2 + ( 2 * $numsubsts )
set substs = ( $argv[3-$endsubsts] )
@ eplusone = $endsubsts + 1
set target = $argv[$eplusone]
@ eplustwo = $endsubsts + 2
set source = $argv[$eplustwo]
@ eplusthree = $endsubsts + 3

# I can't just do a single substitution, because I can't both
# quote and not quote $ in the same literal.

set files = ( )

foreach f ( $argv[$eplusthree-$#argv] )
    if ( "\" != "$f" ) then 
        set sample = `echo $f | sed -e "s|/$archos/|/ARCHOS/|" -e 's|/ARCHOS/|/$(ARCHOS)/|'`
	set temp = ( $substs )
	while ( $#temp )
	    set repl = '$('$temp[1]')'
	    set sample = `echo $sample | sed -e "s|^$temp[2]|$repl|"`
	    shift temp
	    shift temp
	end
	set files = ( $files $sample )
    endif
end

if ( "$source:h" == "$source" ) then
    set dir = ""
else
    set dir = "$source:h"/
endif

echo '$(TRUE_OBJDIR)'"$dir"$target $source $files
echo
