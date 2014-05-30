#!/bin/csh -f
#
# Portions of this file (c) Copyright 1998 - 2002 The MITRE Corporation
# Portions of this file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

if ($#argv > 2) then
  echo "Usage: set_os.csh [os [version]]"
  exit 1
endif

if ($#argv == 2) then
  set version = $argv[2]
else
  set version = `uname -r`
endif

if ($#argv > 0) then
  set os = $argv[1]
else
  set os = `uname -s`
endif

switch($os)
  case SunOS:
    switch($version)
      case 4.1*:
	setenv OS sunos
      breaksw
      case 5*:
	setenv OS solaris
      breaksw
    endsw
  breaksw
  case OSF*:
    setenv OS osf
  breaksw
  case HP-UX:
    setenv OS hpux
  breaksw
  case IRIX*:
    setenv OS irix
  breaksw
  case Linux:
    setenv OS linux
  breaksw
  default:
    setenv OS $os
  breaksw
endsw

unset os
unset version

echo $OS

# for Emacs...
# Local Variables:
# mode: csh
# comment-column: 40
# fill-column: 77
# csh-indent-level: 2
# End:
