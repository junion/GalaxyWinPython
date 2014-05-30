#!/bin/csh -f
#
# Portions of this file (c) Copyright 1998 - 2002 The MITRE Corporation
# Portions of this file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

if ($#argv > 2) then
  echo "Usage: set_arch.csh [os [processor]]"
  exit 1
endif

if ($#argv == 2) then
  set proc = $argv[2]
else
  set proc = `uname -m`
endif

if ($#argv > 0) then
  set os = $argv[1]
else
  set os = `uname -s`
endif

# Darwin -m returns "Power Macintosh", which needs to be
# converted to a symbol. uname -p returns powerpc, so
# that's what we use. See below.

# Collapse all things of form sun4{m,u,etc} to sparc. We should
# really use 'uname -p', but sunos4.* uname doesn't support -p,
# nor does linux 2.0.*, nor does OSF1 3.*, and the gyrations to
# deal with it aren't worth the effort.

# SAM 8/17/99: sadly, uname -s and uname -m seem to do something
# odd on Irix; -s returns IRIX or IRIX64 (an important arch distinction)
# while -m returns IP22, IP27, etc., which don't make any arch
# distinctions. 

if ("$proc" == "Power Macintosh") then
  set proc = `uname -p`
endif

if ($os == SunOS) then
  switch($proc)
  case sun4*:
    setenv ARCH sparc
    breaksw
  case i86pc:
    setenv ARCH x86
    breaksw
  endsw
else
  switch($proc)
  case i86pc:
    setenv ARCH x86
    breaksw
  case i?86:
    setenv ARCH x86
    breaksw
  case IP*:
    switch($os)
    case IRIX64:
       setenv ARCH mips64
       breaksw
    default:
       setenv ARCH mips
       breaksw
    endsw
    breaksw
  default:
    setenv ARCH $proc
  endsw
endif

unset os
unset proc

echo $ARCH

# for Emacs...
# Local Variables:
# mode: csh
# comment-column: 40
# fill-column: 77
# csh-indent-level: 2
# End:
