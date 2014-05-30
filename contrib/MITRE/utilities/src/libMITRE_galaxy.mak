# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Microsoft Developer Studio Generated NMAKE File, Based on libMITRE_galaxy.dsp
!IF "$(CFG)" == ""
CFG=libMITRE_galaxy - Win32 Debug
!MESSAGE No configuration specified. Defaulting to libMITRE_galaxy - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "libMITRE_galaxy - Win32 Release" && "$(CFG)" != "libMITRE_galaxy - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libMITRE_galaxy.mak" CFG="libMITRE_galaxy - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libMITRE_galaxy - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libMITRE_galaxy - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libMITRE_galaxy - Win32 Release"

OUTDIR=.\../lib/x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\../lib/x86-nt/
# End Custom Macros

ALL : "$(OUTDIR)\libMITRE_galaxy.lib"


CLEAN :
	-@erase "$(INTDIR)\binary_utility.obj"
	-@erase "$(INTDIR)\broker_utility.obj"
	-@erase "$(INTDIR)\frame_utility.obj"
	-@erase "$(INTDIR)\mgutil_init.obj"
	-@erase "$(INTDIR)\stdin_utility.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\libMITRE_galaxy.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "..\..\..\..\include" /I "..\include" /I "..\..\..\..\templates\x86-nt" /D "NDEBUG" /D "_WINDOWS" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_MBCS" /D "_REENTRANT" /Fp"$(INTDIR)\libMITRE_galaxy.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libMITRE_galaxy.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libMITRE_galaxy.lib" 
LIB32_OBJS= \
	"$(INTDIR)\binary_utility.obj" \
	"$(INTDIR)\broker_utility.obj" \
	"$(INTDIR)\frame_utility.obj" \
	"$(INTDIR)\mgutil_init.obj" \
	"$(INTDIR)\stdin_utility.obj"

"$(OUTDIR)\libMITRE_galaxy.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libMITRE_galaxy - Win32 Debug"

OUTDIR=.\../lib/x86-nt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\../lib/x86-nt/
# End Custom Macros

ALL : "$(OUTDIR)\libMITRE_galaxy_debug.lib" "$(OUTDIR)\libMITRE_galaxy.bsc"


CLEAN :
	-@erase "$(INTDIR)\binary_utility.obj"
	-@erase "$(INTDIR)\binary_utility.sbr"
	-@erase "$(INTDIR)\broker_utility.obj"
	-@erase "$(INTDIR)\broker_utility.sbr"
	-@erase "$(INTDIR)\frame_utility.obj"
	-@erase "$(INTDIR)\frame_utility.sbr"
	-@erase "$(INTDIR)\mgutil_init.obj"
	-@erase "$(INTDIR)\mgutil_init.sbr"
	-@erase "$(INTDIR)\stdin_utility.obj"
	-@erase "$(INTDIR)\stdin_utility.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\libMITRE_galaxy.bsc"
	-@erase "$(OUTDIR)\libMITRE_galaxy_debug.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\include" /I "..\include" /I "..\..\..\..\templates\x86-nt" /D "_DEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_MBCS" /D "_REENTRANT" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\libMITRE_galaxy.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libMITRE_galaxy.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\binary_utility.sbr" \
	"$(INTDIR)\broker_utility.sbr" \
	"$(INTDIR)\frame_utility.sbr" \
	"$(INTDIR)\mgutil_init.sbr" \
	"$(INTDIR)\stdin_utility.sbr"

"$(OUTDIR)\libMITRE_galaxy.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libMITRE_galaxy_debug.lib" 
LIB32_OBJS= \
	"$(INTDIR)\binary_utility.obj" \
	"$(INTDIR)\broker_utility.obj" \
	"$(INTDIR)\frame_utility.obj" \
	"$(INTDIR)\mgutil_init.obj" \
	"$(INTDIR)\stdin_utility.obj"

"$(OUTDIR)\libMITRE_galaxy_debug.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("libMITRE_galaxy.dep")
!INCLUDE "libMITRE_galaxy.dep"
!ELSE 
!MESSAGE Warning: cannot find "libMITRE_galaxy.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libMITRE_galaxy - Win32 Release" || "$(CFG)" == "libMITRE_galaxy - Win32 Debug"
SOURCE=.\binary_utility.c

!IF  "$(CFG)" == "libMITRE_galaxy - Win32 Release"


"$(INTDIR)\binary_utility.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libMITRE_galaxy - Win32 Debug"


"$(INTDIR)\binary_utility.obj"	"$(INTDIR)\binary_utility.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\broker_utility.c

!IF  "$(CFG)" == "libMITRE_galaxy - Win32 Release"


"$(INTDIR)\broker_utility.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libMITRE_galaxy - Win32 Debug"


"$(INTDIR)\broker_utility.obj"	"$(INTDIR)\broker_utility.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\frame_utility.c

!IF  "$(CFG)" == "libMITRE_galaxy - Win32 Release"


"$(INTDIR)\frame_utility.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libMITRE_galaxy - Win32 Debug"


"$(INTDIR)\frame_utility.obj"	"$(INTDIR)\frame_utility.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\mgutil_init.c

!IF  "$(CFG)" == "libMITRE_galaxy - Win32 Release"


"$(INTDIR)\mgutil_init.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libMITRE_galaxy - Win32 Debug"


"$(INTDIR)\mgutil_init.obj"	"$(INTDIR)\mgutil_init.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\stdin_utility.c

!IF  "$(CFG)" == "libMITRE_galaxy - Win32 Release"


"$(INTDIR)\stdin_utility.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libMITRE_galaxy - Win32 Debug"


"$(INTDIR)\stdin_utility.obj"	"$(INTDIR)\stdin_utility.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 

