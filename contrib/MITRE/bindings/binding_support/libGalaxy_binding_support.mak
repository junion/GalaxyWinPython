# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Microsoft Developer Studio Generated NMAKE File, Based on libGalaxy_binding_support.dsp
!IF "$(CFG)" == ""
CFG=libGalaxy_binding_support - Win32 Debug
!MESSAGE No configuration specified. Defaulting to libGalaxy_binding_support - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "libGalaxy_binding_support - Win32 Release" && "$(CFG)" != "libGalaxy_binding_support - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libGalaxy_binding_support.mak" CFG="libGalaxy_binding_support - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libGalaxy_binding_support - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libGalaxy_binding_support - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "libGalaxy_binding_support - Win32 Release"

OUTDIR=.\../lib/x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\../lib/x86-nt/
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\libGalaxy_binding_support.lib"

!ELSE 

ALL : "libGalaxy - Win32 Release" "$(OUTDIR)\libGalaxy_binding_support.lib"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libGalaxy - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\binding_support.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\libGalaxy_binding_support.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "..\..\..\..\include" /I "..\include" /I "..\..\..\..\templates\x86-nt" /D "NDEBUG" /D "_WINDOWS" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_MBCS" /D "_REENTRANT" /Fp"$(INTDIR)\libGalaxy_binding_support.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libGalaxy_binding_support.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libGalaxy_binding_support.lib" 
LIB32_OBJS= \
	"$(INTDIR)\binding_support.obj" \
	"..\..\..\..\lib\x86-nt\libGalaxy.lib"

"$(OUTDIR)\libGalaxy_binding_support.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libGalaxy_binding_support - Win32 Debug"

OUTDIR=.\../lib/x86-nt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\../lib/x86-nt/
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\libGalaxy_binding_support_debug.lib" "$(OUTDIR)\libGalaxy_binding_support.bsc"

!ELSE 

ALL : "libGalaxy - Win32 Debug" "$(OUTDIR)\libGalaxy_binding_support_debug.lib" "$(OUTDIR)\libGalaxy_binding_support.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libGalaxy - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\binding_support.obj"
	-@erase "$(INTDIR)\binding_support.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\libGalaxy_binding_support.bsc"
	-@erase "$(OUTDIR)\libGalaxy_binding_support_debug.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\include" /I "..\include" /I "..\..\..\..\templates\x86-nt" /D "_DEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_MBCS" /D "_REENTRANT" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\libGalaxy_binding_support.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libGalaxy_binding_support.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\binding_support.sbr"

"$(OUTDIR)\libGalaxy_binding_support.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libGalaxy_binding_support_debug.lib" 
LIB32_OBJS= \
	"$(INTDIR)\binding_support.obj" \
	"..\..\..\..\lib\x86-nt\libGalaxy_debug.lib"

"$(OUTDIR)\libGalaxy_binding_support_debug.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("libGalaxy_binding_support.dep")
!INCLUDE "libGalaxy_binding_support.dep"
!ELSE 
!MESSAGE Warning: cannot find "libGalaxy_binding_support.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libGalaxy_binding_support - Win32 Release" || "$(CFG)" == "libGalaxy_binding_support - Win32 Debug"
SOURCE=.\binding_support.c

!IF  "$(CFG)" == "libGalaxy_binding_support - Win32 Release"


"$(INTDIR)\binding_support.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libGalaxy_binding_support - Win32 Debug"


"$(INTDIR)\binding_support.obj"	"$(INTDIR)\binding_support.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

!IF  "$(CFG)" == "libGalaxy_binding_support - Win32 Release"

"libGalaxy - Win32 Release" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Release" 
   cd "..\..\contrib\MITRE\bindings\binding_support"

"libGalaxy - Win32 ReleaseCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Release" RECURSE=1 CLEAN 
   cd "..\..\contrib\MITRE\bindings\binding_support"

!ELSEIF  "$(CFG)" == "libGalaxy_binding_support - Win32 Debug"

"libGalaxy - Win32 Debug" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Debug" 
   cd "..\..\contrib\MITRE\bindings\binding_support"

"libGalaxy - Win32 DebugCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\..\contrib\MITRE\bindings\binding_support"

!ENDIF 


!ENDIF 

