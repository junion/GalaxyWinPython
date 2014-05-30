# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Microsoft Developer Studio Generated NMAKE File, Based on Backend.dsp
!IF "$(CFG)" == ""
CFG=Backend - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Backend - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Backend - Win32 Release" && "$(CFG)" != "Backend - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Backend.mak" CFG="Backend - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Backend - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Backend - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "Backend - Win32 Release"

OUTDIR=.\./bin/x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\./bin/x86-nt
# End Custom Macros

ALL : "$(OUTDIR)\Backend.exe"


CLEAN :
	-@erase "$(INTDIR)\Backend.obj"
	-@erase "$(INTDIR)\component_engine.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\Backend.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "../../../../include" /I "../../../../templates/x86-nt" /I "../../utilities/include" /D "NDEBUG" /D "__STDC__" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /Fp"$(INTDIR)\Backend.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Backend.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=libMITRE_galaxy.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ws2_32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\Backend.pdb" /machine:I386 /out:"$(OUTDIR)\Backend.exe" /libpath:"../../utilities/lib/x86-nt" /libpath:"../../../../lib/x86-nt" 
LINK32_OBJS= \
	"$(INTDIR)\Backend.obj" \
	"$(INTDIR)\component_engine.obj"

"$(OUTDIR)\Backend.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Backend - Win32 Debug"

OUTDIR=.\./bin/x86-nt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\./bin/x86-nt
# End Custom Macros

ALL : "$(OUTDIR)\Backend_debug.exe" "$(OUTDIR)\Backend.bsc"


CLEAN :
	-@erase "$(INTDIR)\Backend.obj"
	-@erase "$(INTDIR)\Backend.sbr"
	-@erase "$(INTDIR)\component_engine.obj"
	-@erase "$(INTDIR)\component_engine.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\Backend.bsc"
	-@erase "$(OUTDIR)\Backend_debug.exe"
	-@erase "$(OUTDIR)\Backend_debug.ilk"
	-@erase "$(OUTDIR)\Backend_debug.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "../../../../include" /I "../../../../templates/x86-nt" /I "../../utilities/include" /D "_DEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\Backend.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Backend.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\Backend.sbr" \
	"$(INTDIR)\component_engine.sbr"

"$(OUTDIR)\Backend.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=libMITRE_galaxy_debug.lib ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\Backend_debug.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Backend_debug.exe" /pdbtype:sept /libpath:"../../utilities/lib/x86-nt" /libpath:"../../../../lib/x86-nt" 
LINK32_OBJS= \
	"$(INTDIR)\Backend.obj" \
	"$(INTDIR)\component_engine.obj"

"$(OUTDIR)\Backend_debug.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("Backend.dep")
!INCLUDE "Backend.dep"
!ELSE 
!MESSAGE Warning: cannot find "Backend.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Backend - Win32 Release" || "$(CFG)" == "Backend - Win32 Debug"
SOURCE=.\Backend.c

!IF  "$(CFG)" == "Backend - Win32 Release"


"$(INTDIR)\Backend.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Backend - Win32 Debug"


"$(INTDIR)\Backend.obj"	"$(INTDIR)\Backend.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\component_engine.c

!IF  "$(CFG)" == "Backend - Win32 Release"


"$(INTDIR)\component_engine.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Backend - Win32 Debug"


"$(INTDIR)\component_engine.obj"	"$(INTDIR)\component_engine.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 

