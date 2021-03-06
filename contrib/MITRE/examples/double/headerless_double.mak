# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Microsoft Developer Studio Generated NMAKE File, Based on headerless_double.dsp
!IF "$(CFG)" == ""
CFG=headerless_double - Win32 Debug
!MESSAGE No configuration specified. Defaulting to headerless_double - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "headerless_double - Win32 Release" && "$(CFG)" != "headerless_double - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "headerless_double.mak" CFG="headerless_double - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "headerless_double - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "headerless_double - Win32 Debug" (based on "Win32 (x86) Console Application")
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

!IF  "$(CFG)" == "headerless_double - Win32 Release"

OUTDIR=.\bin\x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\bin\x86-nt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\headerless_double.exe"

!ELSE 

ALL : "libMITRE_galaxy - Win32 Release" "libGalaxy - Win32 Release" "$(OUTDIR)\headerless_double.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libGalaxy - Win32 ReleaseCLEAN" "libMITRE_galaxy - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\double_core.obj"
	-@erase "$(INTDIR)\headerless_double.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\headerless_double.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "..\..\..\..\include" /D "NDEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /Fp"$(INTDIR)\headerless_double.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\headerless_double.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ws2_32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\headerless_double.pdb" /machine:I386 /out:"$(OUTDIR)\headerless_double.exe" /libpath:"../../../../lib/x86-nt" 
LINK32_OBJS= \
	"$(INTDIR)\double_core.obj" \
	"$(INTDIR)\headerless_double.obj" \
	"..\..\..\..\lib\x86-nt\libGalaxy.lib" \
	"..\..\utilities\lib\x86-nt\libMITRE_galaxy.lib"

"$(OUTDIR)\headerless_double.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "headerless_double - Win32 Debug"

OUTDIR=.\bin\x86-nt
INTDIR=.\headerless_double___Win32_Debug
# Begin Custom Macros
OutDir=.\bin\x86-nt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\headerless_double_debug.exe"

!ELSE 

ALL : "libMITRE_galaxy - Win32 Debug" "libGalaxy - Win32 Debug" "$(OUTDIR)\headerless_double_debug.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libGalaxy - Win32 DebugCLEAN" "libMITRE_galaxy - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\double_core.obj"
	-@erase "$(INTDIR)\headerless_double.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\headerless_double_debug.exe"
	-@erase "$(OUTDIR)\headerless_double_debug.ilk"
	-@erase "$(OUTDIR)\headerless_double_debug.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\include" /D "_DEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /Fp"$(INTDIR)\headerless_double.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\headerless_double.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\headerless_double_debug.pdb" /debug /machine:I386 /out:"$(OUTDIR)\headerless_double_debug.exe" /pdbtype:sept /libpath:"../../../../lib/x86-nt" 
LINK32_OBJS= \
	"$(INTDIR)\double_core.obj" \
	"$(INTDIR)\headerless_double.obj" \
	"..\..\..\..\lib\x86-nt\libGalaxy_debug.lib" \
	"..\..\utilities\lib\x86-nt\libMITRE_galaxy_debug.lib"

"$(OUTDIR)\headerless_double_debug.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
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
!IF EXISTS("headerless_double.dep")
!INCLUDE "headerless_double.dep"
!ELSE 
!MESSAGE Warning: cannot find "headerless_double.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "headerless_double - Win32 Release" || "$(CFG)" == "headerless_double - Win32 Debug"
SOURCE=.\double_core.c

"$(INTDIR)\double_core.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\headerless_double.c

"$(INTDIR)\headerless_double.obj" : $(SOURCE) "$(INTDIR)"


!IF  "$(CFG)" == "headerless_double - Win32 Release"

"libGalaxy - Win32 Release" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Release" 
   cd "..\..\contrib\MITRE\examples\double"

"libGalaxy - Win32 ReleaseCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Release" RECURSE=1 CLEAN 
   cd "..\..\contrib\MITRE\examples\double"

!ELSEIF  "$(CFG)" == "headerless_double - Win32 Debug"

"libGalaxy - Win32 Debug" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Debug" 
   cd "..\..\contrib\MITRE\examples\double"

"libGalaxy - Win32 DebugCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\..\contrib\MITRE\examples\double"

!ENDIF 

!IF  "$(CFG)" == "headerless_double - Win32 Release"

"libMITRE_galaxy - Win32 Release" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MITRE\utilities\src"
   $(MAKE) /$(MAKEFLAGS) /F .\libMITRE_galaxy.mak CFG="libMITRE_galaxy - Win32 Release" 
   cd "..\..\examples\double"

"libMITRE_galaxy - Win32 ReleaseCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MITRE\utilities\src"
   $(MAKE) /$(MAKEFLAGS) /F .\libMITRE_galaxy.mak CFG="libMITRE_galaxy - Win32 Release" RECURSE=1 CLEAN 
   cd "..\..\examples\double"

!ELSEIF  "$(CFG)" == "headerless_double - Win32 Debug"

"libMITRE_galaxy - Win32 Debug" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MITRE\utilities\src"
   $(MAKE) /$(MAKEFLAGS) /F .\libMITRE_galaxy.mak CFG="libMITRE_galaxy - Win32 Debug" 
   cd "..\..\examples\double"

"libMITRE_galaxy - Win32 DebugCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MITRE\utilities\src"
   $(MAKE) /$(MAKEFLAGS) /F .\libMITRE_galaxy.mak CFG="libMITRE_galaxy - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\..\examples\double"

!ENDIF 


!ENDIF 

