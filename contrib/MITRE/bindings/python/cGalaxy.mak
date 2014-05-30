# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Microsoft Developer Studio Generated NMAKE File, Based on cGalaxy.dsp
!IF "$(CFG)" == ""
CFG=cGalaxy - Win32 Debug
!MESSAGE No configuration specified. Defaulting to cGalaxy - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "cGalaxy - Win32 Release" && "$(CFG)" != "cGalaxy - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "cGalaxy.mak" CFG="cGalaxy - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "cGalaxy - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "cGalaxy - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "cGalaxy - Win32 Release"

OUTDIR=.\x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\x86-nt
# End Custom Macros

ALL : "$(OUTDIR)\cGalaxy.dll"


CLEAN :
	-@erase "$(INTDIR)\cGalaxy_wrap.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\cGalaxy.dll"
	-@erase "$(OUTDIR)\cGalaxy.exp"
	-@erase "$(OUTDIR)\cGalaxy.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "C:\Python22\include" /I "..\..\..\..\include" /I "..\binding_support" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "CGALAXY_EXPORTS" /Fp"$(INTDIR)\cGalaxy.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\cGalaxy.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\cGalaxy.pdb" /machine:I386 /out:"$(OUTDIR)\cGalaxy.dll" /implib:"$(OUTDIR)\cGalaxy.lib" /libpath:"../../../../lib/x86-nt" /libpath:"C:/Python22/libs" 
LINK32_OBJS= \
	"$(INTDIR)\cGalaxy_wrap.obj"

"$(OUTDIR)\cGalaxy.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "cGalaxy - Win32 Debug"

OUTDIR=.\x86-nt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\x86-nt
# End Custom Macros

ALL : "$(OUTDIR)\cGalaxy_debug.dll"


CLEAN :
	-@erase "$(INTDIR)\cGalaxy_wrap.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\cGalaxy_debug.dll"
	-@erase "$(OUTDIR)\cGalaxy_debug.exp"
	-@erase "$(OUTDIR)\cGalaxy_debug.ilk"
	-@erase "$(OUTDIR)\cGalaxy_debug.lib"
	-@erase "$(OUTDIR)\cGalaxy_debug.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "C:\Python22\include" /I "..\..\..\..\include" /I "..\binding_support" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "CGALAXY_EXPORTS" /Fp"$(INTDIR)\cGalaxy.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\cGalaxy.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\cGalaxy_debug.pdb" /debug /machine:I386 /out:"$(OUTDIR)\cGalaxy_debug.dll" /implib:"$(OUTDIR)\cGalaxy_debug.lib" /pdbtype:sept /libpath:"../../../../lib/x86-nt" /libpath:"C:/Python22/libs" 
LINK32_OBJS= \
	"$(INTDIR)\cGalaxy_wrap.obj"

"$(OUTDIR)\cGalaxy_debug.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("cGalaxy.dep")
!INCLUDE "cGalaxy.dep"
!ELSE 
!MESSAGE Warning: cannot find "cGalaxy.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "cGalaxy - Win32 Release" || "$(CFG)" == "cGalaxy - Win32 Debug"
SOURCE=.\cGalaxy_wrap.c

"$(INTDIR)\cGalaxy_wrap.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

