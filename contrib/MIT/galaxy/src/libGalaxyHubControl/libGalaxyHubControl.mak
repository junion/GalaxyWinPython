# Microsoft Developer Studio Generated NMAKE File, Based on libGalaxyHubControl.dsp
!IF "$(CFG)" == ""
CFG=libGalaxyHubControl - Win32 Debug
!MESSAGE No configuration specified. Defaulting to libGalaxyHubControl - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "libGalaxyHubControl - Win32 Release" && "$(CFG)" != "libGalaxyHubControl - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libGalaxyHubControl.mak" CFG="libGalaxyHubControl - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libGalaxyHubControl - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libGalaxyHubControl - Win32 Debug" (based on "Win32 (x86) Static Library")
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

!IF  "$(CFG)" == "libGalaxyHubControl - Win32 Release"

OUTDIR=.\../../lib/x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\../../lib/x86-nt
# End Custom Macros

ALL : "$(OUTDIR)\libGalaxyHubControl.lib"


CLEAN :
	-@erase "$(INTDIR)\mit_control.obj"
	-@erase "$(INTDIR)\print_hub_program.obj"
	-@erase "$(INTDIR)\read_hub_program.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\libGalaxyHubControl.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "../../../../../include" /I "../../../../../src/local-include" /I "../../../../../src/HUB" /I "../../../../../templates/x86-nt" /D "NDEBUG" /D "_REENTRANT" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\libGalaxyHubControl.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libGalaxyHubControl.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libGalaxyHubControl.lib" 
LIB32_OBJS= \
	"$(INTDIR)\mit_control.obj" \
	"$(INTDIR)\print_hub_program.obj" \
	"$(INTDIR)\read_hub_program.obj"

"$(OUTDIR)\libGalaxyHubControl.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libGalaxyHubControl - Win32 Debug"

OUTDIR=.\../../lib/x86-nt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\../../lib/x86-nt
# End Custom Macros

ALL : "$(OUTDIR)\libGalaxyHubControl_debug.lib"


CLEAN :
	-@erase "$(INTDIR)\mit_control.obj"
	-@erase "$(INTDIR)\print_hub_program.obj"
	-@erase "$(INTDIR)\read_hub_program.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\libGalaxyHubControl_debug.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "../../../../../include" /I "../../../../../src/local-include" /I "../../../../../src/HUB" /I "../../../../../templates/x86-nt" /D "_DEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_REENTRANT" /Fp"$(INTDIR)\libGalaxyHubControl.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libGalaxyHubControl.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libGalaxyHubControl_debug.lib" 
LIB32_OBJS= \
	"$(INTDIR)\mit_control.obj" \
	"$(INTDIR)\print_hub_program.obj" \
	"$(INTDIR)\read_hub_program.obj"

"$(OUTDIR)\libGalaxyHubControl_debug.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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
!IF EXISTS("libGalaxyHubControl.dep")
!INCLUDE "libGalaxyHubControl.dep"
!ELSE 
!MESSAGE Warning: cannot find "libGalaxyHubControl.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libGalaxyHubControl - Win32 Release" || "$(CFG)" == "libGalaxyHubControl - Win32 Debug"
SOURCE=.\mit_control.c

"$(INTDIR)\mit_control.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\print_hub_program.c

"$(INTDIR)\print_hub_program.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\read_hub_program.c

"$(INTDIR)\read_hub_program.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

