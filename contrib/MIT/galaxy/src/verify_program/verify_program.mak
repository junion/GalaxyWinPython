# Microsoft Developer Studio Generated NMAKE File, Based on verify_program.dsp
!IF "$(CFG)" == ""
CFG=verify_program - Win32 Debug
!MESSAGE No configuration specified. Defaulting to verify_program - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "verify_program - Win32 Release" && "$(CFG)" != "verify_program - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "verify_program.mak" CFG="verify_program - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "verify_program - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "verify_program - Win32 Debug" (based on "Win32 (x86) Console Application")
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

!IF  "$(CFG)" == "verify_program - Win32 Release"

OUTDIR=.\..\..\bin\x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\..\bin\x86-nt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\verify_program.exe"

!ELSE 

ALL : "libGalaxyHubControl - Win32 Release" "libGalaxy - Win32 Release" "$(OUTDIR)\verify_program.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libGalaxy - Win32 ReleaseCLEAN" "libGalaxyHubControl - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\alarm.obj"
	-@erase "$(INTDIR)\external_api.obj"
	-@erase "$(INTDIR)\hub_namespace.obj"
	-@erase "$(INTDIR)\print_hub_program.obj"
	-@erase "$(INTDIR)\read_hub_program.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\verify_program.obj"
	-@erase "$(OUTDIR)\verify_program.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "..\..\..\..\..\include" /I "..\..\..\..\..\src\HUB" /I "..\libGalaxyHubControl" /I "..\..\..\..\..\src\local-include" /I "../../../../../templates/x86-nt" /D "NDEBUG" /D "_REENTRANT" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\verify_program.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\verify_program.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\verify_program.pdb" /machine:I386 /out:"$(OUTDIR)\verify_program.exe" /libpath:"../../../../../lib/x86-nt" 
LINK32_OBJS= \
	"$(INTDIR)\alarm.obj" \
	"$(INTDIR)\external_api.obj" \
	"$(INTDIR)\hub_namespace.obj" \
	"$(INTDIR)\print_hub_program.obj" \
	"$(INTDIR)\read_hub_program.obj" \
	"$(INTDIR)\verify_program.obj" \
	"..\..\..\..\..\lib\x86-nt\libGalaxy.lib" \
	"..\..\lib\x86-nt\libGalaxyHubControl.lib"

"$(OUTDIR)\verify_program.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"

OUTDIR=.\..\..\bin\x86-nt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\..\..\bin\x86-nt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\verify_program_debug.exe" "$(OUTDIR)\verify_program.bsc"

!ELSE 

ALL : "libGalaxyHubControl - Win32 Debug" "libGalaxy - Win32 Debug" "$(OUTDIR)\verify_program_debug.exe" "$(OUTDIR)\verify_program.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libGalaxy - Win32 DebugCLEAN" "libGalaxyHubControl - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\alarm.obj"
	-@erase "$(INTDIR)\alarm.sbr"
	-@erase "$(INTDIR)\external_api.obj"
	-@erase "$(INTDIR)\external_api.sbr"
	-@erase "$(INTDIR)\hub_namespace.obj"
	-@erase "$(INTDIR)\hub_namespace.sbr"
	-@erase "$(INTDIR)\print_hub_program.obj"
	-@erase "$(INTDIR)\print_hub_program.sbr"
	-@erase "$(INTDIR)\read_hub_program.obj"
	-@erase "$(INTDIR)\read_hub_program.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\verify_program.obj"
	-@erase "$(INTDIR)\verify_program.sbr"
	-@erase "$(OUTDIR)\verify_program.bsc"
	-@erase "$(OUTDIR)\verify_program_debug.exe"
	-@erase "$(OUTDIR)\verify_program_debug.ilk"
	-@erase "$(OUTDIR)\verify_program_debug.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\include" /I "..\..\..\..\..\src\HUB" /I "..\libGalaxyHubControl" /I "..\..\..\..\..\src\local-include" /I "../../../../../templates/x86-nt" /D "_DEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\verify_program.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\verify_program.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\alarm.sbr" \
	"$(INTDIR)\external_api.sbr" \
	"$(INTDIR)\hub_namespace.sbr" \
	"$(INTDIR)\print_hub_program.sbr" \
	"$(INTDIR)\read_hub_program.sbr" \
	"$(INTDIR)\verify_program.sbr"

"$(OUTDIR)\verify_program.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\verify_program_debug.pdb" /debug /machine:I386 /out:"$(OUTDIR)\verify_program_debug.exe" /pdbtype:sept /libpath:"../../../../../lib/x86-nt" 
LINK32_OBJS= \
	"$(INTDIR)\alarm.obj" \
	"$(INTDIR)\external_api.obj" \
	"$(INTDIR)\hub_namespace.obj" \
	"$(INTDIR)\print_hub_program.obj" \
	"$(INTDIR)\read_hub_program.obj" \
	"$(INTDIR)\verify_program.obj" \
	"..\..\..\..\..\lib\x86-nt\libGalaxy_debug.lib" \
	"..\..\lib\x86-nt\libGalaxyHubControl_debug.lib"

"$(OUTDIR)\verify_program_debug.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("verify_program.dep")
!INCLUDE "verify_program.dep"
!ELSE 
!MESSAGE Warning: cannot find "verify_program.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "verify_program - Win32 Release" || "$(CFG)" == "verify_program - Win32 Debug"
SOURCE=..\..\..\..\..\src\HUB\alarm.c

!IF  "$(CFG)" == "verify_program - Win32 Release"


"$(INTDIR)\alarm.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"


"$(INTDIR)\alarm.obj"	"$(INTDIR)\alarm.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\..\..\..\src\HUB\external_api.c

!IF  "$(CFG)" == "verify_program - Win32 Release"


"$(INTDIR)\external_api.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"


"$(INTDIR)\external_api.obj"	"$(INTDIR)\external_api.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\..\..\..\src\HUB\hub_namespace.c

!IF  "$(CFG)" == "verify_program - Win32 Release"


"$(INTDIR)\hub_namespace.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"


"$(INTDIR)\hub_namespace.obj"	"$(INTDIR)\hub_namespace.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\libGalaxyHubControl\print_hub_program.c

!IF  "$(CFG)" == "verify_program - Win32 Release"


"$(INTDIR)\print_hub_program.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"


"$(INTDIR)\print_hub_program.obj"	"$(INTDIR)\print_hub_program.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\libGalaxyHubControl\read_hub_program.c

!IF  "$(CFG)" == "verify_program - Win32 Release"


"$(INTDIR)\read_hub_program.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"


"$(INTDIR)\read_hub_program.obj"	"$(INTDIR)\read_hub_program.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\verify_program.c

!IF  "$(CFG)" == "verify_program - Win32 Release"


"$(INTDIR)\verify_program.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"


"$(INTDIR)\verify_program.obj"	"$(INTDIR)\verify_program.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

!IF  "$(CFG)" == "verify_program - Win32 Release"

"libGalaxy - Win32 Release" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Release" 
   cd "..\..\contrib\MIT\galaxy\src\verify_program"

"libGalaxy - Win32 ReleaseCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Release" RECURSE=1 CLEAN 
   cd "..\..\contrib\MIT\galaxy\src\verify_program"

!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"

"libGalaxy - Win32 Debug" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Debug" 
   cd "..\..\contrib\MIT\galaxy\src\verify_program"

"libGalaxy - Win32 DebugCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\..\contrib\MIT\galaxy\src\verify_program"

!ENDIF 

!IF  "$(CFG)" == "verify_program - Win32 Release"

"libGalaxyHubControl - Win32 Release" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MIT\galaxy\src\libGalaxyHubControl"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxyHubControl.mak CFG="libGalaxyHubControl - Win32 Release" 
   cd "..\verify_program"

"libGalaxyHubControl - Win32 ReleaseCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MIT\galaxy\src\libGalaxyHubControl"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxyHubControl.mak CFG="libGalaxyHubControl - Win32 Release" RECURSE=1 CLEAN 
   cd "..\verify_program"

!ELSEIF  "$(CFG)" == "verify_program - Win32 Debug"

"libGalaxyHubControl - Win32 Debug" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MIT\galaxy\src\libGalaxyHubControl"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxyHubControl.mak CFG="libGalaxyHubControl - Win32 Debug" 
   cd "..\verify_program"

"libGalaxyHubControl - Win32 DebugCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MIT\galaxy\src\libGalaxyHubControl"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxyHubControl.mak CFG="libGalaxyHubControl - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\verify_program"

!ENDIF 


!ENDIF 

