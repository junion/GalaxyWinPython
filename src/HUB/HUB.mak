# Microsoft Developer Studio Generated NMAKE File, Based on HUB.dsp
!IF "$(CFG)" == ""
CFG=HUB - Win32 Debug
!MESSAGE No configuration specified. Defaulting to HUB - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "HUB - Win32 Release" && "$(CFG)" != "HUB - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "HUB.mak" CFG="HUB - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "HUB - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "HUB - Win32 Debug" (based on "Win32 (x86) Console Application")
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

!IF  "$(CFG)" == "HUB - Win32 Release"

OUTDIR=.\../../bin/x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\../../bin/x86-nt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\HUB.exe"

!ELSE 

ALL : "libGalaxyHubControl - Win32 Release" "libGalaxy - Win32 Release" "$(OUTDIR)\HUB.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libGalaxy - Win32 ReleaseCLEAN" "libGalaxyHubControl - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\alarm.obj"
	-@erase "$(INTDIR)\broker_proxy.obj"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\db.obj"
	-@erase "$(INTDIR)\external_api.obj"
	-@erase "$(INTDIR)\external_runtime_api.obj"
	-@erase "$(INTDIR)\hub.obj"
	-@erase "$(INTDIR)\hub_init.obj"
	-@erase "$(INTDIR)\hub_namespace.obj"
	-@erase "$(INTDIR)\hub_process.obj"
	-@erase "$(INTDIR)\hub_report_status.obj"
	-@erase "$(INTDIR)\hub_special.obj"
	-@erase "$(INTDIR)\hub_util.obj"
	-@erase "$(INTDIR)\logfile.obj"
	-@erase "$(INTDIR)\session.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\HUB.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "../../include" /I "../local-include" /I "../../templates/x86-nt" /D "NDEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /Fp"$(INTDIR)\HUB.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\HUB.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\HUB.pdb" /machine:I386 /out:"$(OUTDIR)\HUB.exe" /libpath:"../../lib/x86-nt" /libpath:"../../contrib/MIT/galaxy/lib/x86-nt" 
LINK32_OBJS= \
	"$(INTDIR)\alarm.obj" \
	"$(INTDIR)\broker_proxy.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\db.obj" \
	"$(INTDIR)\external_api.obj" \
	"$(INTDIR)\external_runtime_api.obj" \
	"$(INTDIR)\hub.obj" \
	"$(INTDIR)\hub_init.obj" \
	"$(INTDIR)\hub_namespace.obj" \
	"$(INTDIR)\hub_process.obj" \
	"$(INTDIR)\hub_report_status.obj" \
	"$(INTDIR)\hub_special.obj" \
	"$(INTDIR)\hub_util.obj" \
	"$(INTDIR)\logfile.obj" \
	"$(INTDIR)\session.obj" \
	"..\..\lib\x86-nt\libGalaxy.lib" \
	"..\..\contrib\MIT\galaxy\lib\x86-nt\libGalaxyHubControl.lib"

"$(OUTDIR)\HUB.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"

OUTDIR=.\../../bin/x86-nt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\../../bin/x86-nt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\HUB_debug.exe" "$(OUTDIR)\HUB.bsc"

!ELSE 

ALL : "libGalaxyHubControl - Win32 Debug" "libGalaxy - Win32 Debug" "$(OUTDIR)\HUB_debug.exe" "$(OUTDIR)\HUB.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libGalaxy - Win32 DebugCLEAN" "libGalaxyHubControl - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\alarm.obj"
	-@erase "$(INTDIR)\alarm.sbr"
	-@erase "$(INTDIR)\broker_proxy.obj"
	-@erase "$(INTDIR)\broker_proxy.sbr"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\builtin.sbr"
	-@erase "$(INTDIR)\db.obj"
	-@erase "$(INTDIR)\db.sbr"
	-@erase "$(INTDIR)\external_api.obj"
	-@erase "$(INTDIR)\external_api.sbr"
	-@erase "$(INTDIR)\external_runtime_api.obj"
	-@erase "$(INTDIR)\external_runtime_api.sbr"
	-@erase "$(INTDIR)\hub.obj"
	-@erase "$(INTDIR)\hub.sbr"
	-@erase "$(INTDIR)\hub_init.obj"
	-@erase "$(INTDIR)\hub_init.sbr"
	-@erase "$(INTDIR)\hub_namespace.obj"
	-@erase "$(INTDIR)\hub_namespace.sbr"
	-@erase "$(INTDIR)\hub_process.obj"
	-@erase "$(INTDIR)\hub_process.sbr"
	-@erase "$(INTDIR)\hub_report_status.obj"
	-@erase "$(INTDIR)\hub_report_status.sbr"
	-@erase "$(INTDIR)\hub_special.obj"
	-@erase "$(INTDIR)\hub_special.sbr"
	-@erase "$(INTDIR)\hub_util.obj"
	-@erase "$(INTDIR)\hub_util.sbr"
	-@erase "$(INTDIR)\logfile.obj"
	-@erase "$(INTDIR)\logfile.sbr"
	-@erase "$(INTDIR)\session.obj"
	-@erase "$(INTDIR)\session.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\HUB.bsc"
	-@erase "$(OUTDIR)\HUB_debug.exe"
	-@erase "$(OUTDIR)\HUB_debug.ilk"
	-@erase "$(OUTDIR)\HUB_debug.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "../../include" /I "../local-include" /I "../../templates/x86-nt" /D "_DEBUG" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\HUB.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\HUB.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\alarm.sbr" \
	"$(INTDIR)\broker_proxy.sbr" \
	"$(INTDIR)\builtin.sbr" \
	"$(INTDIR)\db.sbr" \
	"$(INTDIR)\external_api.sbr" \
	"$(INTDIR)\external_runtime_api.sbr" \
	"$(INTDIR)\hub.sbr" \
	"$(INTDIR)\hub_init.sbr" \
	"$(INTDIR)\hub_namespace.sbr" \
	"$(INTDIR)\hub_process.sbr" \
	"$(INTDIR)\hub_report_status.sbr" \
	"$(INTDIR)\hub_special.sbr" \
	"$(INTDIR)\hub_util.sbr" \
	"$(INTDIR)\logfile.sbr" \
	"$(INTDIR)\session.sbr"

"$(OUTDIR)\HUB.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\HUB_debug.pdb" /debug /machine:I386 /out:"$(OUTDIR)\HUB_debug.exe" /pdbtype:sept /libpath:"../../lib/x86-nt" /libpath:"../../contrib/MIT/galaxy/lib/x86-nt" 
LINK32_OBJS= \
	"$(INTDIR)\alarm.obj" \
	"$(INTDIR)\broker_proxy.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\db.obj" \
	"$(INTDIR)\external_api.obj" \
	"$(INTDIR)\external_runtime_api.obj" \
	"$(INTDIR)\hub.obj" \
	"$(INTDIR)\hub_init.obj" \
	"$(INTDIR)\hub_namespace.obj" \
	"$(INTDIR)\hub_process.obj" \
	"$(INTDIR)\hub_report_status.obj" \
	"$(INTDIR)\hub_special.obj" \
	"$(INTDIR)\hub_util.obj" \
	"$(INTDIR)\logfile.obj" \
	"$(INTDIR)\session.obj" \
	"..\..\lib\x86-nt\libGalaxy_debug.lib" \
	"..\..\contrib\MIT\galaxy\lib\x86-nt\libGalaxyHubControl_debug.lib"

"$(OUTDIR)\HUB_debug.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("HUB.dep")
!INCLUDE "HUB.dep"
!ELSE 
!MESSAGE Warning: cannot find "HUB.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "HUB - Win32 Release" || "$(CFG)" == "HUB - Win32 Debug"
SOURCE=.\alarm.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\alarm.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\alarm.obj"	"$(INTDIR)\alarm.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=..\libGalaxy\ServerStub\broker_proxy.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\broker_proxy.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\broker_proxy.obj"	"$(INTDIR)\broker_proxy.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\builtin.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\builtin.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\builtin.obj"	"$(INTDIR)\builtin.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\db.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\db.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\db.obj"	"$(INTDIR)\db.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\external_api.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\external_api.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\external_api.obj"	"$(INTDIR)\external_api.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\external_runtime_api.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\external_runtime_api.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\external_runtime_api.obj"	"$(INTDIR)\external_runtime_api.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\hub.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\hub.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\hub.obj"	"$(INTDIR)\hub.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\hub_init.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\hub_init.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\hub_init.obj"	"$(INTDIR)\hub_init.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\hub_namespace.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\hub_namespace.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\hub_namespace.obj"	"$(INTDIR)\hub_namespace.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\hub_process.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\hub_process.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\hub_process.obj"	"$(INTDIR)\hub_process.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\hub_report_status.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\hub_report_status.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\hub_report_status.obj"	"$(INTDIR)\hub_report_status.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\hub_special.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\hub_special.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\hub_special.obj"	"$(INTDIR)\hub_special.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\hub_util.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\hub_util.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\hub_util.obj"	"$(INTDIR)\hub_util.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\logfile.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\logfile.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\logfile.obj"	"$(INTDIR)\logfile.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\session.c

!IF  "$(CFG)" == "HUB - Win32 Release"


"$(INTDIR)\session.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"


"$(INTDIR)\session.obj"	"$(INTDIR)\session.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

!IF  "$(CFG)" == "HUB - Win32 Release"

"libGalaxy - Win32 Release" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Release" 
   cd "..\HUB"

"libGalaxy - Win32 ReleaseCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Release" RECURSE=1 CLEAN 
   cd "..\HUB"

!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"

"libGalaxy - Win32 Debug" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Debug" 
   cd "..\HUB"

"libGalaxy - Win32 DebugCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\src\libGalaxy"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxy.mak CFG="libGalaxy - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\HUB"

!ENDIF 

!IF  "$(CFG)" == "HUB - Win32 Release"

"libGalaxyHubControl - Win32 Release" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MIT\galaxy\src\libGalaxyHubControl"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxyHubControl.mak CFG="libGalaxyHubControl - Win32 Release" 
   cd "..\..\..\..\..\src\HUB"

"libGalaxyHubControl - Win32 ReleaseCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MIT\galaxy\src\libGalaxyHubControl"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxyHubControl.mak CFG="libGalaxyHubControl - Win32 Release" RECURSE=1 CLEAN 
   cd "..\..\..\..\..\src\HUB"

!ELSEIF  "$(CFG)" == "HUB - Win32 Debug"

"libGalaxyHubControl - Win32 Debug" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MIT\galaxy\src\libGalaxyHubControl"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxyHubControl.mak CFG="libGalaxyHubControl - Win32 Debug" 
   cd "..\..\..\..\..\src\HUB"

"libGalaxyHubControl - Win32 DebugCLEAN" : 
   cd "\Communicator\GalaxyCommunicator2\contrib\MIT\galaxy\src\libGalaxyHubControl"
   $(MAKE) /$(MAKEFLAGS) /F .\libGalaxyHubControl.mak CFG="libGalaxyHubControl - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\..\..\..\..\src\HUB"

!ENDIF 


!ENDIF 

