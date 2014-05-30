# Microsoft Developer Studio Generated NMAKE File, Based on libGalaxy.dsp
!IF "$(CFG)" == ""
CFG=libGalaxy - Win32 Debug
!MESSAGE No configuration specified. Defaulting to libGalaxy - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "libGalaxy - Win32 Release" && "$(CFG)" != "libGalaxy - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libGalaxy.mak" CFG="libGalaxy - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libGalaxy - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libGalaxy - Win32 Debug" (based on "Win32 (x86) Static Library")
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

!IF  "$(CFG)" == "libGalaxy - Win32 Release"

OUTDIR=.\..\..\lib\x86-nt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\..\lib\x86-nt
# End Custom Macros

ALL : "$(OUTDIR)\libGalaxy.lib"


CLEAN :
	-@erase "$(INTDIR)\bcopy.obj"
	-@erase "$(INTDIR)\binary_io.obj"
	-@erase "$(INTDIR)\broker_data.obj"
	-@erase "$(INTDIR)\broker_proxy.obj"
	-@erase "$(INTDIR)\continuation.obj"
	-@erase "$(INTDIR)\debug_memory.obj"
	-@erase "$(INTDIR)\dispatch_function.obj"
	-@erase "$(INTDIR)\dynamic_buffer.obj"
	-@erase "$(INTDIR)\error_tags.obj"
	-@erase "$(INTDIR)\frame_util.obj"
	-@erase "$(INTDIR)\galaxy_elr.obj"
	-@erase "$(INTDIR)\generic-server-main.obj"
	-@erase "$(INTDIR)\generic-server-toplevel.obj"
	-@erase "$(INTDIR)\generic-server.obj"
	-@erase "$(INTDIR)\gettimeofday.obj"
	-@erase "$(INTDIR)\grovel.obj"
	-@erase "$(INTDIR)\gthread.obj"
	-@erase "$(INTDIR)\gthread_init.obj"
	-@erase "$(INTDIR)\hub_server.obj"
	-@erase "$(INTDIR)\init_server_wrapper.obj"
	-@erase "$(INTDIR)\ip_util.obj"
	-@erase "$(INTDIR)\local_memory.obj"
	-@erase "$(INTDIR)\mkdirp.obj"
	-@erase "$(INTDIR)\name_barrier.obj"
	-@erase "$(INTDIR)\nfio.obj"
	-@erase "$(INTDIR)\nframe.obj"
	-@erase "$(INTDIR)\oa.obj"
	-@erase "$(INTDIR)\plist.obj"
	-@erase "$(INTDIR)\pointer_queue.obj"
	-@erase "$(INTDIR)\pr_util.obj"
	-@erase "$(INTDIR)\print_program.obj"
	-@erase "$(INTDIR)\print_usage.obj"
	-@erase "$(INTDIR)\program_tags.obj"
	-@erase "$(INTDIR)\random.obj"
	-@erase "$(INTDIR)\read_program.obj"
	-@erase "$(INTDIR)\server_locations.obj"
	-@erase "$(INTDIR)\signal.obj"
	-@erase "$(INTDIR)\sleep.obj"
	-@erase "$(INTDIR)\sls_verbose.obj"
	-@erase "$(INTDIR)\sls_verbose_color.obj"
	-@erase "$(INTDIR)\sock.obj"
	-@erase "$(INTDIR)\sockqueue.obj"
	-@erase "$(INTDIR)\srandom.obj"
	-@erase "$(INTDIR)\SS_default_init.obj"
	-@erase "$(INTDIR)\stream_util.obj"
	-@erase "$(INTDIR)\string_util.obj"
	-@erase "$(INTDIR)\strtok_r.obj"
	-@erase "$(INTDIR)\sym.obj"
	-@erase "$(INTDIR)\test_conditions.obj"
	-@erase "$(INTDIR)\timed_tasks.obj"
	-@erase "$(INTDIR)\tobj.obj"
	-@erase "$(INTDIR)\uucode.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vlist.obj"
	-@erase "$(INTDIR)\xdr.obj"
	-@erase "$(INTDIR)\xdr_array.obj"
	-@erase "$(INTDIR)\xdr_buffer.obj"
	-@erase "$(INTDIR)\xdr_float.obj"
	-@erase "$(INTDIR)\xdr_mem.obj"
	-@erase "$(INTDIR)\xdr_sizeof.obj"
	-@erase "$(OUTDIR)\libGalaxy.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "..\..\include" /I "..\local-include" /I "..\..\templates\x86-nt" /I ".\util\compat\win32" /D "NDEBUG" /D "_WINDOWS" /D "_REENTRANT" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_MBCS" /D "__STDC__" /Fp"$(INTDIR)\libGalaxy.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libGalaxy.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libGalaxy.lib" 
LIB32_OBJS= \
	"$(INTDIR)\bcopy.obj" \
	"$(INTDIR)\binary_io.obj" \
	"$(INTDIR)\broker_data.obj" \
	"$(INTDIR)\broker_proxy.obj" \
	"$(INTDIR)\continuation.obj" \
	"$(INTDIR)\debug_memory.obj" \
	"$(INTDIR)\dispatch_function.obj" \
	"$(INTDIR)\dynamic_buffer.obj" \
	"$(INTDIR)\error_tags.obj" \
	"$(INTDIR)\frame_util.obj" \
	"$(INTDIR)\galaxy_elr.obj" \
	"$(INTDIR)\generic-server-main.obj" \
	"$(INTDIR)\generic-server-toplevel.obj" \
	"$(INTDIR)\generic-server.obj" \
	"$(INTDIR)\gettimeofday.obj" \
	"$(INTDIR)\grovel.obj" \
	"$(INTDIR)\gthread.obj" \
	"$(INTDIR)\gthread_init.obj" \
	"$(INTDIR)\hub_server.obj" \
	"$(INTDIR)\init_server_wrapper.obj" \
	"$(INTDIR)\ip_util.obj" \
	"$(INTDIR)\local_memory.obj" \
	"$(INTDIR)\mkdirp.obj" \
	"$(INTDIR)\name_barrier.obj" \
	"$(INTDIR)\nfio.obj" \
	"$(INTDIR)\nframe.obj" \
	"$(INTDIR)\oa.obj" \
	"$(INTDIR)\plist.obj" \
	"$(INTDIR)\pointer_queue.obj" \
	"$(INTDIR)\pr_util.obj" \
	"$(INTDIR)\print_program.obj" \
	"$(INTDIR)\print_usage.obj" \
	"$(INTDIR)\program_tags.obj" \
	"$(INTDIR)\random.obj" \
	"$(INTDIR)\read_program.obj" \
	"$(INTDIR)\server_locations.obj" \
	"$(INTDIR)\signal.obj" \
	"$(INTDIR)\sleep.obj" \
	"$(INTDIR)\sls_verbose.obj" \
	"$(INTDIR)\sls_verbose_color.obj" \
	"$(INTDIR)\sock.obj" \
	"$(INTDIR)\sockqueue.obj" \
	"$(INTDIR)\srandom.obj" \
	"$(INTDIR)\SS_default_init.obj" \
	"$(INTDIR)\stream_util.obj" \
	"$(INTDIR)\string_util.obj" \
	"$(INTDIR)\strtok_r.obj" \
	"$(INTDIR)\sym.obj" \
	"$(INTDIR)\test_conditions.obj" \
	"$(INTDIR)\timed_tasks.obj" \
	"$(INTDIR)\tobj.obj" \
	"$(INTDIR)\uucode.obj" \
	"$(INTDIR)\vlist.obj" \
	"$(INTDIR)\xdr.obj" \
	"$(INTDIR)\xdr_array.obj" \
	"$(INTDIR)\xdr_buffer.obj" \
	"$(INTDIR)\xdr_float.obj" \
	"$(INTDIR)\xdr_mem.obj" \
	"$(INTDIR)\xdr_sizeof.obj"

"$(OUTDIR)\libGalaxy.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"

OUTDIR=.\..\..\lib\x86-nt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\..\..\lib\x86-nt
# End Custom Macros

ALL : "$(OUTDIR)\libGalaxy_debug.lib" "$(OUTDIR)\libGalaxy.bsc"


CLEAN :
	-@erase "$(INTDIR)\bcopy.obj"
	-@erase "$(INTDIR)\bcopy.sbr"
	-@erase "$(INTDIR)\binary_io.obj"
	-@erase "$(INTDIR)\binary_io.sbr"
	-@erase "$(INTDIR)\broker_data.obj"
	-@erase "$(INTDIR)\broker_data.sbr"
	-@erase "$(INTDIR)\broker_proxy.obj"
	-@erase "$(INTDIR)\broker_proxy.sbr"
	-@erase "$(INTDIR)\continuation.obj"
	-@erase "$(INTDIR)\continuation.sbr"
	-@erase "$(INTDIR)\debug_memory.obj"
	-@erase "$(INTDIR)\debug_memory.sbr"
	-@erase "$(INTDIR)\dispatch_function.obj"
	-@erase "$(INTDIR)\dispatch_function.sbr"
	-@erase "$(INTDIR)\dynamic_buffer.obj"
	-@erase "$(INTDIR)\dynamic_buffer.sbr"
	-@erase "$(INTDIR)\error_tags.obj"
	-@erase "$(INTDIR)\error_tags.sbr"
	-@erase "$(INTDIR)\frame_util.obj"
	-@erase "$(INTDIR)\frame_util.sbr"
	-@erase "$(INTDIR)\galaxy_elr.obj"
	-@erase "$(INTDIR)\galaxy_elr.sbr"
	-@erase "$(INTDIR)\generic-server-main.obj"
	-@erase "$(INTDIR)\generic-server-main.sbr"
	-@erase "$(INTDIR)\generic-server-toplevel.obj"
	-@erase "$(INTDIR)\generic-server-toplevel.sbr"
	-@erase "$(INTDIR)\generic-server.obj"
	-@erase "$(INTDIR)\generic-server.sbr"
	-@erase "$(INTDIR)\gettimeofday.obj"
	-@erase "$(INTDIR)\gettimeofday.sbr"
	-@erase "$(INTDIR)\grovel.obj"
	-@erase "$(INTDIR)\grovel.sbr"
	-@erase "$(INTDIR)\gthread.obj"
	-@erase "$(INTDIR)\gthread.sbr"
	-@erase "$(INTDIR)\gthread_init.obj"
	-@erase "$(INTDIR)\gthread_init.sbr"
	-@erase "$(INTDIR)\hub_server.obj"
	-@erase "$(INTDIR)\hub_server.sbr"
	-@erase "$(INTDIR)\init_server_wrapper.obj"
	-@erase "$(INTDIR)\init_server_wrapper.sbr"
	-@erase "$(INTDIR)\ip_util.obj"
	-@erase "$(INTDIR)\ip_util.sbr"
	-@erase "$(INTDIR)\local_memory.obj"
	-@erase "$(INTDIR)\local_memory.sbr"
	-@erase "$(INTDIR)\mkdirp.obj"
	-@erase "$(INTDIR)\mkdirp.sbr"
	-@erase "$(INTDIR)\name_barrier.obj"
	-@erase "$(INTDIR)\name_barrier.sbr"
	-@erase "$(INTDIR)\nfio.obj"
	-@erase "$(INTDIR)\nfio.sbr"
	-@erase "$(INTDIR)\nframe.obj"
	-@erase "$(INTDIR)\nframe.sbr"
	-@erase "$(INTDIR)\oa.obj"
	-@erase "$(INTDIR)\oa.sbr"
	-@erase "$(INTDIR)\plist.obj"
	-@erase "$(INTDIR)\plist.sbr"
	-@erase "$(INTDIR)\pointer_queue.obj"
	-@erase "$(INTDIR)\pointer_queue.sbr"
	-@erase "$(INTDIR)\pr_util.obj"
	-@erase "$(INTDIR)\pr_util.sbr"
	-@erase "$(INTDIR)\print_program.obj"
	-@erase "$(INTDIR)\print_program.sbr"
	-@erase "$(INTDIR)\print_usage.obj"
	-@erase "$(INTDIR)\print_usage.sbr"
	-@erase "$(INTDIR)\program_tags.obj"
	-@erase "$(INTDIR)\program_tags.sbr"
	-@erase "$(INTDIR)\random.obj"
	-@erase "$(INTDIR)\random.sbr"
	-@erase "$(INTDIR)\read_program.obj"
	-@erase "$(INTDIR)\read_program.sbr"
	-@erase "$(INTDIR)\server_locations.obj"
	-@erase "$(INTDIR)\server_locations.sbr"
	-@erase "$(INTDIR)\signal.obj"
	-@erase "$(INTDIR)\signal.sbr"
	-@erase "$(INTDIR)\sleep.obj"
	-@erase "$(INTDIR)\sleep.sbr"
	-@erase "$(INTDIR)\sls_verbose.obj"
	-@erase "$(INTDIR)\sls_verbose.sbr"
	-@erase "$(INTDIR)\sls_verbose_color.obj"
	-@erase "$(INTDIR)\sls_verbose_color.sbr"
	-@erase "$(INTDIR)\sock.obj"
	-@erase "$(INTDIR)\sock.sbr"
	-@erase "$(INTDIR)\sockqueue.obj"
	-@erase "$(INTDIR)\sockqueue.sbr"
	-@erase "$(INTDIR)\srandom.obj"
	-@erase "$(INTDIR)\srandom.sbr"
	-@erase "$(INTDIR)\SS_default_init.obj"
	-@erase "$(INTDIR)\SS_default_init.sbr"
	-@erase "$(INTDIR)\stream_util.obj"
	-@erase "$(INTDIR)\stream_util.sbr"
	-@erase "$(INTDIR)\string_util.obj"
	-@erase "$(INTDIR)\string_util.sbr"
	-@erase "$(INTDIR)\strtok_r.obj"
	-@erase "$(INTDIR)\strtok_r.sbr"
	-@erase "$(INTDIR)\sym.obj"
	-@erase "$(INTDIR)\sym.sbr"
	-@erase "$(INTDIR)\test_conditions.obj"
	-@erase "$(INTDIR)\test_conditions.sbr"
	-@erase "$(INTDIR)\timed_tasks.obj"
	-@erase "$(INTDIR)\timed_tasks.sbr"
	-@erase "$(INTDIR)\tobj.obj"
	-@erase "$(INTDIR)\tobj.sbr"
	-@erase "$(INTDIR)\uucode.obj"
	-@erase "$(INTDIR)\uucode.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\vlist.obj"
	-@erase "$(INTDIR)\vlist.sbr"
	-@erase "$(INTDIR)\xdr.obj"
	-@erase "$(INTDIR)\xdr.sbr"
	-@erase "$(INTDIR)\xdr_array.obj"
	-@erase "$(INTDIR)\xdr_array.sbr"
	-@erase "$(INTDIR)\xdr_buffer.obj"
	-@erase "$(INTDIR)\xdr_buffer.sbr"
	-@erase "$(INTDIR)\xdr_float.obj"
	-@erase "$(INTDIR)\xdr_float.sbr"
	-@erase "$(INTDIR)\xdr_mem.obj"
	-@erase "$(INTDIR)\xdr_mem.sbr"
	-@erase "$(INTDIR)\xdr_sizeof.obj"
	-@erase "$(INTDIR)\xdr_sizeof.sbr"
	-@erase "$(OUTDIR)\libGalaxy.bsc"
	-@erase "$(OUTDIR)\libGalaxy_debug.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\include" /I "..\local-include" /I "..\..\templates\x86-nt" /I ".\util\compat\win32" /D "_DEBUG" /D "_LIB" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_MBCS" /D "__STDC__" /D "_REENTRANT" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\libGalaxy.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libGalaxy.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\bcopy.sbr" \
	"$(INTDIR)\binary_io.sbr" \
	"$(INTDIR)\broker_data.sbr" \
	"$(INTDIR)\broker_proxy.sbr" \
	"$(INTDIR)\continuation.sbr" \
	"$(INTDIR)\debug_memory.sbr" \
	"$(INTDIR)\dispatch_function.sbr" \
	"$(INTDIR)\dynamic_buffer.sbr" \
	"$(INTDIR)\error_tags.sbr" \
	"$(INTDIR)\frame_util.sbr" \
	"$(INTDIR)\galaxy_elr.sbr" \
	"$(INTDIR)\generic-server-main.sbr" \
	"$(INTDIR)\generic-server-toplevel.sbr" \
	"$(INTDIR)\generic-server.sbr" \
	"$(INTDIR)\gettimeofday.sbr" \
	"$(INTDIR)\grovel.sbr" \
	"$(INTDIR)\gthread.sbr" \
	"$(INTDIR)\gthread_init.sbr" \
	"$(INTDIR)\hub_server.sbr" \
	"$(INTDIR)\init_server_wrapper.sbr" \
	"$(INTDIR)\ip_util.sbr" \
	"$(INTDIR)\local_memory.sbr" \
	"$(INTDIR)\mkdirp.sbr" \
	"$(INTDIR)\name_barrier.sbr" \
	"$(INTDIR)\nfio.sbr" \
	"$(INTDIR)\nframe.sbr" \
	"$(INTDIR)\oa.sbr" \
	"$(INTDIR)\plist.sbr" \
	"$(INTDIR)\pointer_queue.sbr" \
	"$(INTDIR)\pr_util.sbr" \
	"$(INTDIR)\print_program.sbr" \
	"$(INTDIR)\print_usage.sbr" \
	"$(INTDIR)\program_tags.sbr" \
	"$(INTDIR)\random.sbr" \
	"$(INTDIR)\read_program.sbr" \
	"$(INTDIR)\server_locations.sbr" \
	"$(INTDIR)\signal.sbr" \
	"$(INTDIR)\sleep.sbr" \
	"$(INTDIR)\sls_verbose.sbr" \
	"$(INTDIR)\sls_verbose_color.sbr" \
	"$(INTDIR)\sock.sbr" \
	"$(INTDIR)\sockqueue.sbr" \
	"$(INTDIR)\srandom.sbr" \
	"$(INTDIR)\SS_default_init.sbr" \
	"$(INTDIR)\stream_util.sbr" \
	"$(INTDIR)\string_util.sbr" \
	"$(INTDIR)\strtok_r.sbr" \
	"$(INTDIR)\sym.sbr" \
	"$(INTDIR)\test_conditions.sbr" \
	"$(INTDIR)\timed_tasks.sbr" \
	"$(INTDIR)\tobj.sbr" \
	"$(INTDIR)\uucode.sbr" \
	"$(INTDIR)\vlist.sbr" \
	"$(INTDIR)\xdr.sbr" \
	"$(INTDIR)\xdr_array.sbr" \
	"$(INTDIR)\xdr_buffer.sbr" \
	"$(INTDIR)\xdr_float.sbr" \
	"$(INTDIR)\xdr_mem.sbr" \
	"$(INTDIR)\xdr_sizeof.sbr"

"$(OUTDIR)\libGalaxy.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libGalaxy_debug.lib" 
LIB32_OBJS= \
	"$(INTDIR)\bcopy.obj" \
	"$(INTDIR)\binary_io.obj" \
	"$(INTDIR)\broker_data.obj" \
	"$(INTDIR)\broker_proxy.obj" \
	"$(INTDIR)\continuation.obj" \
	"$(INTDIR)\debug_memory.obj" \
	"$(INTDIR)\dispatch_function.obj" \
	"$(INTDIR)\dynamic_buffer.obj" \
	"$(INTDIR)\error_tags.obj" \
	"$(INTDIR)\frame_util.obj" \
	"$(INTDIR)\galaxy_elr.obj" \
	"$(INTDIR)\generic-server-main.obj" \
	"$(INTDIR)\generic-server-toplevel.obj" \
	"$(INTDIR)\generic-server.obj" \
	"$(INTDIR)\gettimeofday.obj" \
	"$(INTDIR)\grovel.obj" \
	"$(INTDIR)\gthread.obj" \
	"$(INTDIR)\gthread_init.obj" \
	"$(INTDIR)\hub_server.obj" \
	"$(INTDIR)\init_server_wrapper.obj" \
	"$(INTDIR)\ip_util.obj" \
	"$(INTDIR)\local_memory.obj" \
	"$(INTDIR)\mkdirp.obj" \
	"$(INTDIR)\name_barrier.obj" \
	"$(INTDIR)\nfio.obj" \
	"$(INTDIR)\nframe.obj" \
	"$(INTDIR)\oa.obj" \
	"$(INTDIR)\plist.obj" \
	"$(INTDIR)\pointer_queue.obj" \
	"$(INTDIR)\pr_util.obj" \
	"$(INTDIR)\print_program.obj" \
	"$(INTDIR)\print_usage.obj" \
	"$(INTDIR)\program_tags.obj" \
	"$(INTDIR)\random.obj" \
	"$(INTDIR)\read_program.obj" \
	"$(INTDIR)\server_locations.obj" \
	"$(INTDIR)\signal.obj" \
	"$(INTDIR)\sleep.obj" \
	"$(INTDIR)\sls_verbose.obj" \
	"$(INTDIR)\sls_verbose_color.obj" \
	"$(INTDIR)\sock.obj" \
	"$(INTDIR)\sockqueue.obj" \
	"$(INTDIR)\srandom.obj" \
	"$(INTDIR)\SS_default_init.obj" \
	"$(INTDIR)\stream_util.obj" \
	"$(INTDIR)\string_util.obj" \
	"$(INTDIR)\strtok_r.obj" \
	"$(INTDIR)\sym.obj" \
	"$(INTDIR)\test_conditions.obj" \
	"$(INTDIR)\timed_tasks.obj" \
	"$(INTDIR)\tobj.obj" \
	"$(INTDIR)\uucode.obj" \
	"$(INTDIR)\vlist.obj" \
	"$(INTDIR)\xdr.obj" \
	"$(INTDIR)\xdr_array.obj" \
	"$(INTDIR)\xdr_buffer.obj" \
	"$(INTDIR)\xdr_float.obj" \
	"$(INTDIR)\xdr_mem.obj" \
	"$(INTDIR)\xdr_sizeof.obj"

"$(OUTDIR)\libGalaxy_debug.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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
!IF EXISTS("libGalaxy.dep")
!INCLUDE "libGalaxy.dep"
!ELSE 
!MESSAGE Warning: cannot find "libGalaxy.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libGalaxy - Win32 Release" || "$(CFG)" == "libGalaxy - Win32 Debug"
SOURCE=.\util\compat\win32\bcopy.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\bcopy.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\bcopy.obj"	"$(INTDIR)\bcopy.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\io\binary_io.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\binary_io.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\binary_io.obj"	"$(INTDIR)\binary_io.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\io\broker_data.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\broker_data.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\broker_data.obj"	"$(INTDIR)\broker_data.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\ServerStub\broker_proxy.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\broker_proxy.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\broker_proxy.obj"	"$(INTDIR)\broker_proxy.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\ServerStub\continuation.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\continuation.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\continuation.obj"	"$(INTDIR)\continuation.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\debug_memory.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\debug_memory.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\debug_memory.obj"	"$(INTDIR)\debug_memory.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\dispatch_function.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\dispatch_function.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\dispatch_function.obj"	"$(INTDIR)\dispatch_function.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\dynamic_buffer.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\dynamic_buffer.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\dynamic_buffer.obj"	"$(INTDIR)\dynamic_buffer.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\error_tags.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\error_tags.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\error_tags.obj"	"$(INTDIR)\error_tags.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\ServerStub\frame_util.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\frame_util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\frame_util.obj"	"$(INTDIR)\frame_util.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\ServerStub\galaxy_elr.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\galaxy_elr.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\galaxy_elr.obj"	"$(INTDIR)\galaxy_elr.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=".\ServerStub\generic-server-main.c"

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\generic-server-main.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\generic-server-main.obj"	"$(INTDIR)\generic-server-main.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=".\ServerStub\generic-server-toplevel.c"

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\generic-server-toplevel.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\generic-server-toplevel.obj"	"$(INTDIR)\generic-server-toplevel.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=".\ServerStub\generic-server.c"

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\generic-server.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\generic-server.obj"	"$(INTDIR)\generic-server.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\win32\gettimeofday.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\gettimeofday.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\gettimeofday.obj"	"$(INTDIR)\gettimeofday.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\grovel.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\grovel.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\grovel.obj"	"$(INTDIR)\grovel.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\gthread.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\gthread.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\gthread.obj"	"$(INTDIR)\gthread.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\gthread_init.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\gthread_init.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\gthread_init.obj"	"$(INTDIR)\gthread_init.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\io\hub_server.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\hub_server.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\hub_server.obj"	"$(INTDIR)\hub_server.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\ServerStub\init_server_wrapper.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\init_server_wrapper.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\init_server_wrapper.obj"	"$(INTDIR)\init_server_wrapper.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\io\ip_util.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\ip_util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\ip_util.obj"	"$(INTDIR)\ip_util.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\local_memory.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\local_memory.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\local_memory.obj"	"$(INTDIR)\local_memory.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\mkdirp.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\mkdirp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\mkdirp.obj"	"$(INTDIR)\mkdirp.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\name_barrier.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\name_barrier.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\name_barrier.obj"	"$(INTDIR)\name_barrier.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\nfio.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\nfio.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\nfio.obj"	"$(INTDIR)\nfio.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\nframe.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\nframe.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\nframe.obj"	"$(INTDIR)\nframe.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\oa.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\oa.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\oa.obj"	"$(INTDIR)\oa.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\plist.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\plist.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\plist.obj"	"$(INTDIR)\plist.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\io\pointer_queue.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\pointer_queue.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\pointer_queue.obj"	"$(INTDIR)\pointer_queue.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\pr_util.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\pr_util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\pr_util.obj"	"$(INTDIR)\pr_util.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\print_program.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\print_program.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\print_program.obj"	"$(INTDIR)\print_program.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\ServerStub\print_usage.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\print_usage.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\print_usage.obj"	"$(INTDIR)\print_usage.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\program_tags.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\program_tags.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\program_tags.obj"	"$(INTDIR)\program_tags.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\common\random.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\random.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\random.obj"	"$(INTDIR)\random.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\read_program.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\read_program.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\read_program.obj"	"$(INTDIR)\read_program.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\ServerStub\server_locations.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\server_locations.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\server_locations.obj"	"$(INTDIR)\server_locations.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\signal.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\signal.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\signal.obj"	"$(INTDIR)\signal.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\win32\sleep.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\sleep.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\sleep.obj"	"$(INTDIR)\sleep.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\sls_verbose.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\sls_verbose.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\sls_verbose.obj"	"$(INTDIR)\sls_verbose.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\sls_verbose_color.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\sls_verbose_color.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\sls_verbose_color.obj"	"$(INTDIR)\sls_verbose_color.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\sock.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\sock.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\sock.obj"	"$(INTDIR)\sock.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\io\sockqueue.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\sockqueue.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\sockqueue.obj"	"$(INTDIR)\sockqueue.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\common\srandom.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\srandom.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\srandom.obj"	"$(INTDIR)\srandom.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\ServerStub\SS_default_init.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\SS_default_init.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\SS_default_init.obj"	"$(INTDIR)\SS_default_init.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\stream_util.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\stream_util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\stream_util.obj"	"$(INTDIR)\stream_util.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\string_util.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\string_util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\string_util.obj"	"$(INTDIR)\string_util.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\win32\strtok_r.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\strtok_r.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\strtok_r.obj"	"$(INTDIR)\strtok_r.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\sym.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\sym.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\sym.obj"	"$(INTDIR)\sym.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\test_conditions.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\test_conditions.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\test_conditions.obj"	"$(INTDIR)\test_conditions.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\timed_tasks.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\timed_tasks.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\timed_tasks.obj"	"$(INTDIR)\timed_tasks.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\tobj.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\tobj.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\tobj.obj"	"$(INTDIR)\tobj.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\uucode.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\uucode.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\uucode.obj"	"$(INTDIR)\uucode.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\galaxy\vlist.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\vlist.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\vlist.obj"	"$(INTDIR)\vlist.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\win32\xdr.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\xdr.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\xdr.obj"	"$(INTDIR)\xdr.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\win32\xdr_array.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\xdr_array.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\xdr_array.obj"	"$(INTDIR)\xdr_array.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\io\xdr_buffer.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\xdr_buffer.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\xdr_buffer.obj"	"$(INTDIR)\xdr_buffer.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\win32\xdr_float.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\xdr_float.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\xdr_float.obj"	"$(INTDIR)\xdr_float.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\win32\xdr_mem.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\xdr_mem.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\xdr_mem.obj"	"$(INTDIR)\xdr_mem.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\util\compat\win32\xdr_sizeof.c

!IF  "$(CFG)" == "libGalaxy - Win32 Release"


"$(INTDIR)\xdr_sizeof.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"


"$(INTDIR)\xdr_sizeof.obj"	"$(INTDIR)\xdr_sizeof.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

