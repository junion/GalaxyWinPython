# Microsoft Developer Studio Project File - Name="libGalaxy" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libGalaxy - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libGalaxy.mak".
!MESSAGE 
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

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libGalaxy - Win32 Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\lib\x86-nt"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\..\include" /I "..\local-include" /I "..\..\templates\x86-nt" /I ".\util\compat\win32" /D "NDEBUG" /D "_WINDOWS" /D "_REENTRANT" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_MBCS" /D "__STDC__" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libGalaxy - Win32 Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\lib\x86-nt"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\include" /I "..\local-include" /I "..\..\templates\x86-nt" /I ".\util\compat\win32" /D "_DEBUG" /D "_LIB" /D "GAL_THREADS" /D "WIN32_LEAN_AND_MEAN" /D _WIN32_WINNT=0x0400 /D WINVER=0x0400 /D "WIN32" /D "_MBCS" /D "__STDC__" /D "_REENTRANT" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\lib\x86-nt\libGalaxy_debug.lib"

!ENDIF 

# Begin Target

# Name "libGalaxy - Win32 Release"
# Name "libGalaxy - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\util\compat\win32\bcopy.c
# End Source File
# Begin Source File

SOURCE=.\io\binary_io.c
# End Source File
# Begin Source File

SOURCE=.\io\broker_data.c
# End Source File
# Begin Source File

SOURCE=.\ServerStub\broker_proxy.c
# End Source File
# Begin Source File

SOURCE=.\ServerStub\continuation.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\debug_memory.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\dispatch_function.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\dynamic_buffer.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\error_tags.c
# End Source File
# Begin Source File

SOURCE=.\ServerStub\frame_util.c
# End Source File
# Begin Source File

SOURCE=.\ServerStub\galaxy_elr.c
# End Source File
# Begin Source File

SOURCE=".\ServerStub\generic-server-main.c"
# End Source File
# Begin Source File

SOURCE=".\ServerStub\generic-server-toplevel.c"
# End Source File
# Begin Source File

SOURCE=".\ServerStub\generic-server.c"
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\gettimeofday.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\grovel.c
# End Source File
# Begin Source File

SOURCE=.\util\gthread.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\gthread_init.c
# End Source File
# Begin Source File

SOURCE=.\io\hub_server.c
# End Source File
# Begin Source File

SOURCE=.\ServerStub\init_server_wrapper.c
# End Source File
# Begin Source File

SOURCE=.\io\ip_util.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\local_memory.c
# End Source File
# Begin Source File

SOURCE=.\util\mkdirp.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\name_barrier.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\nfio.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\nframe.c
# End Source File
# Begin Source File

SOURCE=.\util\oa.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\plist.c
# End Source File
# Begin Source File

SOURCE=.\io\pointer_queue.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\pr_util.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\print_program.c
# End Source File
# Begin Source File

SOURCE=.\ServerStub\print_usage.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\program_tags.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\common\random.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\read_program.c
# End Source File
# Begin Source File

SOURCE=.\ServerStub\server_locations.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\signal.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\sleep.c
# End Source File
# Begin Source File

SOURCE=.\util\sls_verbose.c
# End Source File
# Begin Source File

SOURCE=.\util\sls_verbose_color.c
# End Source File
# Begin Source File

SOURCE=.\util\sock.c
# End Source File
# Begin Source File

SOURCE=.\io\sockqueue.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\common\srandom.c
# End Source File
# Begin Source File

SOURCE=.\ServerStub\SS_default_init.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\stream_util.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\string_util.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\strtok_r.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\sym.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\test_conditions.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\timed_tasks.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\tobj.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\uucode.c
# End Source File
# Begin Source File

SOURCE=.\galaxy\vlist.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\xdr.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\xdr_array.c
# End Source File
# Begin Source File

SOURCE=.\io\xdr_buffer.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\xdr_float.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\xdr_mem.c
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\xdr_sizeof.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\io\binary_io.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\common_decls.h
# End Source File
# Begin Source File

SOURCE=.\ServerStub\continuation.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\debug_memory.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\distinguished_keys.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\error_tags.h
# End Source File
# Begin Source File

SOURCE=.\galaxy\gal_internal.h
# End Source File
# Begin Source File

SOURCE=.\galaxy\gal_types_internal.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\galaxy.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\galaxy_all.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\galaxy_io.h
# End Source File
# Begin Source File

SOURCE=.\io\galio_types_internal.h
# End Source File
# Begin Source File

SOURCE="..\..\templates\x86-nt\GC_config.h"
# End Source File
# Begin Source File

SOURCE=".\ServerStub\generic-server-internal.h"
# End Source File
# Begin Source File

SOURCE="..\..\include\galaxy\generic-server-types.h"
# End Source File
# Begin Source File

SOURCE="..\..\include\galaxy\generic-server.h"
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\gthread.h
# End Source File
# Begin Source File

SOURCE=.\io\io_internal.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\io_msg_types.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\name_barrier.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\program.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\program_tags.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\server_functions.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\server_prototypes.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\socket.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\sysdep.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\tag_enum.h
# End Source File
# Begin Source File

SOURCE=..\..\include\galaxy\util.h
# End Source File
# Begin Source File

SOURCE=.\util\compat\win32\rpc\xdr.h
# End Source File
# End Group
# End Target
# End Project
