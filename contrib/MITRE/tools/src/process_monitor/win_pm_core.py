# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import win32process, win32pipe, win32security, win32con, \
       win32api, win32file, string, types, pywintypes, os, string

# This is a translation, into native Python, of the C code
# in win32popen.cpp in the Windows extensions for Python. The
# reason I'm doing this perverse thing is because it will give
# me access to both the I/O and the process control, something
# that seems to be impossible in the popen abstraction you
# currently get.

# I'm indebted to Pat Knight, http://www.wideawake.co.uk, for posting
# a version of this translation to comp.lang.python in 1999, and to Craig
# Curtin
# for posting a tip on doing a nonblocking poll. The tip turned out to
# be wrong, but I'm grateful anyway. It turns out that you can't do
# overlapped I/O on an anonymous pipe, but you CAN use the poorly-named
# PeekNamedPipe to do a nonblocking poll.

# All the Windows-specific stuff needs to go in here.

_ver = win32api.GetVersionEx()[3]        # distinguish NT and Win95/98
if _ver == 2:                    # we're on NT
    PROCESS_RUNNER = 'cmd.exe /c'
else:
    # PROCESS_RUNNER = '<name of program based on MS conspawn.exe>'
    raise "Unsupported before Windows NT"

###########################################################
#
#    THIS FILE SHOULD EXPORT:
#    _MAYBE_ALIVE
#    _DEFINITELY_ALIVE
#    _DEFINITELY_DEAD
#    ProcessContainer
#        methods: _ReadBytes, FDCallbackAvailable, _ForceShutdown,
#                 _RestartChildProcess, _WriteString, _ShutdownSubprocess,
#                 _CheckDead, WrapCmdlineForExecution
#    ProcessContainerSet
#        methods: _CheckInput
#                 _ExecutableExists
#                 _EnforceShutdown
#    ProcessEnvironment
#        methods: _ComputeArchos
#                 _CleanupExpansion
#
###########################################################

_MAYBE_ALIVE = 0
_DEFINITELY_ALIVE = 1
_DEFINITELY_DEAD = 2

# SAM 7/19/02: When you execute either a Windows or a Unix
# process under the shell, it appears to start, and then
# you read from stderr and exit. When you execute a process
# directly in Windows (can't tell in Unix, since popen2
# always wraps /bin/sh around things), you raise an error.
# The problem is that the process monitor assumes the former
# behavior: i.e., Restart always succeeds. I could build
# a "fake" file descriptor, or I could change the logic of
# the process monitor.

# After poking around for a while, I've decided that the
# path of least resistance is to build fake descriptors.
# First, the real ones.

class IOHandle:
    def __init__(self, handle):
        self.handle = handle
    def Close(self):
        self.handle.Close()
    def Peek(self):
        return win32pipe.PeekNamedPipe(self.handle, 0)
    def Read(self, num_bytes):
        return win32file.ReadFile(self.handle, num_bytes, None)
    def Write(self, str):
        win32file.WriteFile(self.handle, str, None)

def CreateIOHandle(handle):
    if handle is not None:
        return IOHandle(handle)
    else:
        return None

class PidHandle:
    def __init__(self, handle):
        self.handle = handle
    def Terminate(self):
        try:
            win32process.TerminateProcess(self.handle, 1)
            self.handle.Close()
        except:
            # Just about any error means that it's toast.
            pass
    def Dead(self):        
        exit_code = win32process.GetExitCodeProcess(self.handle)
        if exit_code == win32con.STILL_ACTIVE:
            return 0
        else:
            return 1

# The fake ones will be loaded for an error.

class FakeIOHandle:
    def __init__(self, read_buffer = ""):
        if read_buffer:
            read_buffer = read_buffer + "\n"
        self.read_buffer = read_buffer
    def Close(self):
        pass
    def Peek(self):
        return None, len(self.read_buffer), None
    def Read(self, num_bytes):
        buf = self.read_buffer
        self.read_buffer = ""
        return None, buf

class FakePidHandle:
    def Terminate(self):
        pass
    def Dead(self):
        return 1

class ProcessContainer:

    def FDCallbackAvailable(self):
        return 0
                
    def _ReadBytes(self, handle, num_bytes):
        if num_bytes == -1:
            try:
                ignore, num_bytes, num_read = handle.Peek()
            except:
                # Just about any error means that it's toast.
                return _DEFINITELY_DEAD, ""
        if num_bytes > 0:
            try:
                ignore, s = handle.Read(num_bytes)
            except:
                return _DEFINITELY_DEAD, ""
            if len(s) == num_bytes:
                status = _DEFINITELY_ALIVE
            else:
                status = _MAYBE_ALIVE
            # It turns out that Python automatically converts
            # \r\n to \n when reading from a file descriptor
            # obtained via open(), and I need that same behavior
            # in a number of places, such as when I write the
            # output to Tk widgets and when I compare it to
            # the match files in the test suite. So I think
            # I need to convert it here. This will
            # also apply (vacuously) to the fake handles.
            # We also need to make sure we do the conversion
            # AFTER we check the length.
            s = string.replace(s, "\r\n", "\n")
            return status, s
        else:
            return _MAYBE_ALIVE, ""

    def _ForceShutdown(self, handle):
        handle.Close()

    def WrapCmdlineForExecution(self, cmdline, interleave_error):
        return cmdline
            
    def _mkarg(self, arg):
        result = string.replace(arg,'"','"""')
        if string.count(arg, ' ') > 0:
            return ' "' + result + '"'
        else:
            return ' ' + result

    def _mkcommand(self, command, args):
        # On 95 waspawn.exe is used to run child processes, it prevents a bug
        # when running dos apps/batch files, a console would otherwise be
        # created to run the batch file in, and this would hang around
        # afterwards leaving WA waiting for it. Also stops a console from
        # popping up.
        # On NT cmd.exe is used since otherwise we couldn't invoke DOS
        # commands. This also prevents console windows from popping up.

        # Yes, but. If we use the process runner, it appears we have
        # exactly the same problem as when we run shells with exec:
        # killing the process you have a handle for won't kill the
        # process you want to kill.
        # Neither ExitProcess nor TerminateProcess kill children.
        # And there doesn't seem to be any such thing as exec.

        # our_command = PROCESS_RUNNER + self._mkarg(command)
        our_command = command
        if type(args) == types.StringType:
            our_command = our_command + self._mkarg(args)
        else:
            for arg in args:
                our_command = our_command + self._mkarg(arg)
        return our_command
    
    def _RestartChildProcess(self, cmdline, interleave_error):
        sa = win32security.SECURITY_ATTRIBUTES()
        sa.bInheritHandle = 1

        start_up_info = win32process.STARTUPINFO()
        # set the STARTUPINFO fields
        start_up_info.dwFlags = win32process.STARTF_USESTDHANDLES | \
                                win32process.STARTF_USESHOWWINDOW
        start_up_info.wShowWindow = win32con.SW_HIDE

        # Create the pipe for child stdin.    
        
        stdin_read_end, stdin_write_end = win32pipe.CreatePipe(sa, 0)

        # Duplicate the handler.    

        stdin_write_dup = win32api.DuplicateHandle(win32api.GetCurrentProcess(),
                                                   stdin_write_end,
                                                   win32api.GetCurrentProcess(),
                                                   0, 0, win32con.DUPLICATE_SAME_ACCESS)

        # Close the inheritable end.
        
        stdin_write_end.Close()

        # Add to startup info.

        start_up_info.hStdInput = stdin_read_end    

        # Do the same for stdout.
    
        stdout_read_end, stdout_write_end = win32pipe.CreatePipe(sa, 0)
        stdout_read_dup = win32api.DuplicateHandle(win32api.GetCurrentProcess(),
                                                   stdout_read_end,
                                                   win32api.GetCurrentProcess(),
                                                   0, 0, win32con.DUPLICATE_SAME_ACCESS)
        stdout_read_end.Close()
        start_up_info.hStdOutput = stdout_write_end

        # If we're interleaving the error, use the same output. Otherwise,
        # set up a separate pipe.

        if interleave_error:
            start_up_info.hStdError = stdout_write_end
            stderr_read_dup = None
            stderr_write_end = None
        else:
            stderr_read_end, stderr_write_end = win32pipe.CreatePipe(sa, 0)
            stderr_read_dup = win32api.DuplicateHandle(win32api.GetCurrentProcess(),
                                                       stderr_read_end,
                                                       win32api.GetCurrentProcess(),
                                                       0, 0, win32con.DUPLICATE_SAME_ACCESS)
            stderr_read_end.Close()
            start_up_info.hStdError = stderr_write_end

        # create the process. Split the cmdline, and then
        # reassemble it.
        
        # But then we need to retokenize it, to deal with strings w/spaces.
        # Oops. There may be backslashes in the strings here,
        # which aren't escapes, because they're Windows paths.
        # The right thing to do is to turn the backslash into
        # a double backslash when we expand the values in the
        # expansion dictionary, at least on Windows. Probably
        # by entering them in the expansions with double
        # backslashes. We should probably run the
        # tokenizer on the cmd, too, just to get the
        # escapes right.

        tokenized_cmdline = self.TokenizeCmdlineArgs(cmdline)

        cmd = tokenized_cmdline[0]
        args = tokenized_cmdline[1:]

        modified_cmd = self._mkcommand(cmd, args)

        try:
            process_info = win32process.CreateProcess(None, modified_cmd,
                                                      None, None, 1,
                                                      win32con.CREATE_NEW_CONSOLE,
                                                      None, 
                                                      None, start_up_info)
            proc, thread, proc_id, thread_id = process_info
            thread.Close()
            return CreateIOHandle(stdout_read_dup), \
                   CreateIOHandle(stdin_write_dup), \
                   CreateIOHandle(stderr_read_dup), \
                   PidHandle(proc)
        except pywintypes.error, (e_num, e_fn, e_desc):
            # Be sure to clean up!
            stdin_read_end.Close()
            stdin_write_dup.Close()
            stdout_write_end.Close()
            stdout_read_dup.Close()
            if stderr_write_end is not None:
                stderr_write_end.Close()
            if stderr_read_dup is not None:
                stderr_read_dup.Close()
            # Sometimes, when the error requires a parameter,
            # the error message here is just a default, for
            # some reason. I've copied those into here,
            # just because I'm anal.
            if e_desc == "No error message is available":
                try:
                    desc, num_name = ExtraErrors[e_num]
                    e_desc = "%s: %s" % (num_name, desc)
                except KeyError:
                    e_desc = "%d: %s" % (e_num, e_desc)
            return FakeIOHandle(), FakeIOHandle(), \
                   FakeIOHandle("%s: %s" % (e_fn, e_desc)), \
                   FakePidHandle()

    def _WriteString(self, out_handle, str):
        # Ignore the return value.
        out_handle.Write(str)

    def _ShutdownSubprocess(self, pid_handle, really_kill):
        pid_handle.Terminate()
        return None

    def _CheckDead(self, child_handle):
        return child_handle.Dead()

# We ignore the timeout on Windows.

class ProcessContainerSet:
    def _CheckInput(self, p_info, timeout = None):
        # Everything is potentially readable.
        res_info = []
        for p, handle in p_info:
            # Let's be a little discriminating. I could return
            # everything, but that would be a bad thing, since then
            # the test in the test suite would never time out.
            # Let's peek, and if there are bytes to read or an error,
            # insert it.
            try:
                ignore, num_bytes, num_read = handle.Peek()
                if num_bytes > 0:
                    res_info.append((p, handle, -1))
            except:
                res_info.append((p, handle, -1))
        return res_info
    def _ExecutableExists(self, executable):
        # Check the executable, but also check the path.
        to_check = [executable]
        if os.path.splitext(executable)[1] == "":
            to_check.append(executable + ".exe")
        if os.path.isabs(executable):
            for elt in to_check:
                if os.path.exists(elt):
                    return 1
            return 0
        else:
            # Parse path.
            try:
                dirs = string.split(os.environ["PATH"], ";")
            except KeyError:
                dirs = []
            for d in dirs:
                for elt in to_check:
                    if os.path.exists(os.path.join(d, elt)):
                        return 1
            return 0

    def _EnforceShutdown(self, all_pids, really_kill = 1):
        for pid_handle in all_pids:
            pid_handle.Terminate()

class ProcessEnvironment:
    def _ComputeArchos(self, gc_home):
        return 'x86-nt'
    def _CleanupExpansion(self, expansion):
        # Anything in the expansion table should expect to
        # be tokenized and processed. This will include
        # treating backslashes as escapes. In Windows, the
        # entries in this table will almost certainly
        # not want those backslashes eliminated.
        return string.replace(expansion, "\\", "\\\\")

# These are taken from the MSVC docs. These are the
# error messages which have parameters in them which some of the
# Windows extensions barf on.

ExtraErrors = \
{34: ("The wrong diskette is in the drive.", "ERROR_WRONG_DISK"),
 106: ("Insert the diskette for the drive.", "ERROR_SEM_USER_LIMIT"),
 129: ("The application cannot be run in Win32 mode.", "ERROR_CHILD_NOT_COMPLETE"),
 182: ("The operating system cannot run the application.", "ERROR_INVALID_ORDINAL"),
 188: ("The operating system cannot run the application.", "ERROR_INVALID_STARTING_CODESEG"),
 189: ("The operating system cannot run the application.", "ERROR_INVALID_STACKSEG"),
 190: ("The operating system cannot run the application.", "ERROR_INVALID_MODULETYPE"),
 191: ("Cannot run the application in Win32 mode.", "ERROR_INVALID_EXE_SIGNATURE"),
 192: ("The operating system cannot run the application.", "ERROR_EXE_MARKED_INVALID"),
 193: ("The file is not a valid Win32 application.", "ERROR_BAD_EXE_FORMAT"),
 194: ("The operating system cannot run the application.", "ERROR_ITERATED_DATA_EXCEEDS_64k"),
 195: ("The operating system cannot run the application.", "ERROR_INVALID_MINALLOCSIZE"),
 198: ("The operating system cannot run the application.", "ERROR_INVALID_SEGDPL"),
 201: ("The operating system cannot run the application.", "ERROR_RELOC_CHAIN_XEEDS_SEGLIM"),
 202: ("The operating system cannot run the application.", "ERROR_INFLOOP_IN_RELOC_CHAIN"),
 216: ("The image file is valid, but is for a machine type other than the current machine.", "ERROR_EXE_MACHINE_TYPE_MISMATCH"),
 317: ("The system cannot find message text for the message in the message file.", "ERROR_MR_MID_NOT_FOUND")
 }
