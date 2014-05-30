# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import os, fcntl, sys, popen2, signal, errno, time, select, string

try:
    F_SETFL = fcntl.F_SETFL
    F_GETFL = fcntl.F_GETFL
except AttributeError:
    import FCNTL
    F_SETFL = FCNTL.F_SETFL
    F_GETFL = FCNTL.F_GETFL

try:
    O_NONBLOCK = os.O_NONBLOCK
except AttributeError:
    import FCNTL
    O_NONBLOCK = FCNTL.O_NONBLOCK

# All the Unix-specific stuff needs to go in here.

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

class ProcessContainer:

    def FDCallbackAvailable(self):
        return 1

    def _ReadBytes(self, handle, num_bytes):
        # If num_bytes is -1, read whatever you can.
        try:
            if num_bytes == -1:
                data = handle.read()
                if data:
                    status = _DEFINITELY_ALIVE
                else:
                    status = _MAYBE_ALIVE
            else:
                data = handle.read(num_bytes)
                if len(data) < num_bytes:
                    status = _MAYBE_ALIVE
                else:
                    status = _DEFINITELY_ALIVE
            return status, data
        except IOError:
            eval_res = sys.exc_info()[1]
            if hasattr(eval_res, "errno") and \
               eval_res.errno == errno.EAGAIN:
                return _DEFINITELY_ALIVE, ""
            else:
                return _DEFINITELY_DEAD, ""            
    
    def _ForceShutdown(self, fd):
        fd.close()
    
    def _SetNonblocking(self, fd):
        fcntl.fcntl(fd.fileno(), F_SETFL,
                    O_NONBLOCK | fcntl.fcntl(fd.fileno(),
                                             F_GETFL))

    def WrapCmdlineForExecution(self, cmdline, interleave_error):
        if interleave_error:
            return "exec " + cmdline + " 2>&1"
	else:
	    return "exec " + cmdline

    def _RestartChildProcess(self, cmdline, interleave_error):
        wrapped_cmd = self.WrapCmdlineForExecution(cmdline, interleave_error)
        if interleave_error:
            stream = popen2.Popen3(wrapped_cmd, 0, 0)
	    in_f, out_f, err_f = stream.fromchild, stream.tochild, None
	else:
	    stream = popen2.Popen3(wrapped_cmd, 1, 0)
	    in_f, out_f, err_f = stream.fromchild, stream.tochild, stream.childerr

        self._SetNonblocking(in_f)
        if err_f is not None:
            self._SetNonblocking(err_f)

	return in_f, out_f, err_f, stream.pid

    def _WriteString(self, out_handle, str):
        out_handle.write(str)
        out_handle.flush()

    def _ShutdownSubprocess(self, pid_handle, really_kill):
        p = pid_handle
        pid, status = _PidStatus(pid_handle)
        if (pid is not None) and (pid != pid_handle):
            try:
                os.kill(pid_handle, signal.SIGTERM)
            except os.error, (errnum, msg):
                p = None
        else:
            p = pid 
        if p is not None:
            self.process_set._EnforceShutdown([pid_handle], really_kill)
        return p

    def _CheckDead(self, child_handle):
        pid, status = _PidStatus(child_handle)
	if pid is None:
	    return 1
	else:
	    return pid == child_handle

# Utilities.

def _PidStatus(pid):
    try:
	child_pid, status = os.waitpid(pid, os.WNOHANG)
	# print child_pid, status
	return child_pid, status
    except os.error, (errnum, msg):
	return None, -1

class ProcessContainerSet:
    def _CheckInput(self, p_info, timeout = None):
        fd_dict = {}
        for p, handle in p_info:
            fd_dict[handle] = p
        if timeout is None:
            selected_fds = select.select(fd_dict.keys(), [], [])
        else:
            selected_fds = select.select(fd_dict.keys(), [], [], timeout)
        res_info = []
        if selected_fds:
            for s_fd in selected_fds[0]:
                res_info.append((fd_dict[s_fd], s_fd, -1))
        return res_info
    def _ExecutableExists(self, executable):
        return os.path.exists(executable)
    def _EnforceShutdown(self, all_pids, really_kill = 1):
        still_pids = []
        result_tuples = []
        for pid in all_pids:
            # print "Checking pid", pid
            status = -1
            p = pid
            p, status = _PidStatus(pid)
            # print p, status
            if (p is None) or (not really_kill) or (p == pid):
                result_tuples.append((pid, p, status))
            else:
                still_pids.append(pid)
        if still_pids:
            time.sleep(.25)
            for pid in still_pids:
                # print "Checking pid", pid, "again"
                p = pid
                p, status = _PidStatus(pid)
                # print p, status
                if (p is not None) and (p != pid):
                    # print "Really killing", pid
                    # Put a catch around it, JUST IN CASE.
                    try:
                        os.kill(pid, signal.SIGKILL)
                    except os.error, (errnum, msg):
                        p = None
                        p, status = _PidStatus(pid)
                result_tuples.append((pid, p, status))
        return result_tuples


class ProcessEnvironment:
    def _ComputeArchos(self, gc_home):
        arch_fp = os.popen(os.path.join(gc_home, "templates", "set_arch.csh"), "r")
        arch_str = string.strip(arch_fp.read())
        arch_fp.close()
        os_fp = os.popen(os.path.join(gc_home, "templates", "set_os.csh"), "r")
        os_str = string.strip(os_fp.read())
        os_fp.close()
        return arch_str+"-"+os_str
    
    def _CleanupExpansion(self, expansion):
        return expansion
