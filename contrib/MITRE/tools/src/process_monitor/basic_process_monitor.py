# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import os, string, re, tempfile

if os.name == 'posix':
    import unix_pm_core
    from unix_pm_core import _MAYBE_ALIVE, _DEFINITELY_ALIVE, _DEFINITELY_DEAD
    pm_core = unix_pm_core
elif os.name in ['dos', 'nt']:
    import win_pm_core
    from win_pm_core import _MAYBE_ALIVE, _DEFINITELY_ALIVE, _DEFINITELY_DEAD
    pm_core = win_pm_core
else:
    raise "Can't run process monitor on", os.name

# This class controls the subprocess.

# This class has the following basic behavior:
# PROCESS_TITLE: or -T is the title
# PAUSE: or --sleep is how long to sleep after startup
# PROCESS_KEEP_ALIVE: --keep_alive is whether to restart after shutdown
# PROCESS: or -c is the command line

# Tk has --input_line, --input_return, --start, --open

# 5-tuples are (config_file_entry, cmdline, needs arg, method name, block starter).

# ONE BACKWARD COMPATIBILITY GLITCH: for PROCESS:, the keyword 
# STARTS the block, while the command line ENDS the block. All the
# others optionally start the block in the command line, while
# they can't in the configuration file case. Sorry about that.
# See the MustEndBlock() method on ProcessContainerSet.

# For run_test_suite.py, ProcessContainer needs to be able to 
# stand alone.

ProcessContainerError = "ProcessContainerError"

class ProcessContainer(pm_core.ProcessContainer):
    config_table = [("PROCESS_TITLE:", "-T", 1, "AddTitle", 0),
		    ("PAUSE:", "--sleep", 1, "AddPause", 0),
		    ("PROCESS_KEEP_ALIVE:", "--keep_alive", 0,
		     "AddKeepAlive", 0),
		    ("PROCESS:", "-c", 1, "AddCommandLine", 1)]

    def __init__(self, process_set = None):
	if process_set is not None:
	    process_set.AddProcess(self)
	self.process_set = process_set
	self.title = ""
	self.pause = 0
	self.keepalive = 0
	self.cmdline = None
        # Doing away with the file descriptor objects.
	self._SetupHandles()
	self.alive = 0
	self.history_buffer = ""
	self.error_buffer = ""
	self.interleave_error = 1

    # Configuration methods.

    def AddTitle(self, title):
	if self.title:
	    raise ConfigurationError, "title already specified"
	self.title = title

    def AddPause(self, pause):
	if self.pause:
	    raise ConfigurationError, "pause already specified"
	try:
	    self.pause = string.atoi(pause)
	except:
	    raise ConfigurationError, \
		  ("bad pause specification `%s'" % pause)

    def AddKeepAlive(self):
	self.keepalive = 1

    def AddCommandLine(self, command_line):
	if self.process_set is not None:
	    env = self.process_set.process_environment
	    if not env.suppress_expansions:
		command_line = env.ExpandFile(command_line)
	if self.cmdline:
	    raise ConfigurationError, "command line already specified"
	self.cmdline = command_line

    # Runtime behavior.

    def ConfigDone(self):
	if not self.cmdline:
	    raise ConfigurationError, "no command line"

    def _SetupHandles(self):
	self.in_handle = None
	self.err_handle = None
	self.out_handle = None
        self.child_handle = None

    def KeepAlive(self):
	self.keepalive = 1

    def LetDie(self):
	self.keepalive = 0

    def UpdateIdle(self):
	pass
    
    def ConsiderShutdown(self):
	if not (self.in_handle or self.err_handle):
	    self.Shutdown()

    def ConsiderRestart(self):
	if self.keepalive:
	    # If we're supposed to keep this process alive,
	    # then restart the damn thing.
	    self.Restart()

    def WriteHistory(self, str, fd):
	# Keep everything yourself.
        if (fd is not None) and (fd == self.err_handle):
	    self.error_buffer = self.error_buffer + str
	self.history_buffer = self.history_buffer + str

    def ClearHistory(self):
	self.history_buffer = ""
	self.error_buffer = ""

    def ReadCallback(self, handle, num_bytes):
        # num_bytes should be -1 when you can read as
        # much as you want.
        global _DEFINITELY_ALIVE, _MAYBE_ALIVE, _DEFINITELY_DEAD
        status, data = self._ReadBytes(handle, num_bytes)
        if status == _DEFINITELY_ALIVE:
            if data:
                self.WriteHistory(data, handle)
            self.UpdateIdle()
        elif ((status == _MAYBE_ALIVE) and self.Dead()) or \
             (status == _DEFINITELY_DEAD):
            # If it might have been alive, there may be data.
            if data:
                self.WriteHistory(data, handle)
            # Try one last read, just in case something
            # was written before it was definitely dead.
            final_status, final_data = self._ReadBytes(handle, -1)
            if final_data:
                self.WriteHistory(final_data, handle)
            self._ShutdownHandle(handle)
            self.ConsiderShutdown()
            self.ConsiderRestart()

    def WriteCallback(self, handle):
        raise ProcessContainerError, "not implemented"

    def _ShutdownHandle(self, handle):
        if handle == self.in_handle:
            self._ForceShutdown(handle)
            self.in_handle = None
        elif handle == self.out_handle:
            self._ForceShutdown(handle)
            self.out_handle = None
        elif handle == self.err_handle:
            self._ForceShutdown(handle)
            self.err_handle = None
            
    def _ShutdownHandles(self):
        if self.in_handle is not None:
	    self._ShutdownHandle(self.in_handle)
        if self.out_handle is not None:
	    self._ShutdownHandle(self.out_handle)
        if self.err_handle is not None:
	    self._ShutdownHandle(self.err_handle)
        
    def Shutdown(self, really_kill = 1):
	# Unmonitor the output, and set the pause button
	# to "resting" state.
	# Returns the child pid of the killed element.
	# If really_kill is 1, make sure it's dead, by sleeping
	# for a brief time and then really killing it
	# if it's not dead.
        # Return the handle of the process if it's still alive.
        self._ShutdownHandles()
	p = self.child_handle
	if self.child_handle:
	    # It may already be dead...
            p = self._ShutdownSubprocess(self.child_handle, really_kill)
	    self.child_handle = None
	    self.WriteHistory("======================================\n", None)
	self.alive = 0
	return p

    def Dead(self):
	if not self.alive:
	    return 1
        return self._CheckDead(self.child_handle)
    
    # In order to interleave stdout and stderr better, I've
    # redirected stderr to stdout in the subshell. The problem is
    # that under this configuration, the shell does not return
    # a valid fd for the error stream. So I need not to monitor it.

    def MonitorOutput(self):
	# I need this for the child classes, I think.
	pass

    def Restart(self):
	self.Shutdown()
	# Don't buffer anything! If you do, the filehandler
	# stuff won't work...
	cmdline = self.GetCmdlineForExecution()
        # Restart may fail. In Unix, this will just print
        # something to stderr of an apparently live subprocess,
        # but in Windows, it will raise an error. This is
        # because we're running a shell in one case, but not
        # in the other.
        # Problem: we have to make sure that the right things
        # happen when we get a failure. In particular, the
        # folks built on top of this (e.g., the process monitor
        # itself) assumes restart was successful, and that
        # failure and shutdown will be processed as a subsequent
        # read. After careful consideration, I decided to
        # prepare fake handles.
        info = self._RestartChildProcess(cmdline, self.interleave_error)
        self.in_handle, self.out_handle, self.err_handle, self.child_handle = info
        self.MonitorOutput()
        self.alive = 1

    def GetCmdlineForDisplay(self):
	return self.cmdline

    def GetCmdlineForExecution(self):
	cmdline = self.cmdline
	if self.process_set is not None:
	    env = self.process_set.process_environment
	    if not env.initialized:
		env.Initialize()
	    # If expansions are suppressed, the command line
	    # needs to be expanded when it's executed.
	    if env.suppress_expansions or env.tempfiles_created:
		cmdline = env.ExpandFile(self.cmdline)
	return cmdline

    def SetCmdline(self, str):
	if self.process_set is not None:
	    env = self.process_set.process_environment
	    if not env.suppress_expansions:
		str = env.ExpandFile(str)
	self.cmdline = str

    def WriteString(self, str):
        if self.out_handle:
            self._WriteString(self.out_handle, str)
	else:
	    self.WriteHistory("[no process]\n", None)

    def TokenizeCmdlineArgs(self, args):
        in_double_quote = 0
	in_single_quote = 0
	in_escape = 0
	cur_token = []
	cmdline_args = []
	# Don't forget, we also need to compute escapes...
	for c in args:
	    if in_escape:
		cur_token.append(c)
		in_escape = 0
	    elif c == "\\":
		in_escape = 1	    
	    elif in_double_quote:
		if c == '"':
		    in_double_quote = 0
		else:
		    cur_token.append(c)
	    elif in_single_quote:
		if c == "'":
		    in_single_quote = 0
		else:
		    cur_token.append(c)
	    elif c == "'":
		in_single_quote = 1
	    elif c == '"':
		in_double_quote = 1
	    elif c in string.whitespace:
		if cur_token:
		    cmdline_args.append(string.join(cur_token, ""))
		    cur_token = []
	    else:
		cur_token.append(c)
	if cur_token:
	    cmdline_args.append(string.join(cur_token, ""))
        return cmdline_args

# This class controls a set of processes. It can be
# subclassed as an individual test (for the test harness)
# or a process monitor window (for the interactive
# controller).

class ProcessContainerSet(pm_core.ProcessContainerSet):
    config_table = [("TITLE:", "--master_title", 1, "AddTitle", 1),
                    ("REQUIRES:", None, 1, "AddRequirements", 0)]

    def __init__(self, environment = None):
	self.processes = []
        if environment is not None:
            environment.AddProcessSet(self)
        self.process_environment = environment
	self.title = ""
        self.requirements = []

    def AddProcess(self, process):
	self.processes.append(process)
    
    def AddTitle(self, title):
	self.title = title

    def AddRequirements(self, remainder):
        # Requirements can now have values.
        reqs = string.split(remainder)
        for r in reqs:
            res = string.split(r, "=", 1)
            if len(res) == 2:
                self.requirements.append((res[0], res[1]))
            else:
                self.requirements.append((res[0], None))

    def UnmetRequirements(self):
	# Return the missing requirements
	failed = []
	for req, value in self.requirements:
	    if string.upper(req) not in self.process_environment.requirements_met:
		failed.append(req)
            elif (value is not None) and \
                 (self.process_environment.ExpandFile("$"+string.upper(req)) != value):
                failed.append(req+"="+value)
	return failed
    
    def CanStartBlock(self, config_entry, from_cmdline):
	# Anything can start a process block from the command line.	
	if from_cmdline:
	    return 1
	else:
	    return config_entry[-2]

    def MustStartBlock(self, config_entry, from_cmdline):
	# Nothing MUST start a block on the command line.
	if from_cmdline:
	    return 0
	else:
	    return config_entry[-2]

    def MustEndBlock(self, config_entry, from_cmdline):
	# However, on the command line, -c must end a block.
	# See the backward compatibiliy notes for the ProcessContainer class.
	if from_cmdline:
	    return config_entry[1] == "-c"
	else:
	    return 0

    def Run(self):
        some_alive = 1
        while some_alive:
            p_info = []
            for p in self.processes:
                if p.alive:
                    if p.in_handle is not None:
                        p_info.append((p, p.in_handle))
                    if p.err_handle is not None:
                        p_info.append((p, p.err_handle))
            if not p_info:
                break
            else:
                res_info = self._CheckInput(p_info)
                for p, handle, num_bytes in res_info:
                    p.ReadCallback(handle, num_bytes)

    def EnforceShutdown(self, process_handles):
	self._EnforceShutdown(process_handles, really_kill = 1)

    def ExecutableExists(self, executable):
        return self._ExecutableExists(executable)


# This class controls a set of process sets. This 
# contains the global keyword/value substitution environment.
# It also figures out how to process the command line and
# configuration file.

import getopt

ConfigurationError = "ConfigurationError"

# INCLUDE: is also an option.

class ProcessEnvironment(pm_core.ProcessEnvironment):
    
    config_table = [("EXPAND:", None, 1, "AddDirectoryExpansion", 0),
                    ("TEMPFILE:", None, 1, "AddTempFile", 0),
		    ("ARGUMENTS:", None, 1, "AddArguments", 0),
		    ("SUPPRESS_EXPANSIONS:", None, 0, "SuppressExpansions", 0)]
    
    def __init__(self, container_set_class = ProcessContainerSet,
		 container_class = ProcessContainer):
	self.container_set_class = container_set_class
	self.container_class = container_class
	self.expansions = []
	self.process_sets = []
	self.config_choice = None
	self.file = "<cmdline>"
	self.suppress_expansions = 0
	self.files_to_remove = []
	self.initialized = 0
	self.tempfiles_created = 0
        self.num_conforming_process_sets = 0
        
	self.AddDirectoryExpansion("$GC_HOME " + os.environ["GC_HOME"])
        archos = self.ComputeArchos(os.environ["GC_HOME"])
        self.AddDirectoryExpansion("$ARCHOS " + archos)
        # Finally, I want to add the OSTYPE, so I can distinguish
        # coarsely between platform types. Values will be
        # values of os.name, e.g., posix, nt.
        self.AddDirectoryExpansion("$OSTYPE " + os.name)
        # Add config file data. We're now generating a values
        self.requirements_met = ["GC_HOME", "ARCHOS", "OSTYPE"] 
        # file, which means we don't have to parse the Makefile.
        try:
            value_fp = open(os.path.join(os.environ["GC_HOME"], "templates", archos, "config.values"), "r")
            for line in value_fp.readlines():
                l = string.strip(line)
                if l and l[0] != "#" and (len(string.split(line)) > 1):
                    # Don't record it if there's no value. This can happen
                    # in config.make.
                    self.AddDirectoryExpansion("$"+l)
                    self.requirements_met.append(string.upper(string.split(l)[0]))
        except IOError:
            pass
        
    def ComputeArchos(self, gc_home):
        return self._ComputeArchos(gc_home)
	
    def DigestCommandLine(self, command_line,
                          accept_args = 1, accept_config_file = 1):
	expanded_config = self._AssembleConfiguration()
	optlist, from_cmdline = self._DigestConfiguration(command_line,
							  expanded_config,
							  accept_args,
							  accept_config_file)
	self._Configure(optlist, from_cmdline, expanded_config)
	for s in self.process_sets:
	    for p in s.processes:
		p.ConfigDone()        
        for p_set in self.process_sets:
            if not p_set.UnmetRequirements():
                self.num_conforming_process_sets = 1 + self.num_conforming_process_sets
                p_set.meets_requirements = 1
            else:
                p_set.meets_requirements = 0
	
    # Config table method.
    
    def AddDirectoryExpansion(self, instructions):
	# SAM 11/30/01: Special case where instructions
	# of length 1 generate a temp file which is
	# instantiated at the beginning and removed
	# at the end.
        # SAM 7/23/02: I've decided to allow values
        # with whitespace.
	l = string.split(instructions, None, 1)
	# Also, make sure that when you record the expansion,
	# YOU USE THE PREVIOUS EXPANSIONS.
        if len(l) > 2:
            raise ConfigurationError, "Ill-formed EXPAND: directive"
        elif len(l) == 2:
	    self._IAddDirectoryExpansion(l[0], self._CleanupExpansion(l[1]))
	else:
            self.expansions.append((l[0], ""))

    def AddTempFile(self, instructions):
        l = string.split(instructions, None, 1)
        if len(l) != 1:
            raise ConfigurationError, "Ill-formed TEMPFILE: directive"
        self.expansions.append((l[0], None))

    # For tempfiles.
    
    def Initialize(self):
	if self.initialized: return
	new_exps = []
	for key, val in self.expansions:
	    if val is None:
		self.tempfiles_created = 1
		f = tempfile.mktemp()
		self.files_to_remove.append(f)
		new_exps.append((key, self._CleanupExpansion(f)))
	    else:
		new_exps.append((key, val))
	self.expansions = new_exps
	self.initialized = 1
	
    def Cleanup(self):
	for f in self.files_to_remove:
	    try:
		os.unlink(f)
	    except: pass
	self.files_to_remove = []

    def _IAddDirectoryExpansion(self, entry, segment):
	self.expansions.append((entry, self.ExpandFile(segment)))

    def ExpandFile(self, file_string):
	for key, val in self.expansions:
	    # val is None if we haven't generated a
	    # temporary file yet.
	    if val is not None:
		file_string = string.replace(file_string, key, val)
	return file_string

    def AddProcessSet(self, set):
	self.process_sets.append(set)

    def SuppressExpansions(self):
	# If expansions are suppressed, then the command which
	# is shown will be expanded only when we execute.
	# Otherwise, it's expanded before it's ever saved
	# as a command line. 
	self.suppress_expansions = 1

    def AddArguments(self, instructions):
	# If there are args, and they don't match the
	# available remaining arguments, then it's an error.
	# No optional arguments permitted.
	config_args = string.split(instructions)
	# Well, we've already recorded the arguments in the
	# directory expansion table. So for each argument,
	# we should be able to find an entry. If we
	# can't, do usage and raise an error.
	num_args = len(config_args)
	j = 1
	while j <= num_args:
	    num_str = "$%d" % j
	    expansion = self.ExpandFile(num_str)
	    if expansion == num_str:
		# No expansion.
		raise ConfigurationError, ("missing some config args: -- %s" % instructions)
	    # Otherwise, record the expansion.
	    self._IAddDirectoryExpansion(("$"+config_args[j - 1]), expansion)
	    j = j + 1
	# Now, see if there's an expansion for the next index.
	num_str = "$%d" % j
	expansion = self.ExpandFile(num_str)
	if expansion != num_str:
	    print "Ignoring extra config arguments"
	
    # Configuration processing.
    
    def _AssembleConfiguration(self):
	expanded_configs_dict = {}
	self._AddConfigTable(self.__class__, expanded_configs_dict)
	self._AddConfigTable(self.container_set_class, expanded_configs_dict)
	self._AddConfigTable(self.container_class, expanded_configs_dict)
	return expanded_configs_dict
    
    def _AddConfigTable(self, config_class, expanded_configs):
	for c in config_class.config_table:
	    if c[0] is not None:
		expanded_configs[c[0]] = c + (config_class,)
	    if c[1] is not None:
		expanded_configs[c[1]] = c + (config_class,)
		
    def _DigestConfiguration(self, command_line, expanded_config_dict,
			     accept_args = 1, accept_config_file = 1):
	short_args = ""
	long_args = []
	# Just look at the entries which are the command line.
	for key, e in expanded_config_dict.items():
	    # e[1] is the command line.
	    if e[1] is None: continue
	    if key != e[1]: continue
	    if (len(key) == 2) and \
	       (key[0] == "-") and \
	       (key[1] != "-"):
		# Short arg.
		if e[2]: short_args = short_args + key[1] + ":"
		else: short_args = short_args + key[1]
	    elif (len(key) > 2) and \
		 (key[0:2] == "--"):
		# Long arg.
		if e[2]: long_args.append(key[2:] + "=")
		else: long_args.append(key[2:])
	    else:
		print "Unknown arg type %s; skipping." % key
	try:
	    optlist, args = getopt.getopt(command_line, short_args, long_args)
	except getopt.error:
	    raise ConfigurationError, "error parsing command line"
	if optlist and args:
	    raise ConfigurationError, "can't have both command line arguments and configuration file"
	if optlist:
	    if (not accept_args):
		raise ConfigurationError, "can't use command line arguments"
	    else:
		return optlist, 1
	elif args:
	    if (not accept_config_file):
		raise ConfigurationError, "can't use configuration file"
	    # Now, we "parse" the file.
	    if '--' in args:
		i = args.index('--')
		# Add these things as directory expansions.
		j = 1
		for elt in args[i+1:]:		    
		    self._IAddDirectoryExpansion(("$%d" % j), "'"+self._CleanupExpansion(elt)+"'")
		    j = j + 1
		args = args[:i]
	    if len(args) == 2:
		self.config_choice = args[1]
	    self.file = args[0]
            return self._DigestConfigurationFile(self.file), 0        
	else:
	    raise ConfigurationError, "neither args nor configuration file"
        
    def _DigestConfigurationFile(self, test_file):
        try:
            fp = open(test_file, "r")
        except:
            raise ConfigurationError, "couldn't find configuration file "+ test_file
        lines = fp.readlines()
        fp.close()
        directives = []
        for line in lines:
            l = string.strip(line)
            if l and l[0] != '#':
                # Empty lines don't count, # is comment character.
                try:
                    d = string.split(l, None, 1)
                    # Handle INCLUDE:
                    if d[0] == "INCLUDE:":
                        if len(d) != 2:
                            raise ConfigurationError, "INCLUDE: requires exactly one argument"
                        else:
                            directives = directives + self._DigestConfigurationFile(self.ExpandFile(d[1]))
                    elif len(d) == 1:
                        directives.append((d[0], ""))
                    else:
                        directives.append((d[0], d[1]))
                except ConfigurationError, msg:
                    raise ConfigurationError, msg
                except:
                    raise ConfigurationError, ("ill-formed line `%s'" % l)
        return directives

    def CanStartBlock(self, config_entry, from_cmdline):
	# From the command line, anything can start a
	# set block.
	if from_cmdline:
	    return 1
	else:
	    return (config_entry[-1] == self.container_set_class) and \
		   (config_entry[-2] == 1)

    def MustStartBlock(self, config_entry, from_cmdline):
	# From the command line, nothing MUST start a set block.
	if from_cmdline:
	    return 0
	else:
	    return (config_entry[-1] == self.container_set_class) and \
		   (config_entry[-2] == 1)
	
    def _Configure(self, optlist, from_cmdline, config_dict):
	# So now we work our way through the optlist.
	cur_set = None
	cur_container = None
	for key, val in optlist:
	    cur_set, cur_container = self._ConfigureOption(cur_set, cur_container, key, val, from_cmdline, config_dict)

    def _ConfigureOption(self, cur_set, cur_container,
			 key, val, from_cmdline, config_dict):
	# We ignore directives we don't know about.
	try:
	    config_entry = config_dict[key]
	except:
	    return cur_set, cur_container
	config_key, cmd_key, need_arg, \
		    mname, start_block, kclass = config_entry
	kmethod = None
	if kclass is self.__class__:
	    kmethod = eval("self." + mname)
	    # Treat locally.
	elif kclass is self.container_set_class:
	    if cur_set is None:
		if self.CanStartBlock(config_entry, from_cmdline):
		    cur_set = self.container_set_class(self)
	    elif self.MustStartBlock(config_entry, from_cmdline):
		cur_set = self.container_set_class(self)
	    if cur_set is None:
		raise ConfigurationError, \
		      ("no current set for %s %s" % (key, val))
	    else:
		kmethod = eval("cur_set." + mname)
	elif kclass is self.container_class:
	    if cur_set is None:
		if self.CanStartBlock(config_entry, from_cmdline):
		    cur_set = self.container_set_class(self)
		else:
		    raise ConfigurationError, \
			  "no current set for container"
	    if cur_container is None:
		if cur_set.CanStartBlock(config_entry, from_cmdline):
		    # This needs to be a method because of
		    # backward compatibility problems with
		    # -c vs. PROCESS:.
		    cur_container = self.container_class(cur_set)
	    elif cur_set.MustStartBlock(config_entry, from_cmdline):
		cur_container = self.container_class(cur_set)
	    if cur_container is None:
		raise ConfigurationError, \
		      ("no current container for %s %s" % (key, val))
	    else:
		kmethod = eval("cur_container." + mname)
	    if cur_set.MustEndBlock(config_entry, from_cmdline):
		cur_container = None
	# The method object now embodies the instance,
	# so I don't care about cur_set or cur_container.
	if kmethod:
	    if need_arg:
		kmethod(val)
	    else:
		kmethod()
	return cur_set, cur_container
        
    def ChooseConfig(self):
        p_set = None
        if self.num_conforming_process_sets == 0:
            raise ConfigurationError, "No eligible process sets."
	elif self.num_conforming_process_sets == 1:
            for p in self.process_sets:
                if p.meets_requirements:
                    p_set = p
                    break
	elif (self.num_conforming_process_sets > 1) and \
	     (self.config_choice is None):
            pass
	else:
	    # We should be able to choose the config now based
	    # on the config choice.
	    try:
		index = string.atoi(self.config_choice)
		try:                    
		    p_set = self.process_sets[index - 1]
		except IndexError:
		    raise ConfigurationError, "List index out of range."
	    except ValueError:
		# Try by name.
		for p in self.process_sets:
		    if p.title == self.config_choice:
			p_set = p
                        break
                if p_set is None:
                    raise ConfigurationError, "Couldn't find set named `%s'." % self.config_choice
            if p_set and (p_set.meets_requirements == 0):
                raise ConfigurationError, "Requirements failed: " + string.join(p_set.UnmetRequirements())
        return p_set
