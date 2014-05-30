# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import getopt, string, types, sys

# Inspired by MIT stuff, but I need something less complex.
# One-letter args take single dashes, more than one letter
# are double dashes (so we can use getopt).

# Args = [("a", 1, 0, ("a_string", types.StringType, "foo")),
#         ("bcd", 0, 1, None)]

# means that a is obligatory, not repeatable, and its argument should be
# shown as a_string in the usage message. It must be a string, and
# the default value is foo; bcd takes no argument, and is optional
# and repeatable. If the default value is None, then there's
# no default.

# When you digest the arguments, you get back a dictionary
# where the keys are the arg names, and the values are the 
# number of occurrences for the ones that take no arguments, a
# list of values of the appropriate type for the ones that 
# take arguments and are repeatable, and an individual value
# for the ones which take arguments and are not repeatable.

ArgSpecError = "ArgSpecError"

LegalTypes = [types.StringType, types.IntType, types.FloatType]

class ArgProcessor:
    
    def __init__(self):
	self.short_getopt_args = ""
	self.long_getopt_args = []
	self.argspec_dict = {}
	self.digested_items = None

    # AddArgspec takes a specification of the form above and
    # adds it to the processor in such a way that enables getopt
    # to work and stores the additional information (repeatable,
    # optional), indexed by the argspec itself.
	
    def AddArgspec(self, argspec):
	for argname, opt_status, repeatable, arg in argspec:
	    # First, add the argname to the getopt specs.
	    getopt_arg = argname	    
	    if len(getopt_arg) == 1:
		argspec_arg = "-" + getopt_arg
		if arg is not None:
		    getopt_arg = getopt_arg + ":"
		self.short_getopt_args = self.short_getopt_args + getopt_arg
	    else:
		argspec_arg = "--" + getopt_arg
		if arg is not None:
		    getopt_arg = getopt_arg + "="
		self.long_getopt_args.append(getopt_arg)
	    # Next, guarantee that the type of the arg is
	    # appropriate.
	    if arg is not None:
		if len(arg) != 3:
		    raise ArgSpecError, ("Arg spec must be a triple", arg)
		if arg[1] not in LegalTypes:
		    raise ArgSpecError, ("Arg type not supported", arg[1])
		# Don't check types here, because they have to be strings
		# and I don't want to try to digest them.
		if type(arg[0]) is not type(""):
		    raise ArgSpecError, ("Arg spec name is not a string", arg[0])		
	    if self.argspec_dict.has_key(argspec_arg):
		raise ArgSpecError, ("Arg %s already specified" % argspec_arg)
	    self.argspec_dict[argspec_arg] = (argname, argspec, opt_status, repeatable, arg)

    # Usage() returns a usage string suitable for inserting between the
    # executable name and the non-flag arguments.
    
    def Usage(self):
	str_list = []
	for key, (argname, argspec, obligatory, repeatable, arg) in self.argspec_dict.items():
	    arg_string = key
	    if arg is not None:
		arg_string = arg_string + " " + arg[0]
	    if (not obligatory) and repeatable:
		str_list.append("[" + arg_string + "]*")
	    elif not obligatory:
		str_list.append("[" + arg_string + "]")
	    else:
		str_list.append(arg_string)
		if repeatable:
		    str_list.append("[" + arg_string + "]*")
	return string.joinfields(str_list, " ")

    # DigestArglist populates the processor with the args from the
    # incoming arglist.
    
    def DigestArglist(self):
	self.digested_items = getopt.getopt(sys.argv[1:], self.short_getopt_args, self.long_getopt_args)
	# digested_items is a list of pairs and then a remainder.

    # ArgsForSpec takes one of the argspecs previously used to
    # populate the processor and extracts the relevant args from the
    # input args. It also adds optional args which are not present,
    # complains about obligatory args without defaults which are
    # not present, and adds defaults for obligatory args with defaults
    # which are not present.
	
    def ArgsForSpec(self, argspec_restr):
	res_dict = {}
	if self.digested_items is None:
	    self.DigestArglist()
	for key, value in self.digested_items[0]:
	    argname, argspec, opt_status, repeatable, arg = self.argspec_dict[key]
	    # Only collect those arguments which are relevant.
	    if argspec is not argspec_restr:
		continue
	    # Don't need to check values, because that will
	    # already have been done in getopt.	    
	    if (not repeatable) and res_dict.has_key(argname):
		raise ArgSpecError, ("arg %s cannot be repeated" % key)
	    self.AddValue(res_dict, argname, arg, value, repeatable)
	# Now, collect all the optional nonoccuring args.
	for key, (argname, argspec, obligatory, repeatable, arg) in self.argspec_dict.items():
	    if argspec is not argspec_restr:
		continue
	    # For obligatory arguments with defaults, add if not present.
	    # For obligatory arguments without defaults, barf.
	    if obligatory and (not res_dict.has_key(argname)):
		if (arg is None) or (arg[2] is None):
		    raise ArgSpecError, ("missing obligatory argument %s" % key)
		else:
		    self.AddValue(res_dict, argname, arg, arg[2], repeatable)
	    # For optional arguments with defaults, add if not present.
	    if (not obligatory) and (not res_dict.has_key(argname)):
		if (arg is not None) and (arg[2] is not None):
		    self.AddValue(res_dict, argname, arg, arg[2], repeatable)
	return res_dict

    def Remainder(self):
	if self.digested_items is None:
	    self.DigestArglist()	    
	return self.digested_items[1]

    # AddValue actually adds the args to the result from ArgsForSpec.
    # First, it does type checking, then it incorporates the args.
    # If the doesn't take a value, the number of times the arg is
    # found will be incorporated. If the arg takes a value, the result
    # in the dictionary will be a list if repeatable, a singleton
    # otherwise. The keys in the returned dict are the args without
    # the leading "-" or "--".
    
    def AddValue(self, res_dict, argname, arg, value, repeatable):
	if arg is not None:
	    t = arg[1]
	    if t is types.IntType:
		try:
		    value = string.atoi(value)
		except:
		    raise ArgSpecError, ("value %s is not an int" % value)
	    elif t is types.FloatType:
		try:
		    value = string.atof(value)
		except:
		    raise ArgSpecError, ("value %s is not a float" % value)
	if repeatable and (arg is None):
	    if res_dict.has_key(argname):
		res_dict[argname] = res_dict[argname] + 1
	    else:
		res_dict[argname] = 1
	elif repeatable and (arg is not None):
	    if res_dict.has_key(argname):
		res_dict[argname].append(value)
	    else:
		res_dict[argname] = [value]
	elif arg is None:
	    res_dict[argname] = 1
	else:
	    res_dict[argname] = value
