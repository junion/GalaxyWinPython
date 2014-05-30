# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This class provides an interface for IO devices for
# both the evaluator and the log reader.

import sys, string

PromptFailed = "PromptFailed"

# The default IO device flushes its output so that
# if it's running under subprocess control, the right
# things happen.

class DefaultIODevice:
    def __init__(self, file = None, fp = None):
	self.close = 0
	self.fp = fp
	if file is not None:
	    if file == "-":
		self.fp = sys.stdout
	    else:
		self.fp = open(file, "w")
		self.close = 1
	if not self.fp:
	    self.fp = sys.stdout
    def Close(self):
	if self.close:
	    self.fp.close()
    def ReportStatus(self, str, add_newline = 1):
	if add_newline: str = str + "\n"
	self.fp.write(str)
	self.fp.flush()
    def ReportOutput(self, str, add_newline = 1):
	if add_newline: str = str + "\n"
	self.fp.write(str)
	self.fp.flush()
    def ReportError(self, str, add_newline = 1):
	if add_newline: str = str + "\n"
	self.fp.write("Error: " + str)
	self.fp.flush()
    def EchoOff(self):
	pass
    def EchoOn(self):
	pass
    def Prompt(self, msg):
	if self.fp is not sys.stdin:
	    pass
	self.ReportStatus(msg, add_newline = 0)
	return string.strip(sys.stdin.readline())
    def PrintString(self, str, formatter):
	str = formatter.FormatString(str)
	self.fp.write(str + "\n")
	self.fp.flush()
    def PrintTitle(self, t, formatter):
	title = formatter.FormatTitle(t)
	self.fp.write(title)
	self.fp.flush()
    def PrintTable(self, headers, rows, formatter):
	t = formatter.FormatTable(headers, rows)
	self.fp.write(t)
	self.fp.flush()
