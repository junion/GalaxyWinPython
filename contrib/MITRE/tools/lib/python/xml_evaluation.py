# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# In this file, we set up the scoring infrastructure. This is going
# to use a lot of ideas from the previous evaluation stuff, but 
# we're going to carve it apart differently. I'm not sure, for instance,
# that we'll support rerunning immediately.

# If we record scores, we'll use XML to structure the data (what
# the hell, we're using it for everything else...). It should be
# session-based, and it ought to refer to the URL it scored
# against, rather than cacheing the information itself. The input
# for each directory is the segmented HTML. Each element to report
# is handled by a separate "accumulator".

XMLEvaluationError = "XMLEvaluationError"

import xml_interleave, io_device

# SAM 4/3/00: Major reorganization. It finally occurred to me
# that I need to divide things into Accumulators and ScoreElements.
# Something can be both an Accumulator and a ScoreElement, but 
# fundamentally there's a difference. An Accumulator gathers 
# and harvests information from all the sessions. It can save
# arbitrary tuples of information for each session. A ScoreElement
# gets information out of the accumulator and presents a single
# column of results. These results can be divided into tables, while
# the Accumulator is insensitive to presentation tables.

class Accumulator:
    def __init__(self, score_set = None):
	self.SetScoreSet(score_set)
	# log_dict is a mapping from log_directories
	# to a mapping from tables to ordered values.
	self.log_dict = {}
    def SetScoreSet(self, score_set):
	self.score_set = score_set
    def _SetLogDict(self, dict):
	self.log_dict = dict
    # Called for each log. Each call to Accumulate()
    # should return the appropriate value.
    def Accumulate(self, log_summary):
	raise XMLEvaluationError, "Not implemented"
    def Compute(self, val_list, val_len):
	raise XMLEvaluationError, "Not implemented"
    def _SaveLogValues(self, log_summary):
	self.log_dict[log_summary.log_dir] = self.Accumulate(log_summary)
    def _AggregateLogValues(self, v):
	return self.Compute(filter(lambda x: x is not None, v), len(v))
    def Sum(self, val_list):
	return reduce(lambda x, y: x + y, val_list, 0)
    def Average(self, val_list, val_list_len):
	val_list = map(float, val_list)
	if val_list_len == 0:
	    return 0
	return reduce(lambda x, y: x + y, tuple(val_list))/val_list_len
    def Append(self, val_list):	
	return reduce(lambda x, y: x + y, val_list, [])

# Each score element can return a single column.

class ScoreElement:
    def __init__(self, score_set = None,
		 header = None):
	self.SetScoreSet(score_set)
	# log_dict is a mapping from log_directories
	# to a mapping from tables to ordered values.
	self.log_header = header
	self.accumulators = None
    def SetScoreSet(self, score_set):
	self.score_set = score_set
    def SetAccumulators(self, *accumulators):
	self.accumulators = accumulators
    def FindAccumulators(self):
	raise XMLEValuationError, "Not implemented"
    def RetrieveLogValue(self, log_obj, table):
	raise XMLEValuationError, "Not implemented"
    def RetrieveColumn(self, res_dict):
	for t in self.score_set.tables:
	    col = []
	    accum_input = []
	    use_col = 0	    
	    for l in self.score_set.repository.log_directories:
		vals = map(lambda x, log = l: x.log_dict[log],
			   self.accumulators)
		v = self.RetrieveLogValue(vals, t)
		col.append(v)
		if v is not None:
		    use_col = 1
		    accum_input.append(vals)
	    if not use_col:
		continue
	    if not res_dict.has_key(t):
		res_dict[t] = []
	    res = res_dict[t]
	    # Now, we accumulate the results for all the accumulators.
	    accum_results = []
	    for i in range(len(self.accumulators)):
		accum_results.append(self.accumulators[i]._AggregateLogValues(map(lambda x, j = i: x[j], accum_input)))
	    tot = self.RetrieveLogValue(accum_results, t)
	    res.append([self.log_header] + map(lambda x, s = self: s._ValueToString(x), col) + [self._ValueToString(tot)])
    def _ValueToString(self, v):
	if v is None:
	    return "-"
	else:
	    return self.FormatValue(v)
    def FormatValue(self, v):
	return str(v)

class AccumulatingScoreElement(Accumulator, ScoreElement):
    def __init__(self, score_set = None, header = None):
	Accumulator.__init__(self, score_set)
	ScoreElement.__init__(self, score_set, header)
    def FindAccumulators(self):
	self.SetAccumulators(self)

# Accumulators and presenters are guaranteed to be
# evaluated so that all the accumulators are evaluated first
# in order, and then all the presenters are presented in order.
    
class ScoreSet:
    def __init__(self, repository, accumulators_and_presenters,		 
		 io = None, tables = None):
	self.accumulators = []
	self.presenters = []	
	for elt in accumulators_and_presenters:
	    if isinstance(elt, Accumulator):
		self.accumulators.append(elt)
		elt.SetScoreSet(self)
	    if isinstance(elt, ScoreElement):
		self.presenters.append(elt)
		elt.SetScoreSet(self)
	for p in self.presenters:
	    p.FindAccumulators()
	self.repository = repository
	self.io = io
	if self.io is None:
	    if self.repository:
		self.io = self.repository.io
	    else:
		self.io = io_device.DefaultIODevice()	
	if tables is not None:
	    self.tables = tables
	else:
	    self.tables = []
	self.task_range = self.default_task_range = xml_interleave.WITHIN_TOTAL_TASK
    def FindAccumulator(self, aclass):
	for a in self.accumulators:
	    if isinstance(a, aclass):
		return a
	return None
    # For a single repository, we want to segment and
    # call SaveLogValues() for each log entry. This method 
    # will be called from the Score() log method.
    def Score(self, log_summary, task_range = None):
	if task_range is not None:
	    self.task_range = task_range
	for a in self.accumulators:
	    a._SaveLogValues(log_summary)
    def Present(self, formatter):
	# Assuming the runs have already happened.
	# We collect all the columns, including a column for the
	# log dirs.
	col_seeds = map(lambda x: x.path.PathString(), self.repository.log_directories)
	cols = [[""] + col_seeds + ["Overall"]]
	res_dict = {}
	for a in self.presenters:
	    a.RetrieveColumn(res_dict)
	# Now, we turn it into rows.
	for table_name in self.tables:
	    if res_dict.has_key(table_name):
		self.PresentTable(cols, table_name,
				  res_dict[table_name], formatter)
    def PresentTable(self, col_seed, table_title, cols, formatter):
	cols = col_seed + cols
	rows = []
	# I want to prune out the rows which are empty.
	headers = map(lambda x: x[0], cols)
	for i in range(len(cols[0]) - 1):
	    row = map(lambda x, j = i: x[j + 1], cols)
	    use_it = 0
	    for elt in row[1:]:
		# This is the value None translates into.
		if elt != "-":
		    use_it = 1
		    break
	    if use_it:
		rows.append(map(lambda x, j = i: x[j + 1], cols))
	if rows:
	    # The overall repository doesn't have a formatter, because
	    # the formatters sometimes need to be implemented with
	    # log-specific information for anchors. So we pass one in.
	    self.io.PrintTitle(table_title, formatter)
	    self.io.PrintTable(headers, rows, formatter)
