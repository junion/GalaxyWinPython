# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This file specializes the evaluation for the logfile DMAs.

from xml_evaluation import ScoreElement, ScoreSet, \
     Accumulator, AccumulatingScoreElement
from xml_interleave import COLLAPSED_TURNS, NO_TURNS, \
     WITHIN_ALL_TURNS, WITHIN_TOTAL_TASK, WITHIN_ON_TASK,TurnSummary

import string, url_directory

# Here we define the score elements for the DMA scoring.

# We're going to rely on three major basic collections. 
# (1) task completion information
# (2) task duration, broken down by complete/incomplete
# (3) number of turns, broken down by complete/incomplete

from GC_log_formatting import StartTask, EndTask, NewTurn, \
     TaskCompletion, SystemText, UserSR, UserTranscription, \
     SystemAudioStart, SystemAudioEnd, UserAudioStart, UserAudioEnd, \
     SystemAudioFile, UserAudioFile, ErrorMessage, HelpMessage, \
     SRHypothesis, InputParaphrase, ConceptInput, SystemPrompt, \
     ConceptAnnotation

# This utility computes scores according to task completion.

def AddTaskCompletion(completion, score_val, table_name):
    if completion == 1 and table_name in ["Complete sessions", \
					  "All sessions"]:
	return score_val
    elif completion == 0 and table_name in ["Incomplete sessions", \
					    "All sessions"]:
	return score_val
    elif table_name == "All sessions":
	return score_val
    else:
	return None

# The Accumulate() method now assigns results to columns.

class DMATaskCompletion(AccumulatingScoreElement):
    def __init__(self, score_set = None):
	AccumulatingScoreElement.__init__(self, score_set,
					  "Task completed")
    # Accumulator methods.
    def Accumulate(self, log_summary):
	complete = log_summary.FindInterestingElements([TaskCompletion])[0]
	if complete:
	    # "1" or "0".
	    return string.atoi(complete[0][1].GetValue())
	else:
	    return None
    def Compute(self, val_list, val_list_len):
	return self.Average(val_list, len(val_list))
    # ScoreElement methods.
    def RetrieveLogValue(self, vals, table_name):
	complete = vals[0]
	if table_name == "All sessions":
	    return complete
	else:
	    return None
    def FormatValue(self, v):
	if type(v) is type(0):
	    return `v`
	else:
	    return "%.2f" % v

class DMATaskDurationAccumulator(Accumulator):
    def Accumulate(self, log_summary):
	task_endpoints = log_summary.FindInterestingElements([StartTask, EndTask])[0]
	task_ends = []
	on_task_s_time = None
	total_task_s_time = None
	on_task_e_time = None
	total_task_e_time = None
	for eclass, elt in task_endpoints:
	    if eclass is StartTask:
		if elt.GetValue() == "task":
		    on_task_s_time = elt.GetTimes()[0]
		elif elt.GetValue() == "total":
		    total_task_s_time = elt.GetTimes()[0]
		else:
		    # "true"
		    on_task_s_time = total_task_s_time = elt.GetTimes()[0]
	    else:
		# Class is EndTask
		if elt.GetValue() == "task":
		    on_task_e_time = elt.GetTimes()[1]
		elif elt.GetValue() == "total":
		    total_task_e_time = elt.GetTimes()[1]
		else:
		    # "true"
		    on_task_e_time = total_task_e_time = elt.GetTimes()[1]
	# Now we get return status
	on_task_time = total_task_time = None
	if (on_task_s_time is not None) and \
	   (on_task_e_time is not None):
	    on_task_time = on_task_e_time - on_task_s_time
	if (total_task_s_time is not None) and \
	   (total_task_e_time is not None):
	    total_task_time = total_task_e_time - total_task_s_time
	return (on_task_time, total_task_time)
    def Compute(self, val_list, val_list_len):
	# Either dimension of each pair might be None.
	starts = filter(lambda x: x is not None,
			map(lambda x: x[0], val_list))
	ends = filter(lambda x: x is not None,
		      map(lambda x: x[1], val_list))
	return (self.Average(starts, len(starts)),
		self.Average(ends, len(ends)))

class DMATaskDurationPresenter(ScoreElement):
    def __init__(self, score_set = None):
	ScoreElement.__init__(self, score_set, "Duration (secs)")
    def FindAccumulators(self):
	self.SetAccumulators(self.score_set.FindAccumulator(DMATaskDurationAccumulator), self.score_set.FindAccumulator(DMATaskCompletion))
    def RetrieveLogValue(self, vals, table_name):
	if self.score_set.task_range == WITHIN_ALL_TURNS:
	    return None
	# The first dimension.
	task_time_pair = vals[0]
	if task_time_pair is None:
	    task_time = None
	elif self.score_set.task_range == WITHIN_TOTAL_TASK:
	    task_time = task_time_pair[1]
	elif self.score_set.task_range == WITHIN_ON_TASK:	    
	    task_time = task_time_pair[0]
	else:
	    task_time = None
	completion = vals[1]
	return AddTaskCompletion(completion, task_time, table_name)
    def FormatValue(self, v):
	return "%.2f" % v

class DMATurnsToCompletion(AccumulatingScoreElement):
    def __init__(self, score_set = None):
	AccumulatingScoreElement.__init__(self, score_set,
					  "Turns in interval")
    # Accumulator methods.
    def Accumulate(self, log_summary):
	# The DMA specifies that consecutive turns from the
	# same "side" should be collapsed.
	global_matches = log_summary.FindInterestingElements([NewTurn], task_range = self.score_set.task_range)[0]
	# We already have code to collapse turns. We have to
	# munge this list into a form where we can call it.
	turn_subset = map(lambda x: [TurnSummary(x[1], 0, 0), []],
			  global_matches)
	turn_subset = log_summary.CollapseTurns(turn_subset)
 	return len(turn_subset)
    def Compute(self, val_list, val_list_len):
	return self.Average(val_list, len(val_list))
    # ScoreElement methods.
    def FindAccumulators(self):
	self.SetAccumulators(self, self.score_set.FindAccumulator(DMATaskCompletion))
    def RetrieveLogValue(self, vals, table_name):
	l = vals[0]
	completion = vals[1]
	# Now we get return status
	return AddTaskCompletion(completion, l, table_name)
    def FormatValue(self, v):
	if type(v) is type(0):
	    return `v`
	else:
	    return "%.2f" % v

def _TabulateText(accumulator, log_summary, pclass, postprocessor = None):
    # These DMAs rely on the collapsed turn definition.
    # So we collect number of turns, number of utterances,
    # number of words. Number of utts is reported so
    # that we can be parasitic on it for an expanded set.
    total_words = total_utts = total_turns = 0
    for [turn, transc] in log_summary.FindInterestingElements([pclass], turn_segmentation = COLLAPSED_TURNS, task_range = accumulator.score_set.task_range)[1]:
	if transc:
	    total_turns = total_turns + 1
	for eclass, tr in transc:
	    total_utts = total_utts + 1
	    # Find the data.
	    d = tr.GetData()
	    if postprocessor:
		d = postprocessor.Process(d)
	    total_words = total_words + len(string.split(d))
    return total_words, total_utts, total_turns

class _DMAWordsPerTurn(AccumulatingScoreElement):
    # Accumulator methods.
    def Compute(self, val_list, val_list_len):
	total_words = self.Sum(map(lambda y: y[0], val_list))
	total_utts = self.Sum(map(lambda y: y[1], val_list))
	total_turns = self.Sum(map(lambda y: y[2], val_list))
	return (total_words, total_utts, total_turns)
    # ScoreElement methods.
    def RetrieveLogValue(self, vals, table_name):
	if table_name == "All sessions":
	    v = vals[0]
	    if v is None:
		return None
	    if v[2] == 0:
		return None
	    else:
		return float(v[0])/float(v[2])
	else:
	    return None
    def FormatValue(self, v):
	return "%.2f" % v

class DMAUserWordsPerTurn(_DMAWordsPerTurn):
    def __init__(self, score_set = None):
	_DMAWordsPerTurn.__init__(self, score_set,
				  "Mean user words per turn (noise stripped)")
    def Accumulate(self, log_summary):
	return _TabulateText(self, log_summary, UserTranscription,
			     self.score_set.ref_post)

class DMASystemWordsPerTurn(_DMAWordsPerTurn):
    def __init__(self, score_set = None):
	_DMAWordsPerTurn.__init__(self, score_set,
				  "Mean system words per turn")
    def Accumulate(self, log_summary):
	return _TabulateText(self, log_summary, SystemText)

class DMAErrorMessageCount(AccumulatingScoreElement):
    def __init__(self, score_set = None):
	AccumulatingScoreElement.__init__(self, score_set, "Error messages")
    # Accumulator methods.
    def Accumulate(self, log_summary):	
	return _TabulateText(self, log_summary, ErrorMessage)[1]    
    def Compute(self, val_list, val_list_len):
	return self.Average(val_list, len(val_list))
    # ScoreElement methods.
    def FindAccumulators(self):
	self.SetAccumulators(self, self.score_set.FindAccumulator(DMATaskCompletion))
    def RetrieveLogValue(self, vals, table_name):
	v = vals[0]
	completion = vals[1]    
	return AddTaskCompletion(completion, v, table_name)
    def FormatValue(self, v):
	if type(v) is type(0):
	    return `v`
	else:
	    return "%.2f" % v

class DMAHelpMessageCount(AccumulatingScoreElement):
    def __init__(self, score_set = None):
	AccumulatingScoreElement.__init__(self, score_set, "Help messages")
    # Accumulator methods.
    def Accumulate(self, log_summary):
	return _TabulateText(self, log_summary, HelpMessage)[1]
    def Compute(self, val_list, val_list_len):
	return self.Average(val_list, len(val_list))
    # ScoreElement methods.
    def FindAccumulators(self):
	self.SetAccumulators(self, self.score_set.FindAccumulator(DMATaskCompletion))
    def RetrieveLogValue(self, vals, table_name):
	v = vals[0]
	completion = vals[1]	
	return AddTaskCompletion(completion, v, table_name)
    def FormatValue(self, v):
	if type(v) is type(0):
	    return `v`
	else:
	    return "%.2f" % v

# THIS METRIC IS NOT IMPLEMENTED ACCORDING TO THE DMAS, 
# due to the fact that I can't recognize "pacifiers" yet.

class DMAResponseLatency(AccumulatingScoreElement):
    def __init__(self, score_set = None):
	AccumulatingScoreElement.__init__(self, score_set,
					  "Response latency (mean in secs/variance)")
    # Accumulator methods.
    def Accumulate(self, log_summary):
	# The algorithm is to look, for each user turn, how long
	# afterward the next system turn starts. Since they
	# alternate, we can look at the next item in the list.
	# If there is no next item, skip latency. So don't
	# even look at the last item.
	times = []
	audio_boundaries = log_summary.FindInterestingElements([SystemAudioStart, UserAudioEnd], turn_segmentation = COLLAPSED_TURNS, task_range = self.score_set.task_range)[1]
	for j in range(len(audio_boundaries) - 1):
	    [turn_summary, t] = audio_boundaries[j]
	    turn = turn_summary.turn_anchor
	    if turn.GetValue() == "user":
		all_ends = []
		cur_end = None
		# Get the last end time.
		for eclass, elt in t:
		    if eclass is UserAudioEnd:
			all_ends.append(elt.GetTimes()[1])
		if not all_ends:
		    continue
		else:
		    cur_end = max(all_ends)
		[next_turn, next_t] = audio_boundaries[j + 1]
		cur_start = None
		# Get the first start time. Don't know
		# if they're in order, so collect them all
		# and take the smallest.
		all_starts = []
		for eclass, elt in next_t:
		    if eclass is SystemAudioStart:
			all_starts.append(elt.GetTimes()[0])
		if not all_starts:
		    continue
		else:
		    cur_start = min(all_starts)
		times.append(cur_start - cur_end)
	return times
    def Compute(self, val_list, val_list_len):
	return self.Append(val_list)
    # ScoreElement methods.
    def FindAccumulators(self):
	self.SetAccumulators(self, self.score_set.FindAccumulator(DMATaskCompletion))
    # I want to compute both the mean and the variance.
    # Variance is the average squared deviation from the mean.
    def RetrieveLogValue(self, vals, table_name):
	times = vals[0]
	completion = vals[1]
	if times is None:
	    return None
	elif len(times) == 0:
	    return None
	else:
	    avg = self.Average(times, len(times))
	    # Now, compute variance.
	    # First, create all the differences, each datapoint
	    # minus the avg, and square them.
	    sqdiffs = map(lambda x, y = avg: (x - y) ** 2, times)
	    # Now the sum of squares, divided by the number of
	    # observations. This amounts to the average of
	    # the sqdiffs.
	    variance = self.Average(sqdiffs, len(times))
	    return AddTaskCompletion(completion, (avg, variance), table_name)
    def FormatValue(self, v):
	return "%.2f/%.4f" % v

class DMAUserWordsToCompletion(ScoreElement):
    def __init__(self, score_set = None):	
	ScoreElement.__init__(self, score_set, "User words (noise stripped)")
    def FindAccumulators(self):
	self.SetAccumulators(self.score_set.FindAccumulator(DMAUserWordsPerTurn), self.score_set.FindAccumulator(DMATaskCompletion))
    def RetrieveLogValue(self, vals, table_name):
	turn_entry = vals[0]
	if turn_entry is None:
	    v = None
	else:
	    v = turn_entry[0]
	completion = vals[1]
	return AddTaskCompletion(completion, v, table_name)
    def FormatValue(self, v):
	if type(v) is type(0):
	    return `v`
	else:
	    return "%.2f" % v

class DMASystemWordsToCompletion(ScoreElement):
    def __init__(self, score_set = None):
	ScoreElement.__init__(self, score_set, "System words")
    def FindAccumulators(self):
	self.SetAccumulators(self.score_set.FindAccumulator(DMASystemWordsPerTurn), self.score_set.FindAccumulator(DMATaskCompletion))
    def RetrieveLogValue(self, vals, table_name):
	turn_entry = vals[0]
	if turn_entry is None:
	    v = None
	else:
	    v = turn_entry[0]
	completion = vals[1]
	return AddTaskCompletion(completion, v, table_name)
    def FormatValue(self, v):
	if type(v) is type(0):
	    return `v`
	else:
	    return "%.2f" % v

# Here, we collect the prompts. I'm going to compute two metrics here.
# The first is the percentage which are prompts. The second is if the
# prompt is a reprompt.

class DMAPromptPercentage(AccumulatingScoreElement):
    def __init__(self, score_set = None):
	AccumulatingScoreElement.__init__(self, score_set, "Prompt percentage")
    # Accumulator methods.
    def Accumulate(self, log_summary):	
	total_utts = _TabulateText(self, log_summary, SystemText)[1]
	# Just get a flat list of turns.
	if total_utts == 0:
	    return None
	else:
	    prompts = log_summary.FindInterestingElements([SystemPrompt], turn_segmentation = NO_TURNS, task_range = self.score_set.task_range)[1]
	    return len(prompts), total_utts
    def Compute(self, val_list, val_list_len):
	return (self.Sum(map(lambda x: x[0], val_list)),
		self.Sum(map(lambda x: x[1], val_list)))
    # ScoreElement methods.
    def FindAccumulators(self):
	self.SetAccumulators(self, self.score_set.FindAccumulator(DMATaskCompletion))
    def RetrieveLogValue(self, vals, table_name):
	prompt_pair = vals[0]
	if prompt_pair is None:
	    v = None
	else:
	    prompts = prompt_pair[0]
	    total_utts = prompt_pair[1]
	    if total_utts == 0:
		v = None
	    else:
		v = float(prompts) / float(total_utts)
	completion = vals[1]
	return AddTaskCompletion(completion, v, table_name)
    def FormatValue(self, v):
	return "%.2f" % v

class DMARepromptCount(AccumulatingScoreElement):
    def __init__(self, score_set = None):
	AccumulatingScoreElement.__init__(self, score_set, "Number of reprompts")
    # Accumulator methods.
    def Accumulate(self, log_summary):
	# Just get a flat list of turns.
	prompts = log_summary.FindInterestingElements([SystemPrompt], turn_segmentation = NO_TURNS, task_range = self.score_set.task_range)[1]
	reprompts = 0
	if prompts:
	    cur_prompt = prompts[0][1].GetValue()
	    for eclass, prompt in prompts[1:]:
		next_prompt = prompt.GetValue()
		if cur_prompt == next_prompt:
		    reprompts = reprompts + 1
		cur_prompt = next_prompt
	return reprompts
    def Compute(self, val_list, val_list_len):
	return self.Average(val_list, len(val_list))
    # ScoreElement methods.
    def FindAccumulators(self):
	self.SetAccumulators(self, self.score_set.FindAccumulator(DMATaskCompletion))
    def RetrieveLogValue(self, vals, table_name):
	reprompts = vals[0]
	completion = vals[1]	
	return AddTaskCompletion(completion, reprompts, table_name)
    def FormatValue(self, v):
	if type(v) is type(0):
	    return `v`
	else:
	    return "%.2f" % v

# Here, we collect the start and end times of the utterances.
# So the system turn length can be parasitic on the utterance length.
# Well, that didn't quite work, since I hadn't divided the 
# logs into utterances...

class DMAMeanSystemUtteranceLength(AccumulatingScoreElement):
    def __init__(self, score_set = None):
	AccumulatingScoreElement.__init__(self, score_set,
					  "Mean system utterance duration (secs)")
    # Accumulator methods.
    def Accumulate(self, log_summary):
	pairs = []
	audio_boundaries = log_summary.FindInterestingElements([SystemAudioStart, SystemAudioEnd], task_range = self.score_set.task_range)[1]
	for [turn_summary, t] in audio_boundaries:
	    cur_start = None
	    for eclass, elt in t:
		if eclass is SystemAudioStart:
		    cur_start = elt.GetTimes()[0]
		elif eclass is SystemAudioEnd:
		    if cur_start is not None:
			e_time = elt.GetTimes()[1]
			if e_time is not None:
			    pairs.append((cur_start, e_time))
	if len(pairs) == 0:
	    return None
	return pairs    
    def Compute(self, val_list, val_list_len):
	return self.Append(val_list)
    # ScoreElement methods.
    def RetrieveLogValue(self, vals, table_name):
	if table_name == "All sessions":
	    v = vals[0]
	    if v is None:
		return None
	    else:
		lens = map(lambda x: x[1] - x[0], v)
		if len(lens) == 0:
		    return None
		else:
		    return reduce(lambda x, y: x + y, tuple(lens))/len(lens)
	else:
	    return None
    def FormatValue(self, v):
	return "%.2f" % v
 
# This metric requires collapsing the turns. We also collapse 
# silence in here.

class DMASystemTurnAccumulator(Accumulator):
    def Accumulate(self, log_summary):
	pairs = []
	for [turn_summary, t] in log_summary.FindInterestingElements([SystemAudioStart, SystemAudioEnd], turn_segmentation = COLLAPSED_TURNS, task_range = self.score_set.task_range)[1]:	    
	    cur_start = None
	    e_time = None
	    total_silence = 0
	    for eclass, elt in t:
		if eclass is SystemAudioStart:
		    local_start = elt.GetTimes()[0]
		    if cur_start is None:
			cur_start = local_start		    
		    if e_time is not None:
			total_silence = total_silence + \
					(local_start - e_time)
		elif eclass is SystemAudioEnd:
		    if cur_start is not None:
			e_time = elt.GetTimes()[1]
	    if (cur_start is not None) and \
	       (e_time is not None):
		pairs.append((cur_start, e_time, total_silence))
		cur_start = None
	return pairs
    def Compute(self, val_list, val_list_len):
	return self.Append(val_list)

class DMAMeanSystemTurnLength(ScoreElement):
    def __init__(self, score_set = None):	
	ScoreElement.__init__(self, score_set, "Mean system turn duration (secs)")
    def FindAccumulators(self):
	self.SetAccumulators(self.score_set.FindAccumulator(DMASystemTurnAccumulator))
    def RetrieveLogValue(self, vals, table_name):
	if table_name == "All sessions":
	    v = vals[0]
	    if v is None:
		return None
	    else:
		lens = map(lambda x: x[1] - x[0], v)
		if len(lens) == 0:
		    return None
		else:
		    return reduce(lambda x, y: x + y, tuple(lens))/len(lens)
	else:
	    return None
    def FormatValue(self, v):
	return "%.2f" % v

class DMAMeanSystemTurnSilence(ScoreElement):
    def __init__(self, score_set = None):	
	ScoreElement.__init__(self, score_set, "Mean system turn silence (secs)")
    def FindAccumulators(self):
	self.SetAccumulators(self.score_set.FindAccumulator(DMASystemTurnAccumulator))
    def RetrieveLogValue(self, vals, table_name):
	if table_name == "All sessions":
	    v = vals[0]
	    if v is None:
		return None
	    else:
		lens = map(lambda x: x[2], v)
		# Now, I want the average of these.
		if len(lens) == 0:
		    return None
		else:
		    return reduce(lambda x, y: x + y, tuple(lens))/len(lens)
	else:
	    return None
    def FormatValue(self, v):
	return "%.2f" % v

class DMASystemTurnSilencePercentage(ScoreElement):
    def __init__(self, score_set = None):	
	ScoreElement.__init__(self, score_set, "System turn silence pct")
    def FindAccumulators(self):
	self.SetAccumulators(self.score_set.FindAccumulator(DMASystemTurnAccumulator))
    def RetrieveLogValue(self, vals, table_name):
	if table_name == "All sessions":
	    v = vals[0]
	    if v is None:
		return None
	    else:
		all_silences = map(lambda x: x[2], v)
		all_lens = map(lambda x: x[1] - x[0], v)
		# Now, I want the average of these.
		if len(all_lens) == 0:
		    return None
		else:
		    return reduce(lambda x, y: x + y, tuple(all_silences))/reduce(lambda x, y: x + y, tuple(all_lens))
	else:
	    return None
    def FormatValue(self, v):
	return "%.2f" % v
	

# Accumulators are guaranteed to be evaluated in order. So
# for instance, TaskCompletion will happen first.

# In order to invoke the DMAScoreSet, I need to actually do
# everything three times: once for each interval (all turns,
# total task, on task). So I need to be aware of the tables
# DURING COLLECTION, not just presentation.

class DMAScoreSet(ScoreSet):
    default_task_range = None
    def __init__(self, repository):
	self.all_turn_scores = None
	self.total_task_scores = None
	self.on_task_scores = None
	self.ref_post = None
	ScoreSet.__init__(self, repository,
			  [DMATaskCompletion(),
			   DMATaskDurationAccumulator(),
			   DMATaskDurationPresenter(),
			   DMATurnsToCompletion(),			   
			   DMAUserWordsPerTurn(), DMASystemWordsPerTurn(),
			   DMAErrorMessageCount(), DMAHelpMessageCount(),
			   DMAResponseLatency(),			   
			   DMAUserWordsToCompletion(),
			   DMASystemWordsToCompletion(),
			   DMAPromptPercentage(), DMARepromptCount(),
			   DMAMeanSystemUtteranceLength(),
			   DMASystemTurnAccumulator(),
			   DMAMeanSystemTurnLength(),
			   DMAMeanSystemTurnSilence(),
			   DMASystemTurnSilencePercentage()
			   ],
			  tables = ["Complete sessions",
				    "Incomplete sessions",
				    "All sessions"])
	self.task_range = self.default_task_range = None
    def Score(self, log_summary, task_range = None):
	# Now for on task.
	if (task_range is None) or (task_range == WITHIN_ON_TASK):
	    if self.on_task_scores is None:
		# Initialize.
		map(lambda x: x._SetLogDict({}), self.accumulators)
	    else:
		map(lambda x, y: x._SetLogDict(y),
		    self.accumulators, self.on_task_scores)
	    ScoreSet.Score(self, log_summary, task_range = WITHIN_ON_TASK)
	    self.on_task_scores = map(lambda x: x.log_dict, self.accumulators)
	# Now for the total task
	if (task_range is None) or (task_range == WITHIN_TOTAL_TASK):
	    if self.total_task_scores is None:
		map(lambda x: x._SetLogDict({}), self.accumulators)
	    else:
		map(lambda x, y: x._SetLogDict(y),
		    self.accumulators, self.total_task_scores)
	    ScoreSet.Score(self, log_summary, task_range = WITHIN_TOTAL_TASK)
	    self.total_task_scores = map(lambda x: x.log_dict, self.accumulators)
	# Now for all turns.
	if (task_range is None) or (task_range == WITHIN_ALL_TURNS):
	    if self.all_turn_scores is None:
		map(lambda x: x._SetLogDict({}), self.accumulators)
	    else:
		map(lambda x, y: x._SetLogDict(y),
		    self.accumulators, self.all_turn_scores)
	    ScoreSet.Score(self, log_summary, task_range = WITHIN_ALL_TURNS)
	    self.all_turn_scores = map(lambda x: x.log_dict, self.accumulators)
	
    def Present(self, formatter):
	if self.on_task_scores is not None:
	    # I'm going to generate the tables by swapping
	    # in the data, and then I'll shuffle them.
	    self.on_task_tables = []
	    self.task_range = WITHIN_ON_TASK
	    map(lambda x, y: x._SetLogDict(y),
		self.accumulators, self.on_task_scores)
	    ScoreSet.Present(self, formatter)
	# Reset total task info.
	if self.total_task_scores is not None:
	    self.total_task_tables = []
	    self.task_range = WITHIN_TOTAL_TASK
	    map(lambda x, y: x._SetLogDict(y),
		self.accumulators, self.total_task_scores)
	    ScoreSet.Present(self, formatter)
	# Reset all turns info.
	if self.all_turn_scores is not None:
	    self.all_turns_tables = []
	    self.task_range = WITHIN_ALL_TURNS
	    map(lambda x, y: x._SetLogDict(y),
		self.accumulators, self.all_turn_scores)
	    ScoreSet.Present(self, formatter)

	# Now, we actually present the tables. The
	# lists should be the same length.	
	for t in self.tables:
	    if self.on_task_scores is not None:
		for p_t in self.on_task_tables:
		    if string.find(p_t[1], t) == 0:
			apply(ScoreSet.PresentTable, (self,) + p_t)
	    if self.total_task_scores is not None:
		for p_t in self.total_task_tables:
		    if string.find(p_t[1], t) == 0:
			apply(ScoreSet.PresentTable, (self,) + p_t)
	    if self.all_turn_scores is not None:
		for p_t in self.all_turns_tables:	
		    if string.find(p_t[1], t) == 0:
			apply(ScoreSet.PresentTable, (self,) + p_t)

    def PresentTable(self, col_seed, table_title, cols, formatter):
	if self.task_range == WITHIN_ALL_TURNS:
	    table_title = table_title + " (all turns)"
	    self.all_turns_tables.append((col_seed, table_title, cols,
					  formatter))
	elif self.task_range == WITHIN_TOTAL_TASK:
	    table_title = table_title + " (total task)"
	    self.total_task_tables.append((col_seed, table_title, cols,
					   formatter))
	elif self.task_range == WITHIN_ON_TASK:
	    table_title = table_title + " (on task)"
	    self.on_task_tables.append((col_seed, table_title, cols,
					formatter))

# There's an SR scoring set which will require us to
# extract all the audio inputs along with the transcriptions.
# Not sure what to do with this yet.

class NISTLogSRBaseCollector(Accumulator):
    def Accumulate(self, log_summary):
	turn_data = log_summary.FindInterestingElements([UserAudioFile, UserSR, UserTranscription], task_range = self.score_set.task_range)[1]
	user_audio_files = []
	user_srs = []
	user_transcriptions = []
	for [turn_summary, elts] in turn_data:
	    t = turn_summary.turn_anchor
	    if t.GetValue() != "user":
		continue
	    user_audio_file = None
	    user_sr = None
	    user_transcription = None
	    for eclass, elt in elts:
		if eclass is UserAudioFile:
		    user_audio_file = elt
		elif eclass is UserSR:
		    user_sr = elt
		else:
		    user_transcription = elt
	    user_audio_files.append(user_audio_file)
	    user_srs.append(user_sr)
	    user_transcriptions.append(user_transcription)
	return user_audio_files, user_srs, user_transcriptions

class NISTLogSRCollector(NISTLogSRBaseCollector):
    def Accumulate(self, log_summary):
	r = NISTLogSRBaseCollector.Accumulate(self, log_summary)
	user_audio, user_sr, user_transc = r
	# Now, we extract the data.
	user_audio_strings = []
	user_sr_strings = []
	user_transc_strings = []
	for elt in user_audio:
	    if elt is None:
		user_audio_strings.append(None)
	    else:
		user_audio_strings.append(elt.GetData())
	for elt in user_sr:
	    if elt is None:
		user_sr_strings.append(None)
	    else:
		user_sr_strings.append(elt.GetData())
	for elt in user_transc:
	    if elt is None:
		user_transc_strings.append(None)
	    else:
		user_transc_strings.append(elt.GetData())
	return user_audio_strings, user_sr_strings, user_transc_strings

# This doesn't really belong here, but I'm going to
# put it here anyway.

import popen2

class PostProcessor:
    def __init__(self, cmdline):
	if cmdline is not None:
	    self.postprocess = popen2.Popen3(cmdline, 0)
	else:
	    self.postprocess = None
    def Process(self, s):
	if self.postprocess is not None:
	    self.postprocess.tochild.write(s + "\n")
	    self.postprocess.tochild.flush()
	    return self.postprocess.fromchild.readline()[:-1]
	else:
	    return s
    def Close(self):
	if self.postprocess is not None:
	    self.postprocess.tochild.close()
	    self.postprocess.fromchild.close()

class NISTTranscriptionPostProcessor:
    def __init__(self):
	pass
    def Process(self, s):
	l = []
	toks = string.split(s)
	for tok in toks:
	    if (tok[0] == "[") and (tok[-1] == "]"):
		continue
	    else:
		l.append(tok)
	return string.join(l)
    def Close(self):
	pass

# By default, we'll use the NIST Communicator transcription
# guidelines as the postprocess. As far as I can tell, this involves
# removing tokens in square brackets. I'm not going to strip
# fragments; they're optionally deletable.

class NISTSRCollector(ScoreSet):
    def __init__(self, repository, outdir, outprefix = None):
	ScoreSet.__init__(self, repository,
			  [NISTLogSRCollector()])
	if isinstance(outdir, url_directory.BasePath):
	    self.outdir = outdir
	else:
	    self.outdir = url_directory.Path(outdir, io = repository.io)
	# Make sure it's a directory.
	if not self.outdir.Isdir():
	    self.outdir = self.outdir.Split()[0]
	if outprefix is None: outprefix = "sr"
	self.outprefix = outprefix
    def Present(self, formatter):
	# Not about to present this, since there are no presenters.
	pass
    def SaveSRFiles(self, wav_postprocess = None, hyp_postprocess = None,
		    ref_postprocess = None):	
	collector = self.FindAccumulator(NISTLogSRCollector)
	# First, we need to open the relevant files.	
	hyp_file = self.outdir.Join(self.outprefix + ".hyp").WriteStream()
	ref_file = self.outdir.Join(self.outprefix + ".ref").WriteStream()
	wav_file = self.outdir.Join(self.outprefix + ".wav").WriteStream()
	wav_postprocess = PostProcessor(wav_postprocess)
	hyp_postprocess = PostProcessor(hyp_postprocess)
	if ref_postprocess:
	    ref_postprocess = PostProcessor(ref_postprocess)
	else:
	    ref_postprocess = NISTTranscriptionPostProcessor()
	for d in self.repository.log_directories:
	    # For each directory, we get the list of triples.
	    audios, srs, transcriptions = collector.log_dict[d]
	    # Now, we need to write out each of the paths.
	    for i in range(len(audios)):
		# Now, we need a speaker tag.
		t = self._GenerateSpeakerTag(d, i)		
		if srs[i] is not None:
		    s = srs[i]
		    hyp_file.write(hyp_postprocess.Process(s) + t + "\n")
		    s = transcriptions[i] or ""
		    ref_file.write(ref_postprocess.Process(s) + t + "\n")
		    s = audios[i] or ""
		    wav_file.write(wav_postprocess.Process(s) + "\n")
	hyp_file.close()
	ref_file.close()
	wav_file.close()
	hyp_postprocess.Close()
	ref_postprocess.Close()
	wav_postprocess.Close()
    def _GenerateSpeakerTag(self, log_dir, i):
	# I'll take the last component of the log_dir path and
	# collapse the elements.
	import re, os
	return " (%s-%d)" % (re.sub("-", "", os.path.splitext(log_dir.annotated_xml.write_file.Split()[1])[0]), i)

# Here's the concept annotator. I suspect that I need to figure out how 
# to incorporate these "custom" things.

class ConceptAccumulator(Accumulator):
    def __init__(self, score_set = None):
	Accumulator.__init__(self, score_set)
	self.all_keys = []
    def Accumulate(self, log_summary):
	elts = log_summary.FindInterestingElements([UserSR,
						    ConceptAnnotation])[1]
	score_obj_list = []
	for [turn_summary, annots] in elts:
	    turn = turn_summary.turn_anchor
	    if turn.GetValue() == "user":
		# There really ought to be only one SR and one
		# concept annotation per utterance. I'll just
		# set it up so that it continues at the moment.
		sr = None
		con = None
		for eclass, elt in annots:
		    if eclass is UserSR:
			if sr is not None:
			    continue
			sr = elt
		    else:
			if con is not None:
			    continue
			con = elt
		if sr and con:
		    score_obj = UtteranceScore(sr, con)
		    for key in score_obj.keys:
			if key not in self.all_keys:
			    self.all_keys.append(key)
		    score_obj_list.append(score_obj)
	return score_obj_list
    def Compute(self, val_list, val_list_len):
	return self.Append(val_list)

# Here's the first table presentation.

class _ConceptScoreElement(ScoreElement):
    def __init__(self, score_set = None, header = None, tables = None):
	ScoreElement.__init__(self, score_set, header)
	if tables is not None:
	    self.tables = tables
	else:
	    self.tables = []
    def FindAccumulators(self):
	self.SetAccumulators(self.score_set.FindAccumulator(ConceptAccumulator))
	
    def RetrieveLogValue(self, vals, table_name):
	if table_name not in self.tables:
	    return None
	obj_list = vals[0]
	if obj_list is None:
	    return None
	return self.RetrieveObjlistValue(obj_list, table_name)
    def FormatValue(self, val):
	if type(val) is type(0):
	    return `val`
	elif type(val) is type(0.0):
	    return "%.2f" % val
	else:
	    return `val`

class ConceptSlotScoreElement(_ConceptScoreElement):
    def __init__(self, score_set = None, header = None):
	_ConceptScoreElement.__init__(self, score_set, header,
				      ["All slots",
				       "Explicit slots",
				       "Explicit slots filtered by NL",
				       "Inherited slots"])
    def RetrieveLogValue(self, vals, table_name):
	if (table_name not in self.tables) and \
	   (table_name not in self.score_set.table_slots):
	    return None
	obj_list = vals[0]
	if obj_list is None:
	    return None
	return self.RetrieveObjlistValue(obj_list, table_name)

class ConceptUttScoreElement(_ConceptScoreElement):
    def __init__(self, score_set = None, header = None):
	_ConceptScoreElement.__init__(self, score_set, header,
				      ["All utterances",
				       "Context-independent utterances",
				       "Context-dependent utterances"])    

class ConceptNumUtterances(ConceptUttScoreElement):
    def __init__(self, score_set = None):
	ConceptUttScoreElement.__init__(self, score_set, "Number of utterances")
    def RetrieveObjlistValue(self, obj_list, table_name):
	if table_name == "Context-dependent utterances":
	    obj_list = filter(lambda x: x.context_independent == 0, obj_list)
	elif table_name == "Context-independent utterances":
	    obj_list = filter(lambda x: x.context_independent == 1, obj_list)
	return len(obj_list)

class ConceptNumCorrectUtterances(ConceptUttScoreElement):
    def __init__(self, score_set = None):
	ConceptUttScoreElement.__init__(self, score_set, "Number of correct utterances")
    def RetrieveObjlistValue(self, obj_list, table_name):
	if table_name == "Context-dependent utterances":
	    obj_list = filter(lambda x: x.context_independent == 0, obj_list)
	elif table_name == "Context-independent utterances":
	    obj_list = filter(lambda x: x.context_independent == 1, obj_list)
	return len(filter(lambda x: x.correct == 1, obj_list))

class ConceptUtteranceErrorRate(ConceptUttScoreElement):
    def __init__(self, score_set = None):
	ConceptUttScoreElement.__init__(self, score_set, "Utterance error rate")
    def RetrieveObjlistValue(self, obj_list, table_name):
	if table_name == "Context-dependent utterances":
	    obj_list = filter(lambda x: x.context_independent == 0, obj_list)
	elif table_name == "Context-independent utterances":
	    obj_list = filter(lambda x: x.context_independent == 1, obj_list)
	num_corr = len(filter(lambda x: x.correct == 1, obj_list))
	num_all = len(obj_list)
	if num_all == 0:
	    return None
	return float(num_all - num_corr) / float(num_all)

class ConceptNumSlots(ConceptSlotScoreElement):
    def __init__(self, score_set = None):
	ConceptSlotScoreElement.__init__(self, score_set, "Number of slots")
    def RetrieveObjlistValue(self, obj_list, table_name):
	total = 0
	for res in obj_list:
	    total = total + res.NumSlotTotal(table_name)
	return total

class ConceptNumSlotsCorrect(ConceptSlotScoreElement):
    def __init__(self, score_set = None):
	ConceptSlotScoreElement.__init__(self, score_set, "Number of correct slots")
    def RetrieveObjlistValue(self, obj_list, table_name):
	total = 0
	for res in obj_list:
	    total = total + res.NumSlotsCorrect(table_name)
	return total

class ConceptNumInsertions(ConceptSlotScoreElement):
    def __init__(self, score_set = None):
	ConceptSlotScoreElement.__init__(self, score_set, "Number of insertions")
    def RetrieveObjlistValue(self, obj_list, table_name):
	total = 0
	for res in obj_list:
	    total = total + res.NumInsertions(table_name)
	return total

class ConceptNumDeletions(ConceptSlotScoreElement):
    def __init__(self, score_set = None):
	ConceptSlotScoreElement.__init__(self, score_set, "Number of deletions")
    def RetrieveObjlistValue(self, obj_list, table_name):
	total = 0
	for res in obj_list:
	    total = total + res.NumDeletions(table_name)
	return total

class ConceptNumSubstitutions(ConceptSlotScoreElement):
    def __init__(self, score_set = None):
	ConceptSlotScoreElement.__init__(self, score_set, "Number of substitutions")
    def RetrieveObjlistValue(self, obj_list, table_name):
	total = 0
	for res in obj_list:
	    total = total + res.NumSubstitutions(table_name)
	return total

class ConceptSlotErrorRate(ConceptSlotScoreElement):
    def __init__(self, score_set = None):
	ConceptSlotScoreElement.__init__(self, score_set, "Slot error rate")
    def RetrieveObjlistValue(self, obj_list, table_name):
	total_insertions = 0
	total_deletions = 0
	total_substitutions = 0
	total_slots = 0
	for res in obj_list:
	    total_insertions = total_insertions + res.NumInsertions(table_name)
	    total_deletions = total_deletions + res.NumDeletions(table_name)
	    total_substitutions = total_substitutions + res.NumSubstitutions(table_name)
	    total_slots = total_slots + res.NumSlotTotal(table_name)
	if total_slots == 0:
	    return None
	else:
	    return float(total_insertions + total_deletions + total_substitutions) / float(total_slots)
    
class ConceptSlotPrecision(ConceptSlotScoreElement):
    def __init__(self, score_set = None):
	ConceptSlotScoreElement.__init__(self, score_set, "Slot precision")
    def RetrieveObjlistValue(self, obj_list, table_name):
	total_insertions = 0
	total_deletions = 0
	total_substitutions = 0
	total_correct = 0
	for res in obj_list:
	    total_insertions = total_insertions + res.NumInsertions(table_name)
	    total_deletions = total_deletions + res.NumDeletions(table_name)
	    total_substitutions = total_substitutions + res.NumSubstitutions(table_name)
	    total_correct = total_correct + res.NumSlotsCorrect(table_name)
	if (total_insertions + total_substitutions + total_correct) == 0:
	    return None
	else:
	    # Precision is what did you get that's right.
	    return float(total_correct) / float(total_correct + total_insertions + total_substitutions)

class ConceptSlotRecall(ConceptSlotScoreElement):
    def __init__(self, score_set = None):
	ConceptSlotScoreElement.__init__(self, score_set, "Slot recall")
    def RetrieveObjlistValue(self, obj_list, table_name):
	total_insertions = 0
	total_deletions = 0
	total_substitutions = 0
	total_correct = 0
	for res in obj_list:
	    total_insertions = total_insertions + res.NumInsertions(table_name)
	    total_deletions = total_deletions + res.NumDeletions(table_name)
	    total_substitutions = total_substitutions + res.NumSubstitutions(table_name)
	    total_correct = total_correct + res.NumSlotsCorrect(table_name)
	if (total_deletions + total_substitutions + total_correct) == 0:
	    return None
	else:
	    # Precision is what was right that you got.
	    return float(total_correct) / float(total_correct + total_deletions + total_substitutions)

class ConceptScoreSet(ScoreSet):
    def __init__(self, repository):
	ScoreSet.__init__(self, repository,
			  [ConceptAccumulator(),
			   ConceptNumUtterances(),
			   ConceptNumCorrectUtterances(),
			   ConceptUtteranceErrorRate(),
			   ConceptNumSlots(),
			   ConceptNumSlotsCorrect(),
			   ConceptNumInsertions(),
			   ConceptNumDeletions(),
			   ConceptNumSubstitutions(),
			   ConceptSlotErrorRate(),
			   ConceptSlotPrecision(),
			   ConceptSlotRecall()
			   ],
			  tables = ["All utterances",
				    "Context-independent utterances",
				    "Context-dependent utterances",
				    "All slots",
				    "Explicit slots",
				    "Explicit slots filtered by NL",
				    "Inherited slots"
				    ])
	self.table_slots = []
    def Score(self, log_summary):
	ScoreSet.Score(self, log_summary)
	# Afterwards, we need to get all the keys and add some
	# elements to the tables.
	slot_tables = []
	acc = self.FindAccumulator(ConceptAccumulator)
	# Score is called repeatedly, for each
	# session, so I better make sure not to
	# add slots that are already added.
	for slot in acc.all_keys:
	    if slot not in self.table_slots:
		self.table_slots.append(slot)
		self.tables.append(slot)

class UtteranceScore:
    def __init__(self, s_element, concept_element):
        # Canonicalize the input S.
	if s_element:
	    self.input_s = string.join(string.split(string.lower(s_element.GetData())))
	else:
	    self.input_s = ""
        self.inherited_slots = {"correct": [],
                                "substitutions": [],
                                "deletions": []}
        self.explicit_slots = {"correct": [],
                               "substitutions": [],
                               "deletions": []}
	self.keys = []
        self.insertions = []
        self.correct = 1
        self.context_independent = 1
        self.explicit_slots_in_transcription = []
	data = concept_element.GetData()
	# First, I want to split the data by group.
	groups = []
	cur_group = {}
	for entry in string.split(data, "\n"):
	    entry = string.strip(entry)
	    if not entry:
		groups.append(cur_group)
		cur_group = {}
	    else:
		pre, post = string.splitfields(entry, ":", 1)
		cur_group[pre] = string.strip(post)
	if cur_group:
	    groups.append(cur_group)
	for cur_group in groups:
	    if cur_group.has_key("Expecting") and \
	       cur_group.has_key("Found"):
                try:
                    expected_value = cur_group["Expected value"]
                except KeyError:
                    expected_value = ""
                self.AddConceptScore(cur_group["Key"],\
				     cur_group["Expecting"], \
				     cur_group["Found"], \
				     expected_value)
    def UpdateExpectedValues(self, key, expected_value):
        # I need to canonicalize the expected value, if it
        # exists, and try to find it in the input S. If I
        # do, it's an approximation of correct SR.
        if expected_value:
            e_v = string.join(string.split(string.lower(expected_value)))
            if string.find(self.input_s, e_v) > -1:
                self.explicit_slots_in_transcription.append(key)
    def AddConceptScore(self, key, expecting, found, expected_value):
	if key not in self.keys:
	    self.keys.append(key)
        if expecting in ["Inherited value", "Explicit value"]:
            if expecting == "Explicit value":
                dict = self.explicit_slots
                if expected_value:
                    self.UpdateExpectedValues(key, expected_value)
            else:
                self.context_independent = 0
                dict = self.inherited_slots
            if found == "The right thing":
                dict["correct"].append(key)
            elif found == "The wrong thing":
                dict["substitutions"].append(key)
                self.correct = 0
            elif found == "Nothing":
                dict["deletions"].append(key)
                self.correct = 0
        elif expecting == "Nothing":
            if found == "Nothing":
                # We're going to ignore this one.
                pass
            elif found == "Something":
                self.insertions.append(key)
                self.correct = 0
    def NumSlotTotal(self, table_name):
	correct = self.NumSlotsCorrect(table_name)
        substitutions = self.NumSubstitutions(table_name)
        deletions = self.NumDeletions(table_name)
        return correct + substitutions + deletions + self.NumInsertions(table_name)
    def NumSlotsCorrect(self, table_name):
	if table_name == "All slots":
	    return len(self.inherited_slots["correct"]) + \
		   len(self.explicit_slots["correct"])
	elif table_name == "Explicit slots":
	    return len(self.explicit_slots["correct"])
	elif table_name == "Explicit slots filtered by NL":
	    return len(filter(lambda x, s = self.explicit_slots_in_transcription: x in s, self.explicit_slots["correct"]))
	elif table_name == "Inherited slots":
	    return len(self.inherited_slots["correct"])
	else:
	    # Treat it as a key.
	    tot = 0
	    if table_name in self.inherited_slots["correct"]:
		tot = tot + 1
	    if table_name in self.explicit_slots["correct"]:
		tot = tot + 1
	    return tot
    def NumInsertions(self, table_name):
	if table_name == "Inherited slots":
	    return 0
	elif table_name in ["All slots",
			    "Explicit slots",
			    "Explicit slots filtered by NL"]:
	    return len(self.insertions)
	else:
	    # Treat it as a key.
	    if table_name in self.insertions:
		return 1
	    else:
		return 0	
    def NumDeletions(self, table_name):
	if table_name == "All slots":
	    return len(self.inherited_slots["deletions"]) + \
		   len(self.explicit_slots["deletions"])
	elif table_name == "Explicit slots":
	    return len(self.explicit_slots["deletions"])
	elif table_name == "Explicit slots filtered by NL":
	    return len(filter(lambda x, s = self.explicit_slots_in_transcription: x in s, self.explicit_slots["deletions"]))
	elif table_name == "Inherited slots":
	    return len(self.inherited_slots["deletions"])
	else:
	    # Treat it as a key.
	    tot = 0
	    if table_name in self.inherited_slots["deletions"]:
		tot = tot + 1
	    if table_name in self.explicit_slots["deletions"]:
		tot = tot + 1
	    return tot
    def NumSubstitutions(self, table_name):
	if table_name == "All slots":
	    return len(self.inherited_slots["substitutions"]) + \
		   len(self.explicit_slots["substitutions"])
	elif table_name == "Explicit slots":
	    return len(self.explicit_slots["substitutions"])
	elif table_name == "Explicit slots filtered by NL":
	    return len(filter(lambda x, s = self.explicit_slots_in_transcription: x in s, self.explicit_slots["substitutions"]))
	elif table_name == "Inherited slots":
	    return len(self.inherited_slots["substitutions"])
	else:
	    # Treat it as a key.
	    tot = 0
	    if table_name in self.inherited_slots["substitutions"]:
		tot = tot + 1
	    if table_name in self.explicit_slots["substitutions"]:
		tot = tot + 1
	    return tot
