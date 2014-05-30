# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This file supports summaries and session.cgi.

# These are the interesting format classes.

from GC_log_formatting import StartTask, EndTask, NewTurn, \
     TaskCompletion, SystemText, UserSR, UserTranscription, \
     SystemAudioStart, SystemAudioEnd, UserAudioStart, UserAudioEnd, \
     SystemAudioFile, UserAudioFile, \
     InterestingData, UtteranceFormatClass, DefaultFormatter, \
     InterestingEvent, SRHypothesis, InputParaphrase, ConceptInput, \
     ConceptAnnotation, GetLocation

from GC_human_annotations import CreateInterestingTag

import io_device, os, cgi, string, re

# This is the raw summarization, which you get when you
# do xml_summarize.

RawSummarizationOrder = [UserAudioStart,
			 UserAudioFile,
			 UserAudioEnd,
#			 SRHypothesis,
			 UserSR,
			 UserTranscription,
			 SystemAudioStart,
			 SystemAudioFile,
			 SystemAudioEnd,
			 SystemText]

def RawSummarization(log_summary, task_range):
    formatter = log_summary.log_dir.formatter
    sep = formatter.FormatSeparator()
    sub_s = []
    s = []
    for elt in log_summary.global_list:
	sub_s.append(elt.original_dp.Format(elt))
    sub_s.append(sep)
    s.append(formatter.FormatParagraph(string.joinfields(sub_s, "\n")))
    subsets = log_summary.FindInterestingElements(RawSummarizationOrder, task_range = task_range)[1]
    for [turn_summary, elts] in subsets:
	if sep: s.append(sep)
	turn = turn_summary.turn_anchor
	s.append("Turn %d (%s)" % (turn_summary.true_turn_index,
				   turn.GetValue()))
	s.append(turn.original_dp.Format(turn))
	if sep: s.append(sep)
	sub_s = []
	for eclass, subelt in elts:
	    sub_s.append(subelt.original_dp.Format(subelt))
	s.append(formatter.FormatParagraph(string.joinfields(sub_s, "\n")))
    log_summary.log_dir.io.ReportOutput(string.joinfields(s, ""))

# And here's the tabular visualization of the same thing. We partition
# the data into significant and insignificant, input and output.

PrimaryFormattedInput = [UserAudioFile, UserSR, UserTranscription]
#SecondaryFormattedInput = [SRHypothesis, ChosenSRHypothesis,
#			   InputParaphrase, ConceptInput]
#SecondaryFormattedInput = [UserAudioStart, UserAudioEnd, SRHypothesis,
#			   InputParaphrase, ConceptInput]
SecondaryFormattedInput = [UserAudioStart, UserAudioEnd,
			   InputParaphrase, ConceptInput,
			   ConceptAnnotation]
PrimaryFormattedOutput = [SystemAudioFile, SystemText]
SecondaryFormattedOutput = [SystemAudioStart, SystemAudioEnd]

# This visualization really only works in HTML, because the
# default formatter is too stupid to do tables correctly.

def FormattedSummarization(log_summary, task_range):    
    top_left_list = log_summary.FindInterestingElements(PrimaryFormattedInput, task_range = task_range)[1]
    top_right_list = log_summary.FindInterestingElements(SecondaryFormattedInput, task_range = task_range)[1]
    bottom_left_list = log_summary.FindInterestingElements(PrimaryFormattedOutput, task_range = task_range)[1]
    bottom_right_list = log_summary.FindInterestingElements(SecondaryFormattedOutput, task_range = task_range)[1]
    for turn_id in range(len(top_left_list)):
	# Each table will have four cells. The first row is
	# input primary, input secondary, the second row
	# is output primary, empty.
	top_right_subset = top_right_list[turn_id][1]
	# In a user turn, add a concept annotation node if there is none,
	# and put it somewhere useful.
	if top_right_list[turn_id][0].turn_anchor.GetValue() == "user":
	    for eclass, elt in top_right_subset:
		if eclass is ConceptAnnotation:
		    break
	    else:
		# Don't forget to give this thing a parent,
		# so that session.cgi can anchor the
		# annotation somewhere. Or, better yet,
		# give it an utterance ID.
		# When I put it in the "right place", I need to
		# get the appropriate turn ID not from the length
		# of the list I'm working through, but rather
		# the actual location of the interesting tag.
		true_turn_id, tidx = top_left_list[turn_id][0].turn_anchor.GetLocation()
		dp = CreateInterestingTag(log_summary.log_dir,
					  {"type_annotation":
					   "concept",
					   "turnid":
					   true_turn_id,
					   "dtype":
					   "string"},
					  "GC_DATA",
					  "")
		log_summary.PlaceInTurn(None, dp, top_right_list[turn_id][0].turn_anchor)
		top_right_subset.append((ConceptAnnotation, dp))
	top_left = map(lambda x: x[1].original_dp.Format(x[1]),
		       top_left_list[turn_id][1])
	top_right = map(lambda x: x[1].original_dp.Format(x[1]),
			top_right_subset)
	bottom_left = map(lambda x: x[1].original_dp.Format(x[1]),
			  bottom_left_list[turn_id][1])
	bottom_right = map(lambda x: x[1].original_dp.Format(x[1]),
			   bottom_right_list[turn_id][1])
	table = []
	if top_left or top_right:
	    table.append([string.joinfields(top_left, "\n"),
			  string.joinfields(top_right, "\n")])
	if bottom_left or bottom_right:
	    table.append([string.joinfields(bottom_left, "\n"),
			  string.joinfields(bottom_right, "\n")])
	if table:
	    log_summary.io.PrintTitle("Turn %d (%s)" % (turn_id, top_left_list[turn_id][0].turn_anchor.GetValue()), log_summary.log_dir.formatter)
	    log_summary.io.PrintTable(None, table, log_summary.log_dir.formatter)
	
# And now, we try to write the HTTP formatter.

# I've got to get this right, finally. ALL the stuff that's
# specific to HTML should be here. It shouldn't be necessary
# to have a specific HTTP IO device for the various types
# of output, except to handle the specifics of dealing with
# HTTP (for instance, the fact that CGI output, even though
# it's stdin/stdout, doesn't permit prompts). So I need to
# figure out how to format anchors, etc., so that the behavior
# is transparent across output devices.

class HTMLFormatter(DefaultFormatter):
    def __init__(self, logfile, data_prefix = ""):
	DefaultFormatter.__init__(self, logfile)
	self.data_prefix = data_prefix
	self.AddInterface(InterestingData, "_FormatAnchor",
			  self._FormatAnchorData)
	self.AddInterface(InterestingData, "_Format",
			  self._FormatInteresting)
	self.AddInterface(InterestingEvent, "_Format",
			  self._FormatInterestingEvent)
	self.AddInterface(SRHypothesis, "_FormatData",
			  self._FormatSRHypothesisData)
	self.AddInterface(UserAudioFile, "_Format",
			  self._FormatInterestingAnchor)	
	self.AddInterface(SystemAudioFile, "_Format",
			  self._FormatInterestingAnchor)
	self.AddInterface(InterestingData, "_FormatKey",
			  self._FormatInterestingDataKey)
	self.AddInterface(ConceptInput, "_Format",
			  self._FormatConceptInput)
	self.AddInterface(ConceptAnnotation, "_Format",
			  self._FormatConceptAnnotation)
	self.AddInterface(UtteranceFormatClass, "_FormatHeader",
			  self._FormatUtteranceHeader)
	
	# For table stuff.
	self.table_attrs = "border = 2 cols = 2 width = '100%'"
	self.row_attrs = "valign = top"
	self.cell_attrs = ""
	
    def _FormatAnchorData(self, dp, allow_interesting = None, indent = 0):
	d = dp._FormatData()
	v = os.path.split(d)[1]
	if self.logfile.other_paths.has_key(v):
	    return "<A href = %s/%s>%s</A>" % \
		   (self.data_prefix, v, v)
	return d    
    
    def _FormatInteresting(self, dp, allow_interesting = None, indent = 0):
	return dp._FormatKey(allow_interesting, indent) + \
	       cgi.escape(dp._FormatData(allow_interesting, indent)) + "<br>"

    def _FormatInterestingAnchor(self, dp, allow_interesting = None, indent = 0):
	return dp._FormatKey(allow_interesting, indent) + \
	       dp._FormatData(allow_interesting, indent) + "<br>"

    def _FormatInterestingEvent(self, dp, allow_interesting = None,
				indent = 0):
	return cgi.escape(allow_interesting._Format()) + "<br>"

    def _FormatSRHypothesisData(self, dp, allow_interesting = None, indent = 0):
	return CleanSRHypothesis(dp._FormatData())

    def FormatString(self, str):
	# This will be the formatting for raw strings.
	return "<PRE>" + cgi.escape(str) + "</PRE>"
    # And these are the table formatting stuff.
    def FormatSeparator(self):
	return "<p>"
    def FormatTitle(self, title):
        return "<h3>" + title + "</h3>"
    def FormatParagraph(self, text):
        return "<p>" + text
    def FormatTable(self, headers, rows):
	strs = []
	if headers:
	    header_str = map(lambda x: "<th>%s</th>" % (string.strip(x) or "&nbsp;"), headers)	
        strs.append("<table %s>\n" % self.table_attrs)
	if headers:
	    strs.append(string.join([("<tr %s>" % self.row_attrs), 
				     string.join(header_str), "</tr>\n"]))
	    numcols = len(headers)
	else:
	    numcols = len(rows[0])
	for row in rows:
	    if len(row) > numcols:
		row = row[:numcols]
	    elif len(row) < numcols:
		row = row + ((numcols - len(row)) * ["&nbsp"])
	    row_str = map(lambda x, a = self.cell_attrs: "<td %s>%s</td>" % (a, (string.strip(x) or "&nbsp;")), row)
	    strs.append(string.join([("<tr %s>" % self.row_attrs), string.join(row_str), "</tr>\n"]))
        strs.append("</table>\n")
	return string.joinfields(strs, "")

    def _FormatConceptInput(self, dp, allow_interesting = None, indent = 0):
	d = dp._FormatData()
	if d is None:
	    return dp._FormatKey(allow_interesting, indent) + \
		   "(no concept input found)<BR>"
        else:
	    return dp._FormatKey(allow_interesting, indent) + \
		   "<br><pre>" + d + "</pre><BR>"

    def _FormatConceptAnnotation(self, dp, allow_interesting = None, indent = 0):
	d = dp._FormatData()
	if d is None:
	    return dp._FormatKey(allow_interesting, indent) + \
		   "(no concept annotation found)<BR>"
        else:
	    return dp._FormatKey(allow_interesting, indent) + \
		   "<br><pre>" + d + "</pre><BR>"

    def _FormatInterestingDataKey(self, dp, allow_interesting = None, indent = 0):
	try:
	    key_value = dp.properties["key"]
	except:
	    # key should always be present, but we fucked
	    # up in writing the human annotation spec.
	    key_value = "(unknown key)"
	if allow_interesting:
	    key_value = allow_interesting._header_internal_()
	return "<b>%s:</b> " % key_value

    def _FormatUtteranceHeader(self, dp, allow_interesting = None, indent = 0):
	return "<H3>Utterance %s</H3>" % GetLocation(dp)[0]

class HTTPIODevice(io_device.DefaultIODevice):
    def __init__(self, file = None, fp = None):
	io_device.DefaultIODevice.__init__(self, file, fp)
        os.environ["SLS_VERBOSE"] = "0"
	os.environ["GAL_VERBOSE"] = "0"
    def ReportStatus(self, str, add_newline = 1):
        pass
    def ReportError(self, str, add_newline = 1):
	self.fp.write("Error: " + str + "<br>\n")
	self.fp.flush()
    def Prompt(self, msg):
        raise io_device.PromptFailed

def CleanSRHypothesis(line):
    line = string.strip(line)
    s = re.sub("<[^>]*>", "", line)
    s = re.sub("_"," ", s)
    toks = string.split(s)
    return string.join(toks)    

# And here's the actual visualization code.

import xml_interleave

VisualizationTable = {"raw": RawSummarization,
		      "tabular": FormattedSummarization}

def xml_visualize(log_sum, vtype = "raw", task_range = xml_interleave.WITHIN_TOTAL_TASK):
    log_dir = log_sum.log_dir
    if not VisualizationTable.has_key(vtype):
	log_dir.io.ReportStatus("Unknown visualization type, using raw.")
	vtype = "raw"
    VisualizationTable[vtype](log_sum, task_range)
