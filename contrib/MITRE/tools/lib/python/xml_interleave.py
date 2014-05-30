# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import xml_tree, xml_transducer, string, sys, re
import GC_log_formatting

# This part is used by validation, scoring and visualization.

# Utility to grab turnid and scope.

def GetTurnInfo(turn_elt):
    # I can't just look at the first one,
    # because it's a turn and its scope is global.
    if isinstance(turn_elt, GC_log_formatting.NewTurn):
	scope = turn_elt.GetValue()
    else:
	scope = turn_elt.scope
    try:
	return turn_elt.GetLocation()[0], scope
    except KeyError:
	return None, scope

# SAM 1/8/00: I think I'd better build in a little better support
# for the segmented summaries. I'll add a class which serves
# as a better organizing principle for this stuff. From studying the
# ad hoc examples, I need to do a couple things. The interesting
# format classes support access for data, value, location and times.
# The data access will return int, float, string, None, or tag object.

# The DMA specifies that consecutive turns from the
# same "side" should be collapsed.

EXACT_TURNS = 0
COLLAPSED_TURNS = 1
NO_TURNS = 2

# Task ranges

WITHIN_ALL_TURNS = 0
WITHIN_TOTAL_TASK = 1
WITHIN_ON_TASK = 2

class TurnSummary:
    def __init__(self, turn_anchor, best_placement, true_turn_index):
	self.turn_anchor = turn_anchor
	self.best_placement = best_placement
	self.before_overall_start = []
	self.between_starts = []
	self.within_all = []
	self.between_ends = []
	self.after_overall_end = []
	self.true_turn_index = true_turn_index
    def AddElement(self, elt, stage):
	if stage == 0:
	    self.before_overall_start.append(elt)
	elif stage == 1:
	    self.between_starts.append(elt)
	elif stage == 2:
	    self.within_all.append(elt)
	elif stage == 3:
	    self.between_ends.append(elt)
	elif stage == 4:
	    self.after_overall_end.append(elt)
    def AddToBestPlacement(self, elt):
	self.AddElement(elt, self.best_placement)
    def Print(self):
	print self.turn_anchor, self.before_overall_start, \
	      self.between_starts, self.within_all, \
	      self.between_ends, self.after_overall_end

class LogSummary:
    def __init__(self, xml_doc, human_doc, log_dir):
	self.log_dir = log_dir
	self.io = log_dir.io
	self.xml_doc = xml_doc
	self.human_doc = human_doc
	self.Segment()
	
    # We're going to take the human_doc apart.

    def Segment(self):
	self.global_list = []
	self.turn_list = []
	self.pre_turn_list = []
	self.task_end_task_element = None
	self.task_start_task_element = None
	self.overall_end_task_element = None
	self.overall_task_start_element = None
	
	if hasattr(self.xml_doc, "path"):
	    self.io.ReportStatus("Resegmenting: " + self.xml_doc.path.PathString())
	else:
	    self.io.ReportStatus("Resegmenting... ")
	interesting_elements = self.xml_doc.Extract()
	if self.human_doc is not None:
	    if hasattr(self.human_doc, "path"):
		self.io.ReportStatus("Incorporating human annotations: " + self.human_doc.path.PathString())
	    else:
		self.io.ReportStatus("Incorporating human annotations...")
	    human_interesting_elements = self.human_doc.Extract()
	else:
	    human_interesting_elements = []

	# SAM 8/28/00: I've made a perhaps too hasty
	# assumption about how the scores should be generated.
	# I've assumed that interesting elements should be
	# measured only between total task markers, so that
	# we can't get specific on-task metrics, and
	# we can't get total dialogue WER, for instance.
	# So I need to rewrite the segmenter to address this
	# problem, and probably all the calls to FindInterestingElements.
	# I'm going to restructure the collection so that
	# each element of the turn_list is segmented into
	# elements before overall start, elements between
	# overall and task start, elements within
	# both tasks, elements after task end, and
	# elements after overall end. For the human placement,
	# I also need to collect the "best" location for the
	# turn, in case there is nothing in the turn except
	# the turn anchor and a task marker is encountered
	# before the next new turn, and one of the annotations
	# matches the turn anchor.
	
	# Remember, sometimes multiple interesting
	# elements live on the same tag. So, for
	# instance, if there is already an
	# end task element, but it lives on the
	# same tag as another interesting element,
	# the second shouldn't be considered to be
	# in the post-task.

	turn_list_index = 0
	for elt in interesting_elements:
	    if isinstance(elt, GC_log_formatting.NewTurn):
		turn_index = len(self.turn_list)
		self.turn_list.append(TurnSummary(elt, turn_list_index, turn_index))
	    # First, we collect all the global elements.
	    elif elt.scope == "global":
		self.global_list.append(elt)
		if isinstance(elt, GC_log_formatting.EndTask):
		    v = elt.GetValue()
		    if v in ["task", "true"]:
			self.task_end_task_element = elt
			turn_list_index = 3
		    if v in ["total", "true"]:
			self.overall_end_task_element = elt
			turn_list_index = 4
		elif isinstance(elt, GC_log_formatting.StartTask):
		    v = elt.GetValue()
		    if v in ["total", "true"]:
			self.overall_start_task_element = elt
			turn_list_index = 1
		    if v in ["task", "true"]:
			self.task_start_task_element = elt
			turn_list_index = 2
		    # If there's already a turn list, update
		    # the turn_placement_table to reflect the
		    # "better" placement.
		    if self.turn_list:
			self.turn_list[-1].best_placement = turn_list_index
	    # If we find a turn list, add a
	    # list with places for all the possible positions.
	    # Before we find any turns, add things to the
	    # pre_turn_list. After we find a turn, use the
	    # turn_list_index to place the element,
	    # UNLESS WE'VE FOUND ONE OF THE END TASK ELEMENTS
	    # AND elt.original_dp IS IDENTICAL TO IT.
	    elif len(self.turn_list) == 0:
		self.pre_turn_list.append(elt)
	    elif (turn_list_index == 3) and \
		 (self.task_end_task_element.original_dp is elt.original_dp):
		self.turn_list[-1].AddElement(elt, turn_list_index - 1)
	    elif (turn_list_index == 4) and \
		 (self.overall_end_task_element.original_dp is elt.original_dp):
		self.turn_list[-1].AddElement(elt, turn_list_index - 1)
	    else:
		self.turn_list[-1].AddElement(elt, turn_list_index)
	# Now, when I collect the human interesting elements,
	# I need to try to place the non-global elements in
	# the most plausible place. So what I'll do is start
	# from the inside out. If it matches an element
	# inside both task boundaries, place it there. If not,
	# if it matches an element within one of them, place
	# it there, otherwise, look outside the tasks. If it
	# matches the turn anchor and no other tag, use the
	# turn_placement_table to figure out where to put it.
	for elt in human_interesting_elements:
	    scope = elt.scope
	    if scope == "global":
		self.global_list.append(elt)
	    else:
		props = elt.original_dp.properties
		if props.has_key("turnid"):
		    test_fn = self.human_turnid_matches
		elif props.has_key("tidx"):
		    test_fn = self.human_tidx_matches
		else:
		    self.io.ReportStatus("Warning: couldn't place human annotation (no tidx or turnid): " + elt.original_dp.render())
		    continue
		found_loc = 0
		for turn in self.turn_list:
		    # Check index 2, then 1 and 3, then 0 and 4, then
		    # the turn element itself.
		    for turn_elt in turn.within_all:
			if test_fn(scope, props, turn_elt):
			    turn.within_all.append(elt)
			    found_loc = 1
			    break
		    if found_loc == 1: break
		    for turn_elt in turn.between_starts:
			if test_fn(scope, props, turn_elt):
			    turn.between_starts.append(elt)
			    found_loc = 1
			    break
		    if found_loc == 1: break
		    for turn_elt in turn.between_ends:
			if test_fn(scope, props, turn_elt):
			    turn.between_ends.append(elt)
			    found_loc = 1
			    break
		    if found_loc == 1: break
		    for turn_elt in turn.before_overall_start:
			if test_fn(scope, props, turn_elt):
			    turn.before_overall_start.append(elt)
			    found_loc = 1
			    break
		    if found_loc == 1: break
		    for turn_elt in turn.after_overall_end:
			if test_fn(scope, props, turn_elt):
			    turn.after_overall_end.append(elt)
			    found_loc = 1
			    break
		    if found_loc == 1: break
		    if test_fn(scope, props, turn.turn_anchor):
			turn.AddToBestPlacement(elt)
			found_loc = 1
		    if found_loc == 1:
			break
		if found_loc == 0:
		    self.io.ReportStatus("Warning: couldn't place human annotation: " + elt.original_dp.render())
	self.io.ReportStatus("...resegmented.")

    def human_turnid_matches(self, elt_scope, elt_props, candidate):
	turn_elt_turnid, turn_elt_scope = GetTurnInfo(candidate)
	return (turn_elt_turnid is not None) and \
	       (turn_elt_turnid == elt_props["turnid"]) and \
	       (elt_scope == turn_elt_scope)

    def human_tidx_matches(self, elt_scope, elt_props, candidate):
	turn_props = candidate.original_dp.properties
	return turn_props.has_key("tidx") and \
	       (turn_props["tidx"] == elt_props["tidx"]) and \
	       (elt_scope == turn_elt.scope)
    
    # Utility for search lists.

    def FindInterestingElements(self, eclass_list,
				turn_segmentation = EXACT_TURNS,
				task_range = WITHIN_TOTAL_TASK,
				restrictions = {}):
	global_subset = []
	turn_subset = []
	if not eclass_list:
	    return [], []
	turn_eclasses = []	
	turn_sublist = []
	# Go by the turn's best location.
	for elt in self.turn_list:
	    if (elt.best_placement == 2) or \
	       ((elt.best_placement in [1, 3]) and \
		task_range in [WITHIN_TOTAL_TASK, WITHIN_ALL_TURNS]) or \
		((elt.best_placement in [0, 4]) and \
		 task_range == WITHIN_ALL_TURNS):
		turn_sublist.append(elt)
	for eclass in eclass_list:
	    if eclass.scope == "global":
		try: local_restrs = restrictions[eclass]
		except: local_restrs = {}
		for elt in self.global_list:
		    if self._ItemMatches(elt, eclass, local_restrs):
			global_subset.append((eclass, elt))
		# Don't forget the turns themselves! And make
		# sure you respect the task range.
		for elt in turn_sublist:
		    if self._ItemMatches(elt.turn_anchor, eclass, local_restrs):
			global_subset.append((eclass, elt.turn_anchor))
	    else:
		turn_eclasses.append(eclass)
	# Depending on what task range is chosen, we'll
	# look at different portions of the turn_list elements.
	for turn in turn_sublist:
	    res = []
	    class_restr_pairs = []
	    for eclass in turn_eclasses:
		try: local_restrs = restrictions[eclass]
		except: local_restrs = {}
		class_restr_pairs.append((eclass, local_restrs))
	    # We want to make sure that the elements appear in
	    # order, so we nee to loop through the elements
	    # and then the pairs.
	    if task_range == WITHIN_ALL_TURNS:
		for elt in turn.before_overall_start:
		    for eclass, local_restrs in class_restr_pairs:
			if self._ItemMatches(elt, eclass, local_restrs):
			    res.append((eclass, elt))
			    break
	    if task_range in [WITHIN_ALL_TURNS, WITHIN_TOTAL_TASK]:
		for elt in turn.between_starts:
		    for eclass, local_restrs in class_restr_pairs:
			if self._ItemMatches(elt, eclass, local_restrs):
			    res.append((eclass, elt))
			    break
	    for elt in turn.within_all:
		for eclass, local_restrs in class_restr_pairs:
		    if self._ItemMatches(elt, eclass, local_restrs):
			res.append((eclass, elt))
			break
	    if task_range in [WITHIN_ALL_TURNS, WITHIN_TOTAL_TASK]:
		for elt in turn.between_ends:
		    for eclass, local_restrs in class_restr_pairs:
			if self._ItemMatches(elt, eclass, local_restrs):
			    res.append((eclass, elt))
			    break
	    if task_range == WITHIN_ALL_TURNS:
		for elt in turn.after_overall_end:
		    for eclass, local_restrs in class_restr_pairs:
			if self._ItemMatches(elt, eclass, local_restrs):
			    res.append((eclass, elt))
			    break
	    turn_subset.append([turn, res])
	if turn_segmentation == COLLAPSED_TURNS:
	    turn_subset = self.CollapseTurns(turn_subset)
	elif turn_segmentation == NO_TURNS:
	    all_turns = []
	    for turn in turn_subset:
		all_turns = all_turns + turn[1]
	    turn_subset = all_turns
	return global_subset, turn_subset

    # SAM 4/13/00: Added wildcard matching to comparison.

    def __item_value_matches(self, re_obj, val):
	m = re_obj.match(val)
	if (m is None) or (m.end() != len(val)):
	    return 0
	else:
	    return 1
    
    def _ItemMatches(self, elt, eclass, local_restrs):
	if not isinstance(elt, eclass):
	    return 0
	if not local_restrs:
	    return 1
	props = elt.original_dp.properties
	loc = None	
	for key, value in local_restrs.items():
	    if not isinstance(value, re.RegexObject):
		value = re.compile(value)
	    if key in ["tidx", "turnid"]:
		if loc is None: loc = elt.GetLocation()
		turnid, tidx = loc
		if ((key == "tidx") and \
		    not self.__item_value_matches(value, tidx)) or \
		   ((key == "turnid") and \
		    not self.__item_value_matches(value, turnid)):
		    return 0
	    elif not props.has_key(key):
		return 0
	    elif not self.__item_value_matches(value, props[key]):
		return 0
	return 1
    
    def CollapseTurns(self, turn_subset):
	if not turn_subset:
	    return []
	else:
	    current_side = turn_subset[0][0].turn_anchor.GetValue()
	    all_turns = []
	    cur_turn = turn_subset[0]
	    for turn in turn_subset[1:]:
		if turn[0].turn_anchor.GetValue() != current_side:
		    all_turns.append(cur_turn)
		    current_side = turn[0].turn_anchor.GetValue()
		    cur_turn = turn
		else:
		    cur_turn[1] = cur_turn[1] + turn[1]
	    # Don't forget the final one!
	    all_turns.append(cur_turn)
	    return all_turns

    # Utility for placing new annotations.
    def PlaceInTurn(self, old_dp, new_dp, turn_elt = None):
	for t in self.turn_list:
	    if turn_elt is not None and \
	       turn_elt == turn.turn_anchor:
		t.AddToBestPlacement(new_dp)
		break
	    elif old_dp in t.within_all:
		t.within_all.append(new_dp)
		break
	    elif old_dp in t.between_starts:
		t.between_starts.append(new_dp)
		break
	    elif old_dp in t.between_ends:
		t.between_ends.append(new_dp)
		break
	    elif old_dp in t.before_overall_start:
		t.before_overall_start.append(new_dp)
		break
	    elif old_dp in t.after_overall_end:
		t.after_overall_end.append(new_dp)
		break

    def RemoveFromTurn(self, old_dp):
	for t in self.turn_list:
	    for old_dp_subset in t[1:]:
		if old_dp in t.within_all:
		    t.within_all.remove(old_dp)
		    return
		elif old_dp in t.between_starts:
		    t.between_starts.remove(old_dp)
		    return
		elif old_dp in t.between_ends:
		    t.between_ends.remove(old_dp)
		    return
		elif old_dp in t.before_overall_start:
		    t.before_overall_start.remove(old_dp)
		    return
		elif old_dp in t.after_overall_end:
		    t.after_overall_end.remove(old_dp)
		    return

# I'm trying to produce a single file here which contains all
# the contents of the original file plus the annotations. My strategy
# for this is to work from the summary, adding the annotations at
# the end of their appropriate turns. I have to be careful to make sure
# that the human elements get placed in the right location, which
# means respecting the type_new_turn landmarks more than anything.
# By relying on the log summary, we already get the segmentation.
# And we only want to locate the elements which are interesting
# which are in the human annotation.

def _IsParent(child, parent):
    if child is parent:
	return 1
    elif hasattr(child, "parent") and child.parent:
	return _IsParent(child.parent, parent)
    else:
	return 0

# Does a left-to-right depth-first walk, which will find the first
# turn.

def _FindFirstTurn(xml_obj):
    if isinstance(xml_obj, xml_tree.Tag) and \
       (xml_obj.name == "GC_TURN"):
	return xml_obj
    elif hasattr(xml_obj, "children"):
	for c in xml_obj.children:
	    f = _FindFirstTurn(c)
	    if f: return f
	return None
    else:
	return None

def xml_unify_files(log_sum):
    xml_doc = log_sum.xml_doc
    human_globals = []
    for elt in log_sum.global_list:
	if not _IsParent(elt.original_dp, xml_doc):
	    human_globals.append(elt)
    human_pre_turn = []
    for elt in log_sum.pre_turn_list:
	if not _IsParent(elt.original_dp, xml_doc):
	    human_pre_turn.append(elt)
    human_turn = []
    # We want to collect the original DP, because
    # we'll place the annotation as immediately after
    # that as we can.
    for elt in log_sum.turn_list:
	for element in elt.within_all:
	    if not _IsParent(element.original_dp, xml_doc):
		human_turn.append((elt.turn_anchor, element))
	for element in elt.between_starts:
	    if not _IsParent(element.original_dp, xml_doc):
		human_turn.append((elt.turn_anchor, element))
	for element in elt.between_ends:
	    if not _IsParent(element.original_dp, xml_doc):
		human_turn.append((elt.turn_anchor, element))
	for element in elt.before_overall_start:
	    if not _IsParent(element.original_dp, xml_doc):
		human_turn.append((elt.turn_anchor, element))
	for element in elt.after_overall_end:
	    if not _IsParent(element.original_dp, xml_doc):
		human_turn.append((elt.turn_anchor, element))
    # Now I have all the human elements I need to locate.
    # These elements need to live somewhere useful.
    # The problem is where. We need to respect the
    # structure of the XML file, so that GC_ANNOT can
    # occur as a child of GC_SESSION, but GC_DATA can't;
    # new elements which are GC_EVENT, etc., must be inside
    # a GC_TURN, etc. The rules are very complex.

    # Global elements aren't so much of an issue. They
    # can live anywhere and they'll be yanked out. So for
    # globals, all we need to do is determine what type
    # of data they are: an interesting event, interesting
    # data or a GC_ANNOT. Data I will wrap inside a GC_ANNOT;
    # all elements will be deposited inside the first GC_TURN.

    first_turn = _FindFirstTurn(xml_doc)

    if not first_turn:
	# Just give up.
	log_sum.io.ReportStatus("Warning: no turns in annotated file; aborting.")
	return

    for elt in human_globals:
	first_turn.AddChildAtLocation(_EnsureAnnot(elt.original_dp, log_sum.log_dir), index = 0)
	
    # NewTurn and StartTask and EndTask can only live
    # on events. So we can use this fact to place the
    # remaining data.

    # All the pre-turn stuff will go immediately before
    # the first turn. For each element in the turn list,
    # it will go after the first turn element. The
    # post-turn elements will need to be inserted after the
    # appropriate EndTask. Like the earlier ones, I'll wrap
    # GC_DATA in GC_ANNOT.

    for elt in human_pre_turn:
	t = _EnsureAnnot(elt.original_dp. log_sum.log_dir)
	if log_sum.turn_list:
	    anchor = log_sum.turn_list[0].turn_anchor.original_dp
	    anchor.parent.AddChildAtLocation(t, ref_child = anchor, after_ref = 0)
	else:
	    # Use the first_turn. It doesn't matter where.
	    first_turn.AddChildAtLocation(t, index = 0)

    for turn_anchor, element in human_turn:
	# Insert it immediately after the turn element.
	t = _EnsureAnnot(element.original_dp, log_sum.log_dir)
	anchor = turn_anchor.original_dp
	anchor.parent.AddChildAtLocation(t, ref_child = anchor)

    xml_doc.SetDocumentProperty("human_annotations_included", "1")
    
def _EnsureAnnot(elt, log_dir):
    if elt.name == "GC_DATA":
	# Wrap it in a GC_ANNOT.
	t = GC_log_formatting.FormattableTag("GC_ANNOT", {})
	t.AddChild(elt)
	t.AddLogAndFormatClasses(log_dir)
	return t
    else:
	return elt
