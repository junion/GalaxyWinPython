# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import string
import GC_log_formatting, xml_interleave

# In this function, I want to have described the data types.

def format_kv_pairs(pairs):
    return string.joinfields(map(lambda x: ("%s=\"%s\"" % x), pairs), ", ")

SampleFormatInstances = map(lambda x: x(None, x.key_value_pairs[0][0], x.key_value_pairs[0][1]), GC_log_formatting.InterestingFormatClasses)

def validate_min_max(legal_scopes, elt_dict):
    found_error = 0
    s_output = []
    for c_instance in SampleFormatInstances:
	if c_instance.scope in legal_scopes:
	    try:
		contents = elt_dict[c_instance.__class__]
	    except KeyError:
		contents = []
	    res, reason = c_instance.EnsureMinMax(contents, elt_dict)
	    if res == 0:
		found_error = 1
		s_output.append("Error for one of the attributes %s:" % format_kv_pairs(c_instance.key_value_pairs))
		s_output.append(reason)
    return found_error, string.joinfields(s_output, "\n")

def format_location_info(elt):
    turnid, tidx = GC_log_formatting.GetLocation(elt.original_dp)
    if turnid and tidx:
	return "turnid %s, tidx %s" % (turnid, tidx)
    elif turnid:
	return "turnid %s" % turnid
    elif tidx:
	return "tidx %s" % tidx
    else:
	return "no location info"

def xml_validate(log_sum):
    log_dir = log_sum.log_dir
    global_dict = {}
    for elt in log_sum.global_list:
	if global_dict.has_key(elt.__class__):
	    global_dict[elt.__class__].append(elt)
	else:
	    global_dict[elt.__class__] = [elt]
    final_turn_list = []
    # We're just going to collect the ones between the task boundaries.
    for turn in log_sum.turn_list:
	turn_dict = {}
	for elt in turn.between_starts + turn.within_all + turn.between_ends:
	    if turn_dict.has_key(elt.__class__):
		turn_dict[elt.__class__].append(elt)
	    else:
		turn_dict[elt.__class__] = [elt]
	final_turn_list.append((turn.turn_anchor, turn_dict))
	# Make sure the turn elements are in the global list.
	if global_dict.has_key(turn.turn_anchor.__class__):
	    global_dict[turn.turn_anchor.__class__].append(turn.turn_anchor)
	else:
	    global_dict[turn.turn_anchor.__class__] = [turn.turn_anchor]
    # All the global elements will be in the right scope.
    # The turn elements may not be within the correct
    # type of turn. There may be too many or too few
    # of a given element.
    s_output = ["\n#\n# Log %s:\n#\n" % log_dir.path.PathString(),
		"Global elements:\n"]
    found_error, s_sample = validate_min_max(["global"], global_dict)
    s_output.append(s_sample)
    if found_error == 0:
	s_output.append("No errors found.")
    # Now, report on the elements which appeared before a turn
    # started.
    found_misplaced = 0
    if log_sum.pre_turn_list:
	found_misplaced = 1	
	s_output.append("\nPossibly misplaced elements:\n")
	for elt in log_sum.pre_turn_list:
	    s_output.append("Element with attribute %s appears before any turn begins." % format_kv_pairs([elt.true_kv]))
    for turn in log_sum.turn_list:
	if turn.before_overall_start or turn.after_overall_end:
	    if not found_misplaced:
		found_misplaced = 1
		s_output.append("\nPossibly misplaced elements:\n")
	    for elt in turn.before_overall_start:
		s_output.append("Element in turn %d (%s) with attribute %s appears before overall task start." % (turn.true_turn_index, format_location_info(elt), format_kv_pairs([elt.true_kv])))
	    for elt in turn.after_overall_end:
		s_output.append("Element in turn %d (%s) with attribute %s appears after overall task end." % (turn.true_turn_index, format_location_info(elt), format_kv_pairs([elt.true_kv])))
    found_misplaced_turn = 0
    for turn in log_sum.turn_list:
	if (not turn.between_starts) and (not turn.between_ends) and \
	   (not turn.within_all):
	    if not found_misplaced_turn:
		found_misplaced_turn = 1
		s_output.append("\nPossibly misplaced turns:\n")
	    s_output.append("Turn %d (%s) is completely outside the overall task boundaries." % (turn.true_turn_index, format_location_info(turn.turn_anchor)))
    # Now, report on the individual turns.
    turn_id = 0
    for turn, turn_dict in final_turn_list:
	s_output.append("\nTurn %d (%s)\n" % (turn_id, turn.GetValue()))
	turn_type = turn.GetValue()
	found_error, s_sample = validate_min_max([turn_type], turn_dict)
	s_output.append(s_sample)
	for key, values in turn_dict.items():
	    for v in values:
		if v.scope != turn_type:
		    found_error = 1
		    s_output.append("Element with attribute %s appears in the wrong scope (%s expected, %s found)." % (format_kv_pairs([v.true_kv]), turn_type, v.scope))
	if found_error == 0:
	    s_output.append("No errors found.")
	turn_id = turn_id + 1
    log_dir.io.PrintString(string.joinfields(s_output, "\n"), log_dir.formatter)
