# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This file will take an annotated log and produce a stub
# file for the human annotation, using the SR output.

import xml_interleave, xml_tree, string
from GC_log_formatting import UserSR, UserTranscription, ConceptAnnotation, \
     TaskCompletion, FormattableTag, FormattableXMLDocument

def CreateInterestingTag(log, dict, tag_name, data):
    t = FormattableTag(tag_name, dict)
    if data is not None:
	t.AddChild(xml_tree.Data(data))
    t.AddLogAndFormatClasses(log)
    return t.Extract()[0]

def GenerateHumanAnnotationStub(log_summary, task_range):
    log_dir = log_summary.log_dir
    human_seeds = log_summary.FindInterestingElements([UserSR], task_range = task_range)[1]
    for [turn, seeds] in human_seeds:
	for eclass, seed in seeds:
	    dp = CreateInterestingTag(log_dir,
				      {"type_utt_text":
				       "transcription",
				       "dtype": "string"},
				      "GC_DATA",
				      seed.GetData())
	    turnid, tidx = seed.GetLocation()
	    if turnid is not None:
		dp.original_dp.properties["turnid"] = turnid
	    if tidx is not None:
		dp.original_dp.properties["tidx"] = tidx
	    log_summary.PlaceInTurn(seed, dp)
    # I'd put a placeholder here, but then it won't get
    # marked as interesting.
    dp = CreateInterestingTag(log_dir, {"type_task_completion": "0"},
			      "GC_ANNOT", None)
    log_summary.global_list.append(dp)
    return CreateHumanAnnotation(log_summary, task_range).render(pprint = 1)

def CreateHumanAnnotation(log_summary, task_range = xml_interleave.WITHIN_TOTAL_TASK):
    session = FormattableTag("GC_SESSION")
    global_l, local_l = log_summary.FindInterestingElements([TaskCompletion, UserTranscription, ConceptAnnotation], turn_segmentation = xml_interleave.NO_TURNS, task_range = task_range)
    final = []
    for eclass, l in global_l + local_l:
	if l.original_dp not in final:
	    final.append(l.original_dp)
    # AddChild sets parent link; we don't want to do that.
    session.children = final
    annot = FormattableTag("GC_LOG_ANNOTATIONS")
    annot.children = [session]
    new_doc = FormattableXMLDocument()
    new_doc.children = [annot]
    return new_doc
