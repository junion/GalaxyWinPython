# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# So what I want to do here is use the xml_tree to digest a 
# rule, and then transduce.

import xml_tree, string, re, sys, url_directory
from xml_tree import Tag, Data
import GC_log_formatting
from GC_log_formatting import GetInterestingFormatClass

# Use string.upper to canonicalize the namespace.

def namespace_split(element):
    elts = string.splitfields(element, ":", 1)
    if len(elts) == 1:
	return elts[0], None
    else:
	return elts[1], string.upper(elts[0])

# Be sure to canonicalize the pattern when the rule is recorded.
# Each tag object should have a new_properties and new_children
# entry.

BadRuleFile = "BadRuleFile"

# When I initialize the rules, I should fail if an event rule
# is being hung on GC_DATA, and vice versa. 

# The pattern is now a list of elements.

class TagInfo:
    def __init__(self, tag):
	# The tag is an XML tag (instance of
	# GC_log_formatting.FormattableTag)
	self.tag = tag
	# Properties, new_properties, new_match_properties
	# and negative_properties
	# are property sets corresponding to simple requirements,
	# properties to add ("new:" prefix), properties to
	# copy ("new:" attribute with "old:" value), and properties
	# not to have ("not:" prefix).
	self.properties = {}
	self.new_properties = {}
	self.new_match_properties = {}
	self.negative_properties = {}
	# Children and new_children are the child tags divided
	# by whether they have the "new:" prefix or not).
	self.children = []
	self.new_children = []
	# This is the name of self.tag, basically.
	self.name = ""
	# This tells whether the match
	# should be a positive or a negative one.
	self.positive = 1
    
    def Apply(self, tag, match_dict):
	for c in self.new_children:
	    # I can't really add the rule child, because
	    # in order for a tag to be recognized as bearing
	    # interesting information, it must be formattable.
	    # I have to copy it recursively, too.	    
	    tag.AddChild(c.Copy({"Tag": GC_log_formatting.FormattableTag}))
	for attr_to_add, val_to_add in self.new_properties.items():
	    # Here, we want to make sure that we add any values
	    # which correspond to old matches. The new match properties
	    # will help us with that.	    
	    tag.properties[attr_to_add] = val_to_add
	for attr_to_add, pattern_tag_to_use in self.new_match_properties.items():
	    tag.properties[attr_to_add] = match_dict[pattern_tag_to_use]
	
    # A rule matches if the current tag_obj_match matches the
    # tag and all its non-new attribute values match using the regexp.
    # We need to accumulate matches of regexps now, in order to
    # cache and reuse the results in the new: settings.
    # The toplevel element is a RULE, so we need to ignore it.
    # It serves only as a "holder" for a list of children and
    # for potential new nodes.

    def Match(self, rule_base_instance, tag_obj, rule):
	accumulator = RuleAccumulator(rule)
	if not isinstance(tag_obj, Tag):
	    return 0, None
	if self.name == "RULE":
	    return self.perhaps_match_tag_children(rule_base_instance,
						   tag_obj, accumulator,
						   is_topmost = 1)
	elif self.name != string.upper(tag_obj.name):
	    return 0, None
	# We test the negative properties first.
	for key, value in self.negative_properties.items():
	    # For a negative property to induce a failure, it
	    # must be both present in the tag being tested
	    # and its regex must match completely the tag being tested.
	    if tag_obj.properties.has_key(key):
		m = value.match(tag_obj.properties[key])
		if m and (m.end() == len(tag_obj.properties[key])):
		    # If it matches, fail.
		    return 0, None
	for key, value in self.properties.items():
	    if not tag_obj.properties.has_key(key):
		return 0, None
	    m = value.match(tag_obj.properties[key])
	    if (m is None) or (m.end() != len(tag_obj.properties[key])):
		return 0, None
	    for sub_key in value.groupindex.keys():
		accumulator.match_dict[sub_key] = m.group(sub_key)
	# We accumulate the local impact of the tag_obj_match in
	# perhaps_match_tag_children.
	return self.perhaps_match_tag_children(rule_base_instance,
					       tag_obj, accumulator)

    def perhaps_match_tag_children(self, rule_base_instance, \
				   tag_obj, accumulator, is_topmost = 0):
	# Each match child should match a different
	# child in the tag object, and the match children
	# need to be matched IN ORDER.
	# SAM 1/11/00: Actually, some need to be ordered,
	# and others don't. For instance, GC_DATA
	# tags don't need to be ordered with respect to
	# each other. So we need two different
	# match types. However, it's important to
	# observe that unordered matching is really a PAIRWISE
	# thing. If all the children are GC_DATA or GC_LIST,
	# I'll allow it, otherwise no. Note also that if
	# the node itself is GC_LIST, it should be disallowed.
	
	unordered = self.is_unordered_tag_match()
	
	# Copy it, because we're going to remove stuff.
	
	eligible_children = tag_obj.children[:]
	
	# SAM 4/21/00: we want to take negative children into account.
	# The algorithm is that if children are unordered, any
	# negative children have global extent (that is, if the
	# negative node matches, the overall match fails). But if
	# the children are ordered, it's a little more subtle.
	# Currently, we implement a "nearest match"; we want to
	# continue to support that. Sequences of negative
	# nodes should be unordered (so that B A can't be legal
	# according to a pattern of not-A not-B because B matches
	# not-A and A matches not-B). The right algorithm, then,
	# should be to do the positive match, collecting the negative
	# intervening nodes and the positive target spans,
	# and make sure that the intervening
	# negative nodes in the pattern don't violate the
	# spans. Finally, make sure we deal with the edge conditions
	# (first or last element being a negation).
	# So we set up a list of pairs of spans, where
	# the first element is the list of negations and
	# the second element is the list of potential matches.
	# In the unordered case, the first element will contain
	# all the negative match children, and the second will contain
	# all the target children.

	# SAM 6/9/00: Every time we apply a rule, we need to make sure that
	# we need to separate some of the accumulated changes from
	# the next round. This is for cases where the effect of the
	# accumulator is postponed. The one case that's relevant right
	# now is that the match_dict is collected, and applied later
	# when reviewing the tags. Somehow, we need to know when
	# to retain the record of the labeled pattern matches and
	# when to flush it.

	# My best guess is that we want to flush when we're processing
	# topmost children (that is, children of RULE). But we also
	# need to capture the state of that accumulator. We should be
	# able to do this by flushing the match_dict at the appropriate
	# time, as well as associating it with the relevant accumulated
	# tags.

	success, eligible_children = self.perhaps_match_eligible_tag_children(accumulator, unordered, eligible_children, rule_base_instance)
	
	some_success = success
	
	while success and eligible_children:

	    # Each call to perhaps_match_eligible_tag_children
	    # gathers up attributes and children, and associates
	    # the current attributes with the children. At the
	    # top level, we want to flush the match dict before
	    # each call.
	    
	    if is_topmost:
		accumulator.FlushMatchDict()
	    
	    # If we've achieved success, I think we want to match
	    # as many times as we can. So we should feed the processing
	    # back in and iterate as many times as we can.

	    success, eligible_children = self.perhaps_match_eligible_tag_children(accumulator, unordered, eligible_children, rule_base_instance)
	    
	    some_success = success or some_success
	    
	if some_success:
	    
	    # At this point, all the children and all the tags
	    # have matched. So what I want to do is return the
	    # accumulated changes, along with the local changes.
	
	    accumulator.AddToRule(tag_obj, self)

	    return 1, accumulator
	
	else:
	    
	    return 0, None


    def perhaps_match_eligible_tag_children(self, accumulator, unordered,
					    eligible_children,
					    rule_base_instance):
	negation_pairs = [[[], []]]

	tag_obj_match_children = self.children

	# If there are no match children, then the match succeeds
	# by default. However, if we want to apply the match
	# as many times as possible, we don't want to invoke the
	# match a second time when there were no children to match
	# the first time, since it will just keep on matching. So
	# we "get rid of" the eligible children if there are no
	# match children.

	if len(tag_obj_match_children) == 0:

	    eligible_children = []

	# SAM 10/18/01: Horrible bug. The parent accumulator
	# was absorbing the results each time a child matched,
	# not after all the children matched. This meant that
	# partial matches were being accumulated, and if there
	# was a successful match for one set of children and
	# then only a partially successful match for another
	# set, the partial match was being recorded by accident.
	
	sub_accumulators = []
	
	for match_child in tag_obj_match_children:
	    success = 0
	    sub_tag_info = accumulator.rule.tag_info_dict[match_child]
	    if not sub_tag_info.positive:
		# Just collect it, don't even test it yet.
		negation_pairs[-1][0].append(sub_tag_info)
		continue
	    for i in range(len(eligible_children)):
		success, sub_accumulator = sub_tag_info.Match(rule_base_instance, eligible_children[i], accumulator.rule)
		# First, the accumulation for potential negation.
		# If unordered, always gather.
		if unordered:
		    negation_pairs[-1][1].append(eligible_children[i])
		elif success:
		    # In the ordered case, if you succeed, you close
		    # off the span.
		    negation_pairs.append([[], []])
		else:
		    # In the ordered failure case, you collect the child
		    # for potential negation, I think.
		    negation_pairs[-1][1].append(eligible_children[i])
		if success:
		    sub_accumulators.append(sub_accumulator)
		    # accumulator.Absorb(sub_accumulator)
		    if unordered:
			eligible_children.remove(eligible_children[i])
		    else:
			eligible_children = eligible_children[i+1:]
		    break
	    if not success:
		return 0, eligible_children
	
	# Finally, if there are eligible children left, you
	# append them to the end of the current negation_pairs.
	
	negation_pairs[-1][1] = negation_pairs[-1][1] + eligible_children
	
	# Now, check the negations. For each set of pairs, if any of
	# the candidate children match any of the tags, fail.
	
	for [tag_list, child_list] in negation_pairs:
	    for t in tag_list:
		for c in child_list:
		    if t.Match(rule_base_instance, c, accumulator.rule)[0]:
			# We matched a negative.
			return 0, eligible_children

	# Now, accumulate.

	for s in sub_accumulators:
	    accumulator.Absorb(s)
	
	return 1, eligible_children
	    
    def is_unordered_tag_match(self):
	# If this is a list, it's ordered. Otherwise,
	# if all of the children are data of one kind
	# or another, it's unordered.
	if self.name == "GC_LIST":
	    return 0
	for elt in self.children:
	    if not (isinstance(elt, Tag) and \
		    elt.name in ["GC_LIST", "GC_DATA"]):
		return 0
	    return 1

class DataInfo:
    def __init__(self, tag, regex):
	self.tag = tag
	self.regex = regex
	self.positive = 1
    def Match(self, rule_base_instance, tag_obj, rule):
	accumulator = RuleAccumulator(rule)
	if not isinstance(tag_obj, Data):
	    return 0, None
	match_obj = self.regex
	m = match_obj.match(tag_obj.data)
	if (m is None) or (m.end() != len(tag_obj.data)):
	    return 0, None
	else:
	    # Collect the match info.
	    for key in self.regex.groupindex.keys():
		accumulator.match_dict[key] = m.group(key)
	    return 1, accumulator

# We want to be able to handle negation of various sorts:
# negative attributes and negative children. The logic of the
# latter is a little obscure, but we'll work on it.

class XMLTransductionRule:
    def __init__(self, pattern):
	self.pattern = pattern
	self.properties = self.pattern.properties
	if self.properties is None:
	    self.properties = {}
	self.tag_info_dict = {}
	# Now, canonicalize recursively.
	self.canonicalize(self.pattern)
	# Now, validate new/old name matches.
	self.validate(self.pattern)
    def canonicalize(self, tag_obj):
	if isinstance(tag_obj, Tag):
	    self.canonicalize_tag(tag_obj)
	elif isinstance(tag_obj, Data):
	    self.canonicalize_data(tag_obj)
	else:
	    raise BadRuleFile, "Only tags and data permitted in rule file"
    def validate(self, tag_obj):
	# Once it's canonicalized, I need to make sure that
	# there's no duplication in the regex namespaces and that
	# there's a match for each match property.
	tag_names_provided, tag_names_to_use = self.collect_tag_names(tag_obj, [], [])
	for tag in tag_names_to_use:
	    if tag not in tag_names_provided:
		raise BadRuleFile, ("Pattern name `%s' not found in rule" % tag)
    def collect_tag_names(self, obj, cur_list_provided, cur_list_to_use):
	if isinstance(obj, Tag):
	    obj = self.tag_info_dict[obj]
	    for elt in obj.new_match_properties.values():
		if elt not in cur_list_to_use:
		    cur_list_to_use.append(elt)
	    for r_obj in obj.properties.values():		
		for key in r_obj.groupindex.keys():
		    if key in cur_list_provided:
			raise BadRuleFile, ("Duplicate pattern name `%s' in rule" % key)
		    cur_list_provided.append(key)
	    for child in obj.children:
		cur_list_provided, cur_list_to_use = self.collect_tag_names(child, cur_list_provided, cur_list_to_use)
	elif isinstance(obj, Data):	    
	    obj = self.tag_info_dict[obj]
	    for key in obj.regex.groupindex.keys():
		if key in cur_list_provided:
		    raise BadRuleFile, ("Duplicate pattern name `%s' in rule" % key)
		cur_list_provided.append(key)
	return cur_list_provided, cur_list_to_use
    def canonicalize_data(self, data_obj):
	self.tag_info_dict[data_obj] = DataInfo(data_obj, re.compile(data_obj.data))
    def canonicalize_tag(self, tag_obj):	
	t = TagInfo(tag_obj)
	self.tag_info_dict[tag_obj] = t
	t.name = namespace_split(tag_obj.name)[0]
	# Don't do anything wacky to the RULE tag properties, since
	# we don't compare them and they're needed in the Apply method.
	if t.name != "RULE":
	    self.canonicalize_properties(t, tag_obj)	
	final_children = []
	for obj in tag_obj.children:
	    if isinstance(obj, Tag):
		sp = namespace_split(obj.name)[1]
		if sp == "NEW":
		    obj.name = namespace_split(obj.name)[0]
		    t.new_children.append(obj)
		elif sp == "NOT":
		    self.canonicalize(obj)
		    final_children.append(obj)
		    self.tag_info_dict[obj].positive = 0
		else:
		    self.canonicalize(obj)
		    final_children.append(obj)
	    else:
		self.canonicalize(obj)
		final_children.append(obj)
	t.children = final_children
    def canonicalize_properties(self, t, tag_obj):
	for key in tag_obj.properties.keys():
	    # For any attribute whose namespace isn't "new",
	    # turn the value into a compiled case-insensitive regexp.
	    if namespace_split(key)[1] == "NEW":
		old_prop = tag_obj.properties[key]
		# Now, validate the location of the tag. If the
		# tag of this property isn't in the list of
		# legal tags for the class associated with the
		# tag, barf.
		new_prop_key = namespace_split(key)[0]
		if namespace_split(old_prop)[1] == "OLD":
		    t.new_match_properties[new_prop_key] = namespace_split(old_prop)[0]
		else:		    
		    c = GetInterestingFormatClass(new_prop_key, old_prop)
		    if c is None:
			sys.stderr.write("Warning: encountered rule adding unknown attribute-value pair %s=\"%s\"\n" % (new_prop_key, old_prop))
		    elif t.name not in c.legal_tags:
			raise BadRuleFile, ("Attribute %s cannot live on tag %s; eligible tags are %s" % (new_prop_key, t.name, string.joinfields(c.legal_tags, ", ")))
		    t.new_properties[new_prop_key] = old_prop
	    elif namespace_split(key)[1] == "NOT":
		# This is an attribute which may not occur.
		regexp = re.compile(tag_obj.properties[key], re.I)
		t.negative_properties[key] = regexp
	    else:
		regexp = re.compile(tag_obj.properties[key], re.I)
		t.properties[key] = regexp

# The rules file has the following form:
# <RULES>
# <GC_LOG_VERSION>...</GC_LOG_VERSION>
# <OR> <RULE> ....
# <RULE> ....
# The GC_LOG_VERSION is optional.

# rules is now a list of alternatives (note the new OR tag).
# SAM 4/18/00: Also adding negation, both at the attribute and
# tag levels.

def digest_rule_file(file, io):
    rules = []
    logfile_versions = []
    if isinstance(file, url_directory.BasePath):
	io.ReportStatus("Reading rule file: " + file.PathString())
    else:
	io.ReportStatus("Reading rule file: " + file)
    d = xml_tree.xml_to_tree(file)
    if string.lower(d.children[0].name) != "rules":
	raise BadRuleFile, "Toplevel tag must be RULES"
    for child in d.children[0].children:
	pre = None
	post = None
	if (not isinstance(child, Tag)) or \
	   (string.lower(child.name) not in ["gc_log_version", "rule", "or"]):
	    io.ReportStatus("Ignoring %s element" % child.__class__.__name__)
	elif string.lower(child.name) == "gc_log_version":
	    if isinstance(child.children[0], Data):
		logfile_versions.append(child.children[0].data)
	elif string.lower(child.name) == "or":
	    rules.append(digest_real_rules(child.children))
	else:
	    rules.append(digest_real_rules([child]))
    io.ReportStatus("...read.")
    return RuleBaseInstance(d, rules, logfile_versions)

# SAM 4/18/00: Relaxed the requirement that RULE needs a single
# child. Not sure why I needed it in the first place. This required
# some additional changes to handle the fact that the rule pattern
# is now the RULE node itself.

def digest_real_rules(rule_list):
    rules = []
    for child in rule_list:
	if string.lower(child.name) != "rule":
	    raise BadRuleFile, ("Inappropriate tag %s where rule should be" % child.name)
	num_new = 0
	num_not = 0
	for rchild in child.children:
	    if isinstance(rchild, Tag):
		sp = namespace_split(rchild.name)[1]
		if sp == "NEW":
		    num_new = num_new + 1
		elif sp == "NOT":
		    num_not = num_not + 1
	if (num_new + num_not) == len(child.children):
	    # Can't have all new.
	    raise BadRuleFile, \
		  "toplevel rule elements can't all be `new' or `not'"
	rules.append(XMLTransductionRule(child))
    return rules

# There are two things you can do: add an attribute to a tag,
# add a tag to a parent tag. We can use regular expressions as
# the value of an attribute. I think the algorith goes:
# Start recursing down. Below GC_DATA, no tags are analyzed, only data.
# No regular expressions for an attribute, only for values.

# The RuleAccumulator does all the work of collecting the information
# about its enclosed tags for later application if everything matches.

class RuleAccumulator:
    def __init__(self, rule):
	self.rule = rule
	self.match_dict = {}
	self.tag_accumulators = []
    def AddToRule(self, tag, info):
	# I include a pointer to the match dict, which
	# will be passed to the parent and mostly replaced.
	# At the top level, the match_dict will be flushed
	# so that multiple matches will have the appropriate
	# scope.
	self.tag_accumulators.append((tag, info, self.match_dict))
    def FlushMatchDict(self):
	# Flushing so that a new match dict is saved later.
	self.match_dict = {}
    def Apply(self):
	for tag, info, match_dict in self.tag_accumulators:
	    info.Apply(tag, match_dict)
    def Absorb(self, sub_accumulator):
	# Incorporate successful changes. This will
	# happen recursively.
	for key, value in sub_accumulator.match_dict.items():
	    self.match_dict[key] = value
	for tag, info, old_match_dict in sub_accumulator.tag_accumulators:
	    self.tag_accumulators.append((tag, info, self.match_dict))

InapplicableVersion = "InapplicableVersion"

MatchableTags = ["GC_LOG", "GC_SESSION", "GC_TURN",
		 "GC_OPERATION", "GC_MESSAGE",
		 "GC_EVENT", "GC_DATA"]

# rules is now a list of alternatives.

class RuleBaseInstance:
    def __init__(self, doc, rules, versions):
	self.legal_versions = versions
	self.rules = rules
	self.doc = doc
	self.rule_accumulation = {}
	self.Reset()
    def Reset(self):
	for r_list in self.rules:
	    for r in r_list:
		self.rule_accumulation[r] = []
    def apply_rules(self, cur_root):
	# The only child of the first root will be
	# a GC_LOG tag, and in that we might find logfile_version.
	version = cur_root.DocumentProperty("logfile_version")
	if (version is not None) and \
	   (version not in self.legal_versions):
	    raise InapplicableVersion, version
	self.recursive_apply_rules(cur_root)
	for rule_alternatives in self.rules:
	    for rule in rule_alternatives:
		self.Apply(rule)
	return cur_root
    
    def recursive_apply_rules(self, cur_root):
	for child in cur_root.children:
	    if isinstance(child, Tag) and \
	       string.upper(child.name) in MatchableTags:
		# Try to match the elements, and then recurse
		# on the children.
		self.perhaps_apply_rules(child)
		if string.upper(child.name) != "GC_DATA":
		    # Don't recurse into data.
		    self.recursive_apply_rules(child)

    # The tag_additions list is a list of pairs whose first element
    # is the tag to add to, and the second is the tag to add.
    # The attr_additions list is a list of triples whose first element
    # is the tag to add to, second is the attribute to add, and third
    # is the value.

    # This function here is called for side effect.

    def perhaps_apply_rules(self, tag_obj):
	for rule_alternatives in self.rules:
	    for rule in rule_alternatives:
		if self.Update(tag_obj, rule):
		    break

    def Update(self, tag_obj, rule):
	tag_info = rule.tag_info_dict[rule.pattern]
	success, accumulator = tag_info.Match(self, tag_obj, rule)
	if success:
	    self.rule_accumulation[rule].append(accumulator)
	    return 1	
	else:
	    return 0

    def Apply(self, rule):
	accumulators = self.rule_accumulation[rule]
	occurrences = "all"
	if rule.properties.has_key("occurrences"):
	    occurrences = rule.properties["occurrences"]
	if occurrences == "first":
	    if accumulators:
		accumulators = [accumulators[0]]
	elif occurrences == "last":
	    if accumulators:
		accumulators = [accumulators[-1]]
	for a in accumulators:
	    # Now, apply the damn things.
	    a.Apply()
	
# This is a utility to ensure that you have a rule base instance.

def create_rule_base_instance(r, io):
    if not isinstance(r, RuleBaseInstance):
	# r may be a file name or a Path object.
	r = digest_rule_file(r, io)
    return r
