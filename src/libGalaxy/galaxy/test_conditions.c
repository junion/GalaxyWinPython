/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"

#include "galaxy/util.h"
#include "galaxy/program.h"

static int object_string_value(Gal_Object value, char **string_value_ptr)
{
  Gal_Object first;
  char *string_value = NULL;

  if (Gal_Stringp(value))
  {
    string_value = Gal_StringValue(value);
  }
  else if (Gal_Framep(value))
  {
    string_value = Gal_FrameName(Gal_FrameValue(value));
  }
  else if (Gal_Listp(value) && (first = Gal_GetListObject(value, 0)) && Gal_Stringp(first))
  {
    string_value = Gal_StringValue(first);
  }

  *string_value_ptr = string_value;
  if (string_value)
    return 1;

  return 0;
}

static int object_int_value(Gal_Object value, int *int_value_ptr)
{
  if (Gal_Intp(value))
  {
    *int_value_ptr = Gal_IntValue(value);
    return 1;
  }
  if (Gal_Listp(value))
  {
    *int_value_ptr = Gal_ListLength(value);
    return 1;
  }
  return 0;
}

Gal_Boolean _gal_evaluate_has_key_test(Gal_SimpleTestClause *stc,
				       Gal_Frame *frame_array, int default_namespace)
{
  int newly_created;
  Gal_Object o = Gal_GetProgramEntity(stc->args[0], frame_array,
				      &newly_created);
  
  if (o) {
    if (newly_created)
      Gal_FreeObject(o);
    return GAL_TRUE;
  } else {
    return GAL_FALSE;
  }
}

Gal_Boolean _gal_evaluate_string_test(Gal_SimpleTestClause *stc,
				      Gal_Frame *frame_array, int default_namespace)
{
  int success;
  Gal_Object o1;
  Gal_Object o2;
  char *s1 = (char *) NULL;
  char *s2 = (char *) NULL;
  int o1_newly_created;
  int o2_newly_created;
  int failed = 0;
  int succeeded = 0;

  o1 = Gal_GetProgramEntity(stc->args[0], frame_array, &o1_newly_created);
  if (!o1)
    return GAL_FALSE;
  o2 = Gal_GetProgramEntity(stc->args[1], frame_array, &o2_newly_created);
  if (!o2) {
    if (o1_newly_created) Gal_FreeObject(o1);
    return GAL_FALSE;
  }
  
  if (!stc->extended_syntax) {
    success = object_string_value(o1, &s1);
    if (!success) {
      failed = 1;
    } else {
      success = object_string_value(o2, &s2);
      if (!success) {
	failed = 1;
      }
    }
  } else if (Gal_Stringp(o1) && Gal_Stringp(o2)) {
    s1 = Gal_StringValue(o1);
    s2 = Gal_StringValue(o2);
  } else {
    failed = 1;
  }

  if (!failed) {
    switch (stc->pred_type) {
    case GAL_TEST_STRING_EQ:
      if (s1 && s2 && !strcmp(s1, s2))
	succeeded = 1;
      break;
    case GAL_TEST_STRSTR:
      if (strstr(s1, s2))
	succeeded = 1;
      break;
    }
  }

  if (o1_newly_created) Gal_FreeObject(o1);
  if (o2_newly_created) Gal_FreeObject(o2);

  if (succeeded)
    return GAL_TRUE;
  else
    return GAL_FALSE;
}

Gal_Boolean _gal_evaluate_eq_test(Gal_SimpleTestClause *stc,
				  Gal_Frame *frame_array, int default_namespace)
{
  int success;
  Gal_Object o1;
  Gal_Object o2;
  char *s1 = (char *) NULL;
  int i;
  int o1_newly_created;
  int o2_newly_created;
  int succeeded = 0;
  
  /* We should be able to compare floats in the new regime.
     We should also be able to extend the old regime fairly
     easily to do it. */     
  
  o1 = Gal_GetProgramEntity(stc->args[0], frame_array,
			    &o1_newly_created);
  if (!o1)
    return GAL_FALSE;
  o2 = Gal_GetProgramEntity(stc->args[1], frame_array,
			    &o2_newly_created);
  if (!o2) {
    if (o1_newly_created) Gal_FreeObject(o1);
    return GAL_FALSE;
  }

  if (Gal_Floatp(o1) && Gal_Floatp(o2)) {
    if (Gal_FloatValue(o1) == Gal_FloatValue(o2))
      succeeded = 1;
  } else if (!stc->extended_syntax) {
    /* In the old syntax, we could only have strings and ints.
     And the second element will always be a literal. So the first
     thing we do is see what the type of the second element is. */
    switch (Gal_GetObjectType(o2)) {
    case GAL_STRING:
      success = object_string_value(o1, &s1);
      if (success && s1 && Gal_StringValue(o2) && !strcmp(s1, Gal_StringValue(o2)))
	succeeded = 1;
      break;
    case GAL_INT:
      success = object_int_value(o1, &i);
      if (success && (i == Gal_IntValue(o2)))
	succeeded = 1;
      break;
    default:
      break;
    }
  } else if (Gal_ObjectEqual(o1, o2)) {
    succeeded = 1;
  }

  if (o1_newly_created) Gal_FreeObject(o1);
  if (o2_newly_created) Gal_FreeObject(o2);

  if (succeeded)
    return GAL_TRUE;
  else  
    return GAL_FALSE;
}

Gal_Boolean _gal_evaluate_num_test(Gal_SimpleTestClause *stc,
				   Gal_Frame *frame_array, int default_namespace)
{
  int success;
  Gal_Object o1;
  Gal_Object o2;
  int i, j;
  int o1_newly_created;
  int o2_newly_created;
  int succeeded = 0;
  
  o1 = Gal_GetProgramEntity(stc->args[0], frame_array,
			    &o1_newly_created);
  if (!o1)
    return GAL_FALSE;
  o2 = Gal_GetProgramEntity(stc->args[1], frame_array,
			    &o2_newly_created);
  if (!o2) {
    if (o1_newly_created) Gal_FreeObject(o1);
    return GAL_FALSE;
  }

  /* We should be able to compare floats in the new regime.
     We should also be able to extend the old regime fairly
     easily to do it. */
  
  if (Gal_Floatp(o1) && Gal_Floatp(o2)) {
    switch (stc->pred_type) {
    case GAL_TEST_GT:
      if (Gal_FloatValue(o1) > Gal_FloatValue(o2))
	succeeded = 1;
      break;
    case GAL_TEST_LT:
      if (Gal_FloatValue(o1) < Gal_FloatValue(o2))
	succeeded = 1;
      break;
    case GAL_TEST_GE:
      if (Gal_FloatValue(o1) >= Gal_FloatValue(o2))
	succeeded = 1;
      break;
    case GAL_TEST_LE:
      if (Gal_FloatValue(o1) <= Gal_FloatValue(o2))
	succeeded = 1;
      break;
    }
  } else {
    int failed = 0;
    
    if (!stc->extended_syntax) {
      success = object_int_value(o1, &i);
      if (!success) {
	failed = 1;
      } else {
	success = object_int_value(o2, &j);
	if (!success) {
	  failed = 1;
	}
      }
    } else if (Gal_Intp(o1) && Gal_Intp(o2)) {
      i = Gal_IntValue(o1);
      j = Gal_IntValue(o2);
    } else {
      failed = 1;
    }

    if (!failed) {
      switch (stc->pred_type) {
      case GAL_TEST_GT:
	if (i > j)
	  succeeded = 1;
	break;
      case GAL_TEST_LT:      
	if (i < j)
	  succeeded = 1;
	break;
      case GAL_TEST_GE:      
	if (i >= j)
	  succeeded = 1;
	break;
      case GAL_TEST_LE:      
	if (i <= j)
	  succeeded = 1;
	break;
      }
    }
  }
  
  if (o1_newly_created) Gal_FreeObject(o1);
  if (o2_newly_created) Gal_FreeObject(o2);

  if (succeeded)
    return GAL_TRUE;
  else  
    return GAL_FALSE;
}

Gal_Boolean _gal_evaluate_member_test(Gal_SimpleTestClause *stc,
				      Gal_Frame *frame_array,
				      int default_namespace)
{
  Gal_Object o1;
  Gal_Object o2;
  Gal_Object *l;
  int i, size = 0;
  int o1_newly_created;
  int o2_newly_created;
  int succeeded = 0;

  o1 = Gal_GetProgramEntity(stc->args[0], frame_array,
			    &o1_newly_created);
  if (!o1)
    return GAL_FALSE;
  o2 = Gal_GetProgramEntity(stc->args[1], frame_array,
			    &o2_newly_created);
  if (!o2) {
    if (o1_newly_created) Gal_FreeObject(o1);
    return GAL_FALSE;
  }

  if (Gal_Listp(o2)) {
    l = Gal_ListValue(o2, &size);
    for (i = 0; i < size; i++) {
      if (Gal_ObjectEqual(o1, l[i])) {
	succeeded = 1;
	break;
      }
    }
  }
  
  if (o1_newly_created) Gal_FreeObject(o1);
  if (o2_newly_created) Gal_FreeObject(o2);

  if (succeeded)
    return GAL_TRUE;
  else  
    return GAL_FALSE;
}

/* Test clause evaluation context. */

Gal_Boolean Gal_TestConditionInNamespaces(Gal_TestClause *c,
					  Gal_Frame *frame_array, int default_namespace)
{
  /* This will recurse into the test clause. */
  Gal_LogicalOperatorTestClause *lotc;
  Gal_SimpleTestClause *stc;
  
  switch (c->clause_type) {
  case GAL_SIMPLE_CLAUSE:
    stc = (Gal_SimpleTestClause *) c->clause_data;
    return (*stc->op)(stc, frame_array, default_namespace);
    break;
  case GAL_LOGICAL_OPERATOR_CLAUSE:
    lotc = (Gal_LogicalOperatorTestClause *) c->clause_data;
    return (*lotc->op)(lotc, frame_array, default_namespace);
  default:
    return GAL_FALSE;
  }  
}

Gal_Boolean Gal_TestCondition(Gal_Frame frame, Gal_TestClause *tests)
{
  /* Do it all in the default namespace. */
  return Gal_TestConditionInNamespaces(tests, &frame, 0);
}
