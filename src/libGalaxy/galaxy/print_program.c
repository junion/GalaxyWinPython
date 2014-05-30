/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include "galaxy/program.h"

static void __Gal_IFormatEntity(Gal_ProgramEntity *e, int pedantic,
				Gal_StringBuffer **bufptr)
{
  Gal_NamespaceProgramEntity *ne;
  Gal_ComplexProgramEntity *ce;
  int i;
  Gal_StringBuffer *buf = *bufptr;
  
  switch(e->entity_type) {
  case GAL_OBJECT_ENTITY:
    Gal_ObjectStringToBuffer((Gal_Object) e->entity_data, buf);
    break;
  case GAL_NAMESPACE_ENTITY:
    ne = (Gal_NamespaceProgramEntity *) e->entity_data;
    if (ne->is_default && !pedantic) {
      Gal_StringBufferWrite(buf, -1, ne->key);
    } else {
      int increment;
      increment =  strlen(ne->key) + strlen(ne->namespace_obj->namespace_name) + 7;
      Gal_StringBufferWrite(buf, increment, "$in(%s %s)",
			    ne->key, ne->namespace_obj->namespace_name);
    }
    break;
  case GAL_COMPLEX_ENTITY:
    ce = (Gal_ComplexProgramEntity *) e->entity_data;
    /* Print the name, the parentheses, and then each argument. */
    Gal_StringBufferWrite(buf, -1, ce->descriptor->name);
    Gal_StringBufferWrite(buf, -1, "(");
    for (i = 0; i < ce->num_args; i++) {
      if (i > 0) {
	Gal_StringBufferWrite(buf, -1, " ");
      }
      __Gal_IFormatEntity(ce->args[i], pedantic, bufptr);
    }
    Gal_StringBufferWrite(buf, -1, ")");
    break;
  default:
    break;
  }
}

char *Gal_FormatEntity(Gal_ProgramEntity *e, int pedantic,
		       Gal_StringBuffer **bufptr)
{
  if (!*bufptr) {
    *bufptr = Gal_MakeStringBuffer((char *) NULL, 0);
  } else {
    /* Rewind! */
    (*bufptr)->bufpos = 0;
  }
  __Gal_IFormatEntity(e, pedantic, bufptr);
  return (*bufptr)->buf;
}

void Gal_PrintEntity(Gal_ProgramEntity *e, int pedantic)
{
  Gal_StringBuffer *buf = Gal_MakeStringBuffer((char *) NULL, 0);

  GalUtil_Print(-1,"%s\n", Gal_FormatEntity(e, pedantic, &buf));
  Gal_FreeStringBuffer(buf);
}	 

/* print a Gal_TestClause list in program format */

static void __Gal_PrintChildPredicate(FILE *fp, Gal_TestClause *c, int pedantic)
{
  switch (c->clause_type) {
  case GAL_SIMPLE_CLAUSE:
    Gal_PrintTests(fp, c, pedantic);
    break;
  case GAL_LOGICAL_OPERATOR_CLAUSE:
    GalUtil_fprintf(fp, "( ");
    Gal_PrintTests(fp, c, pedantic);
    GalUtil_fprintf(fp, " )");
  } 
}

void Gal_PrintTests(FILE *fp, Gal_TestClause *c, int pedantic)
{
  Gal_LogicalOperatorTestClause *lotc;
  Gal_SimpleTestClause *stc;
  int i;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;
  char *s;
  
  switch (c->clause_type) {
  case GAL_SIMPLE_CLAUSE:
    stc = (Gal_SimpleTestClause *) c->clause_data;
    switch (stc->pred_type) {
    case GAL_TEST_HAS_KEY:
      /* Just print the entity. */
      if (!pedantic) {
	s = Gal_FormatEntity(stc->args[0], pedantic, &buf);
	if (s) GalUtil_fprintf(fp, s);
	break;
      }
      /* Otherwise, fall through. */
    default:
      s = Gal_FormatEntity(stc->args[0], pedantic, &buf);
      if (s) GalUtil_fprintf(fp, s);
      /* Now the operation. */
      GalUtil_fprintf(fp, " %s", Gal_ProgramTagString(stc->pred_type));
      /* Now all the resg of the args. */
      for (i = 1; i < stc->num_args; i++) {
	GalUtil_fprintf(fp, " ");
	s = Gal_FormatEntity(stc->args[i], pedantic, &buf);
	if (s) GalUtil_fprintf(fp, s);
      }
    }
    break;
  case GAL_LOGICAL_OPERATOR_CLAUSE:
    lotc = (Gal_LogicalOperatorTestClause *) c->clause_data;
    switch (lotc->logical_operator) {
    case GAL_TEST_NOT:
      /* Print a bang, and then the one argument. */
      GalUtil_fprintf(fp, "%s ", Gal_ProgramTagString(lotc->logical_operator));
      __Gal_PrintChildPredicate(fp, lotc->args[0], pedantic);
      break;
    default:
      /* For all the rest, loop and put the connective
	 in between. */
      for (i = 0; i < lotc->num_args; i++) {
	if (i > 0) {
	  GalUtil_fprintf(fp, " %s ", Gal_ProgramTagString(lotc->logical_operator));
	}
	__Gal_PrintChildPredicate(fp, lotc->args[i], pedantic);
      }
    }
  }
  if (buf)
    Gal_FreeStringBuffer(buf);
}

/* print a Gal_ConditionStruct in program format */

void Gal_PrintCondition(FILE *fp, Gal_ConditionStruct *condition, char *tag, int pedantic)
{
  if (condition == NULL)
    return;

  if (tag)
    GalUtil_fprintf(fp, "%s ", tag);
  if (condition->tests)
  {
    Gal_PrintTests(fp, condition->tests, pedantic);
    GalUtil_fprintf(fp, " ");
  }
  GalUtil_fprintf(fp, "--> ");
  if (condition->server)
    GalUtil_fprintf(fp, "%s.", condition->server);
  GalUtil_fprintf(fp, "%s\n", condition->operation);
}
