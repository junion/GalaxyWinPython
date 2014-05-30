/*
  Portions of this file (c) Copyright 1999 - 2000 M.I.T.
  Portions of this file (c) Copyright 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include "galaxy/program.h"
#include <ctype.h>
#include <stdarg.h>

static ProgramObject *copy_program_objects(ProgramObject *objects);
static Gal_Object program_next_object(Gal_ProgramParser *pp);
static int add_first_object(Gal_Object object, Gal_ProgramParser *pp);
static void add_next_object(Gal_Object object, int program_tag, Gal_ProgramParser *pp);
static Gal_Object __Gal_ReadObjectLine(Gal_ProgramParser *pp, int eol_char,
				       int comment_char, int esc_char);
static void __Gal_InitializeProgramObject(ProgramObject *p);
static void __Gal_FreeProgramObjectContents(ProgramObject *p);

/* Gal_FileStruct is for keeping track of the program file
 * and line/line number currently being processed */

/* push a file onto the list when it is opened */

Gal_FileStruct *Gal_PushProgramFile(const char *filename, FILE *fp, Gal_FileStruct *current_files)
{
  Gal_FileStruct *new_file = (Gal_FileStruct *)calloc(1, sizeof(Gal_FileStruct));
  if (new_file)
  {
    strcpy(new_file->filename, filename);
    new_file->fp = fp;
    if (current_files)
    {
      new_file->included_from = current_files;
    }
    GalUtil_PInfo1("Reading %s\n", filename);
    return(new_file);
  }
  return(NULL);
}

/* pop the file list when a file is closed */

Gal_FileStruct *Gal_PopProgramFile(Gal_FileStruct *current_files)
{
  if (current_files)
  {
    Gal_FileStruct *top = current_files;
    current_files = top->included_from;

    GalUtil_PInfo1("Done reading %s (%d lines)\n", top->filename, top->line_count);
    fclose(top->fp);
    free(top);
  }
  return(current_files);
}

/* decide whether it is necessary to prepend a directory to the path */

int Gal_IsFullPath(const char *path)
{
  if (!path)
    return 0;
#ifdef WIN32
  if (isalpha(path[0]) &&
      (strlen(path) > 2) &&
      (path[1] == ':') &&
      ((path[2] == GAL_DIRSEP) ||
       (path[2] == GAL_ALT_DIRSEP)))
    return 1;
  else
    return 0;
#else
  if (path[0] == GAL_DIRSEP)
    return 1;
  else
    return 0;
#endif
}

/* SAM 9/6/02: Just as I was wrapping up the distribution, I
   encountered some chuckleheadedness with manipulation of
   non-absolute pathnames on Windows. First, Gal_IsFullPath was
   just wrong. Second, GAL_DIRSEP can be forward or backward
   slash on Windows. Finally, in general there were several
   places that pathname construction was happening, which
   strikes me as a bad idea.

   The rest of these will not be exported, so I don't need
   to regenerate the docs, etc. */

int _Gal_CreateFullPath(char *path, int path_size, int num_components, ...)
{
  va_list components;
  char *component;
  int i;
  int cur_path_size;
  int cur_component_size;
  
  if (!path)
    return 0;

  /* Terminate the array. */
  path[0] = '\0';
  
  va_start(components, num_components);
  for (i = 0; i < num_components; i++) {
    component = va_arg(components, char *);
    cur_path_size = strlen(path);

    /* For components after the first one, don't
       include a lead separator, since the previous
       iteration ensured that there is a final separator. */
    if (i > 0) {
      if ((component[0] == GAL_DIRSEP)
#ifdef GAL_ALT_DIRSEP
	  || (component[0] == GAL_ALT_DIRSEP)
#endif
	  ) {
	component = component + 1;
      }
    }
    
    cur_component_size = strlen(component);
    
    /* For each component, if the last element is a path
       terminator, don't add a terminator at the end. */
    if ((cur_component_size + cur_path_size) < (path_size - 1)) {
      strcat(path, component);
      cur_path_size = strlen(path);
      if ((path[cur_path_size - 1] != GAL_DIRSEP)
#ifdef GAL_ALT_DIRSEP
	  && (path[cur_path_size - 1] != GAL_ALT_DIRSEP)
#endif
	  ) {
	/* Try to add a terminator. If you can't, don't
	   worry about it. */
	if (cur_path_size < (path_size - 2)) {
	  path[cur_path_size] = GAL_DIRSEP;
	  path[cur_path_size + 1] = '\0';
	}
      }
    } else {
      GalUtil_Warn("Truncating overlong path");
      strncat(path, component, (path_size - cur_path_size - 1));
      path[path_size - 1] = '\0';
      return 0;
    }
  }
  va_end(components);
  
  /* Don't let the last element be a terminator. */
  cur_path_size = strlen(path);
  if ((path[cur_path_size - 1] == GAL_DIRSEP)
#ifdef GAL_ALT_DIRSEP
      || (path[cur_path_size - 1] == GAL_ALT_DIRSEP)
#endif
      ) {
    path[cur_path_size - 1] = '\0';
  }
  return 1;
}

int _Gal_SplitPath(char *out_path, int path_size, const char *in_path)
{
  char *last_dirsep;
  char *last_alt_dirsep = (char *) NULL;
  int in_path_len;
  int result_len;
  
  if (!in_path) {
    if (out_path) {
      out_path[0] = '\0';
    }
    return 0;
  }

  in_path_len = strlen(in_path);
  
  last_dirsep = strrchr(in_path, GAL_DIRSEP);
#ifdef GAL_ALT_DIRSEP
  last_alt_dirsep = strrchr(in_path, GAL_ALT_DIRSEP);
#endif

  if (last_dirsep && last_alt_dirsep) {
    /* Take the shorter of the two. */
    if (strlen(last_alt_dirsep) < strlen(last_dirsep)) {
      last_dirsep = last_alt_dirsep;
    }
  } else if (!last_dirsep) {
    last_dirsep = last_alt_dirsep;
  }

  result_len = in_path_len - strlen(last_dirsep);
  
  if (!last_dirsep) {
    out_path[0] = '\0';
    return 1;
  } else if (result_len < (path_size - 1)) {
    /* If there's room for the whole thing, copy it. */
    strncpy(out_path, in_path, result_len);    
    out_path[result_len] = '\0';
    return 1;
  } else {
    /* Not room for the whole thing. */
    GalUtil_Warn("Truncating overlong path");
    strncpy(out_path, in_path, path_size - 1);
    out_path[path_size] = '\0';
    return 0;
  }
}
    

/*  Add a condition to the current rule.
 *
 *  A condition is composed of one or more tests:
 *
 *			TEST		NOT TEST
 *    HAS_KEY	       :key           !:key
 *    STRING_EQ	       :key = "foo"   :key != "foo"
 *    STRSTR	       :key ^ "foo"   :key !^ "foo"
 *    EQ	       :key = 0	      :key != 0    
 *    GT	       :key > 0
 *    LT	       :key < 0
 *    GE	       :key >= 0
 *    LE	       :key <= 0
 *  combined with logical AND (&) and OR (|).
 *  followed by the operation to be performed if the condition is met:
 *    RULE: test1 & test2 | test3 --> operation
 *  If multiple servers provide the operation, the server can be specified:
 *    RULE: test1 & test2 | test3 --> server.operation
 */

/* SAM 10/8/99: Write a new utility which will support
   string splitting. This function allocates new memory. */

char *Gal_SplitOperationName(const char *op_name, char **server_name)
{
  char *new_op_name;
  
  if (op_name) {
    /* Don't surgically alter op_name. Not thread safe. */
    unsigned int i = strcspn(op_name, ".");
    if (i != strlen(op_name)) {
      new_op_name = _gal_strdup(op_name + i + 1);
      if (server_name) {	
	*server_name = (char *) calloc(i + 1, sizeof(char));
	strncpy(*server_name, op_name, i);
      }
      return new_op_name;
    } else {
      if (server_name) {
	*server_name = (char *) NULL;
      }
      return _gal_strdup(op_name);
    }
  } else {
    if (server_name) {
      *server_name = (char *) NULL;
    }
    return (char *) NULL;
  }
}

/* The pattern is [1]Parser. */

char *Gal_SplitServiceTypeName(const char *type_name, int *provider_int,
			       char **provider_name)
{
  char *buf;
  char *new_type_name;
  
  if (provider_int) {
    *provider_int = -1;
  }
  if (provider_name) {
    *provider_name = (char *) NULL;
  }

  if (!type_name)
    return NULL;

  if (type_name[0] == '[') {
    /* look for a close bracket. */
    char *end_cp = strchr(type_name, ']');
    if (end_cp && (end_cp[1] != '\0')) {
      /* We have both a bracket set and more text. */
      new_type_name = _gal_strdup(end_cp + 1);
    } else if (end_cp) {
      /* All we have is a bracket set. */
      new_type_name = (char *) NULL;
    } else {
      /* We don't have a bracket set. */
      new_type_name = _gal_strdup(type_name);
    }
    if (provider_int || provider_name) {
      /* Otherwise, do atoi(). */
      int bufsize;
      char *endptr;
      long atol_int;
      
      if (end_cp) {
	bufsize = end_cp - type_name;
      } else {
	bufsize = strlen(type_name) - 1;
      }
      buf = (char *) calloc(bufsize, sizeof(char));
      strncpy(buf, type_name + 1, bufsize - 1);
      atol_int = strtol(buf, &endptr, 10);
      if (endptr == buf) {
	/* No conversion was performed. */
	if (provider_name) {
	  *provider_name = buf;
	}
      } else {
	if (provider_int) {
	  *provider_int = (int) atol_int;
	}
	free(buf);
      }
    }
    return new_type_name;
  } else {
    return _gal_strdup(type_name);
  }
}

/* SAM 6/26/00: Writing a parser to segment complex locations. 
   host:port. The return value is
   malloc'ed. */

char *Gal_SplitLocation(const char *location_string, int *port)
{
  char *cp;
  char *new_hostname = (char *) NULL;
  int new_port = -1;
  
  if (location_string) {
    /* Always check for the colon. */
    cp = strchr(location_string, ':');
    if (cp) {
      /* If a port is to be returned, set it from the string. */
      if (port) {
	new_port = atoi(cp + 1);
      }
      new_hostname = calloc((cp - location_string) + 1, 1);
      strncpy(new_hostname, location_string, cp - location_string);
    } else {
      /* If there's no colon, just copy the string. */
      new_hostname = _gal_strdup(location_string);
    }
  }
  if (port) {
    *port = new_port;
  }
  return new_hostname;
} 

/* Memory management note: the Gal_Object must have been
   copied by the caller. */

static Gal_ProgramEntity * __gal_make_literal_entity(Gal_Object o)
{
  Gal_ProgramEntity *e = (Gal_ProgramEntity *) calloc(1, sizeof(Gal_ProgramEntity));

  e->entity_type = GAL_OBJECT_ENTITY;
  e->entity_data = (void *) o;
  e->type_available = Gal_GetObjectType(o);
  e->type_required = -1;
  return e;
}

/* $in */

/* If there's no available namespace, this one is used. */

static Gal_NamespaceEntry DefaultNamespaceTable[] = 
{
  {0, "default", 1, 1}
};

Gal_ProgramEntity *Gal_CreateNamespaceProgramEntity(const char *key,
						    Gal_ProgramParser *pp)
{
  Gal_ProgramEntity *e = (Gal_ProgramEntity *) calloc(1, sizeof(Gal_ProgramEntity));
  Gal_NamespaceProgramEntity *ne = (Gal_NamespaceProgramEntity *) calloc(1, sizeof(Gal_NamespaceProgramEntity));
  e->entity_type = GAL_NAMESPACE_ENTITY;
  /* Don't know yet. */
  e->type_available = -1;
  e->type_required = -1;

  e->entity_data = (void *) ne;
  /* Use the default namespace, whatever it is. */
  if (!pp->default_namespace)
    ne->namespace_obj = DefaultNamespaceTable;
  else
    ne->namespace_obj = pp->default_namespace;
  ne->is_default = 1;
  /* Copied, so it can be freed reliably. */
  ne->key = _gal_strdup(key);
  return e;
}

static Gal_ProgramEntity * __gal_make_namespace_entity(Gal_Object o,
						       Gal_ProgramParser *pp)
{
  char *key = (char *) NULL;
  
  if (Gal_Keywordp(o)) {
    key = Gal_KeywordValue(o);
  } else {
    if (Gal_Tagp(o) || Gal_Symbolp(o)) {
      key = Gal_KeywordValue(o);
    } else if ((Gal_Stringp(o) || Gal_Tokenp(o))) {
      key = Gal_StringValue(o);
    }
    if (!pp->extended_syntax) {
      /* Grrr. Old code prepends a colon. */
      char temp[GAL_LINE_LENGTH];
      if (pp->control_flags & GAL_PROGRAM_VERIFY)
	GalUtil_WarnWithLocation(__FUNCTION__, "Prepending colon to key %s (extended syntax not enabled)", key);
      sprintf(temp, ":%s", key);
      /* SAM 10/18/01: Since Gal_CreateNamespaceProgramEntity now
	 copies the key, we don't have to copy it here anymore. */
      key = temp;
    }
  }
  if (key)
    return Gal_CreateNamespaceProgramEntity(key, pp);
  else
    return (Gal_ProgramEntity *) NULL;
}

Gal_NamespaceEntry *Gal_FindProgramNamespace(Gal_Object o, Gal_ProgramParser *pp)
{
  char *name;
  Gal_NamespaceEntry *ne = pp->namespace_entries;

  if (!Gal_ProgramStringp(o)) {
    return (Gal_NamespaceEntry *) NULL;
  }

  name = Gal_ProgramStringValue(o);

  return Gal_FindNamespaceEntry(name, ne);
}

Gal_NamespaceEntry *Gal_FindNamespaceEntry(const char *name,
					   Gal_NamespaceEntry *ne)
{
  int i = 0;

  if (!name)
    return (Gal_NamespaceEntry *) NULL;
  
  while (ne[i].namespace_name) {
    if (!strcmp(name, ne[i].namespace_name))
      return &ne[i];
    i++;
  }
  return (Gal_NamespaceEntry *) NULL;
}
  
/* The complex entity parsers should NOT digest the final
   parenthesis, because it needs to be digested by the caller.
   Memory management note: each builder must create new
   memory for any objects it saves away. */

static Gal_ProgramEntity *__gal_at_builder(Gal_ComplexEntityDescriptor *t,
					   Gal_Object arglist,
					   Gal_ProgramParser *pp)
{
  Gal_NamespaceProgramEntity *e = (Gal_NamespaceProgramEntity *) NULL;
  int argnum = Gal_ListLength(arglist);
  Gal_ProgramEntity *pe = (Gal_ProgramEntity *) NULL;
  Gal_Object o;
  
  /* We know the symbol was $in, and we know that we found a
     list. Now what? We expect a keyword and then
     a namespace and and 
     the utterance database in the Hub requires another
     argument. */

  if (argnum < 2 || argnum > 3) {
    return NULL;
  }

  /* Keyword. */
  o = Gal_GetListObject(arglist, 0);

  if (Gal_Keywordp(o) || Gal_Tagp(o) ||
      Gal_Tokenp(o) ||
      Gal_Symbolp(o) || Gal_Stringp(o)) {
    pe = __gal_make_namespace_entity(o, pp);	  
    if (pe) {
      e = (Gal_NamespaceProgramEntity *) pe->entity_data;
    } else {
      free(pe);
      return NULL;
    }    
  } else {
    return NULL;
  }
  
  /* namespace name */
  o = Gal_GetListObject(arglist, 1);
  e->namespace_obj = Gal_FindProgramNamespace(o, pp);
  if (!e->namespace_obj) {
    free(e);
    free(pe);
    return NULL;
  } else {
    e->is_default = 0;
  }

  if (argnum == 3) {
    o = Gal_GetListObject(arglist, 2);
    e->extra_arg = Gal_CopyObject(o);
  }
  return pe;
}  

/* $preds, $nth, $name, $path, $ftype, $type */

#define INCREMENT 10

static void __gal_add_complex_program_entity_argument(Gal_ComplexProgramEntity *e,
						      Gal_ProgramEntity *arg)
{
  if (e->num_args == e->arg_buffer_size) {
    /* Realloc. */
    if (e->arg_buffer_size == 0)
      e->args = (Gal_ProgramEntity **) malloc(INCREMENT * sizeof(Gal_ProgramEntity *));
    else
      e->args = (Gal_ProgramEntity **) realloc(e->args, (e->arg_buffer_size + INCREMENT) * sizeof(Gal_ProgramEntity *));
    e->arg_buffer_size += INCREMENT;
  }
  e->args[e->num_args++] = arg;
}

Gal_ProgramEntity *
_Gal_DefaultComplexProgramEntityConstructor(Gal_ComplexEntityDescriptor *t,
					    Gal_Object arglist,
					    Gal_ProgramParser *pp)
{
  Gal_ComplexProgramEntity *ce = (Gal_ComplexProgramEntity *) calloc(1, sizeof(Gal_ComplexProgramEntity));
  Gal_ProgramEntity *pe;
  ProgramObject sub_p;
  
  ce->descriptor = t;
  ce->num_args = 0;
  ce->args = (Gal_ProgramEntity **) NULL;
  ce->arg_buffer_size = 0;

  /* Digest the list as a list of entities. */

  Gal_InstantiateProgramObjectFromList(arglist, &sub_p, 0);

  while (sub_p.index < sub_p.size) {
    Gal_ProgramEntity *e = Gal_CreateProgramEntity(&sub_p, pp, 1, 1, 1);
    if (!e)
      break;
    __gal_add_complex_program_entity_argument(ce, e);
  }

  /* If not all the argument list was chewed up, there's a problem. */
  if (sub_p.index < sub_p.size) {
    free(ce->args);
    free(ce);
    return (Gal_ProgramEntity *) NULL;
  }
  
  pe = (Gal_ProgramEntity *) calloc(1, sizeof(Gal_ProgramEntity));
  
  pe->entity_type = GAL_COMPLEX_ENTITY;
  /* Don't know yet. */
  pe->type_available = -1;
  pe->type_required = -1;
  pe->entity_data = (void *) ce;
  
  return pe;
}

static Gal_ComplexEntityDescriptor __Gal_EntitySymbolTable[] =
{
  {"$in", __gal_at_builder, NULL, NULL},
  {NULL, NULL, NULL, NULL}
};

static Gal_ProgramEntity *
__gal_perhaps_make_complex_entity(ProgramObject *p,
				  Gal_ProgramParser *pp)
{
  char *op;
  int i = 0;
  Gal_ProgramEntity *pe;
  Gal_ComplexEntityDescriptor *entry = (Gal_ComplexEntityDescriptor *) NULL;
  Gal_Object o = p->values[p->index];
    
  if (!Gal_Symbolp(o))
    return (Gal_ProgramEntity *) NULL;
  op = Gal_KeywordValue(o);
  
  if (pp->descriptor_table) {
    while (pp->descriptor_table[i].name) {
      if (!strcmp(pp->descriptor_table[i].name, op)) {
	entry = &(pp->descriptor_table[i]);
	break;
      }
      i++;
    }
  }

  if (!entry) {
    i = 0;
    while (__Gal_EntitySymbolTable[i].name) {
      if (!strcmp(__Gal_EntitySymbolTable[i].name, op)) {
	entry = &(__Gal_EntitySymbolTable[i]);
	break;
      }
      i++;
    }
  }

  if (entry) {
    if (((p->index + 1) < p->size) && 
	(Gal_GetObjectType(p->values[p->index + 1]) == GAL_LIST)) {
      pe = (*(entry->builder))(entry, p->values[p->index + 1], pp);
      if (pe) {
	p->index += 2;
	return pe;
      } else {
	return (Gal_ProgramEntity *) NULL;
      }
    } else {
      return (Gal_ProgramEntity *) NULL;
    }
  }
  return (Gal_ProgramEntity *) NULL;
}
  
/* The access function should probably return a type and data,
   so that we don't need to generate Gal_Objects for things
   when they don't have them. */

/* $and, $or, $not */

/* Evaluators. */

static Gal_Boolean __gal_evaluate_not_test(Gal_LogicalOperatorTestClause *lotc,
					   Gal_Frame *frame_array, int default_namespace)
{
  if (Gal_TestConditionInNamespaces(lotc->args[0], frame_array,
				    default_namespace))
    return GAL_FALSE;
  else return GAL_TRUE;
}

static Gal_Boolean __gal_evaluate_and_test(Gal_LogicalOperatorTestClause *lotc,
					   Gal_Frame *frame_array, int default_namespace)
{
  int i;

  for (i = 0; i < lotc->num_args; i++) {
    if (!Gal_TestConditionInNamespaces(lotc->args[i], frame_array,
				       default_namespace))
      return GAL_FALSE;
  }
  return GAL_TRUE;
}

static Gal_Boolean __gal_evaluate_or_test(Gal_LogicalOperatorTestClause *lotc,
					  Gal_Frame *frame_array, int default_namespace)
{
  int i;
  
  for (i = 0; i < lotc->num_args; i++) {
    if (Gal_TestConditionInNamespaces(lotc->args[i], frame_array,
				      default_namespace))
      return GAL_TRUE;
  }
  return GAL_FALSE;
}

static Gal_TestClause *__gal_make_logical_operator_clause(int op)
{
  Gal_TestClause *tc = (Gal_TestClause *) calloc(1, sizeof(Gal_TestClause));
  Gal_LogicalOperatorTestClause *lotc = (Gal_LogicalOperatorTestClause *) calloc(1, sizeof(Gal_LogicalOperatorTestClause));

  tc->clause_type = GAL_LOGICAL_OPERATOR_CLAUSE;
  tc->clause_data = (void *) lotc;
  lotc->logical_operator = op;
  lotc->num_args = 0;
  lotc->arg_buffer_size = 2;
  lotc->args = (Gal_TestClause **) calloc(2, sizeof(Gal_TestClause *));
  switch (op) {
  case GAL_TEST_NOT:
    lotc->op = __gal_evaluate_not_test;
    break;
  case GAL_TEST_AND:
    lotc->op = __gal_evaluate_and_test;
    break;
  case GAL_TEST_OR:
    lotc->op = __gal_evaluate_or_test;
    break;
  }
  return tc;
}

static int __gal_add_logical_clause_argument(Gal_TestClause *cur_clause,
					     Gal_TestClause *new_clause)
{
  Gal_LogicalOperatorTestClause *lotc = (Gal_LogicalOperatorTestClause *) cur_clause->clause_data;

  if (lotc->logical_operator == GAL_TEST_NOT &&
      lotc->num_args == 1) {
    return 0;
  } else {
    if (lotc->num_args == lotc->arg_buffer_size) {
      /* Realloc. */
      lotc->args = (Gal_TestClause **) realloc(lotc->args, (lotc->arg_buffer_size + INCREMENT) * sizeof(Gal_TestClause *));
      lotc->arg_buffer_size += INCREMENT;
    }
    lotc->args[lotc->num_args++] = new_clause;
    return 1;
  }    
}

/* $like, $exists, $eq, $gt, $lt, $geq, $leq */

/* Evaluators. */

extern Gal_Boolean _gal_evaluate_has_key_test(Gal_SimpleTestClause *stc,
					      Gal_Frame *frame_array, int default_namespace);
extern Gal_Boolean _gal_evaluate_string_test(Gal_SimpleTestClause *stc,
					     Gal_Frame *frame_array, int default_namespace);
extern Gal_Boolean _gal_evaluate_eq_test(Gal_SimpleTestClause *stc,
					 Gal_Frame *frame_array, int default_namespace);
extern Gal_Boolean _gal_evaluate_num_test(Gal_SimpleTestClause *stc,
					  Gal_Frame *frame_array, int default_namespace);
extern Gal_Boolean _gal_evaluate_member_test(Gal_SimpleTestClause *stc,
					     Gal_Frame *frame_array, int default_namespace);

/* Completion testers. */

static int __gal_is_1_arg_complete(Gal_SimpleTestClause *stc)
{
  return (stc->num_args == 1);
}

static int __gal_is_2_arg_complete(Gal_SimpleTestClause *stc)
{
  return (stc->num_args == 2);
}

/* Arg adders. */

static int __gal_add_has_key_arg(Gal_SimpleTestClause *stc,
				 Gal_ProgramEntity *e)
{
  if (stc->num_args == 0) {
    stc->args[0] = e;
    stc->num_args = 1;
    return 1;
  } else {
    return 0;
  }
}

static int __gal_add_string_arg(Gal_SimpleTestClause *stc,
				Gal_ProgramEntity *e)
{
  if (stc->num_args == 2) {
    return 0;
  } else {
    /* I'm not comparing it to what's available, since
       the evaluation changes depending on whether we're
       in the old syntax or the new. */
    e->type_required = GAL_STRING;
    stc->args[stc->num_args++] = e;
    return 1;
  }
}

static int __gal_add_eq_arg(Gal_SimpleTestClause *stc,
			    Gal_ProgramEntity *e)
{
  if (stc->num_args == 2) {
    return 0;
  } else {
    /* I'm not comparing it to what's available, since
       the evaluation changes depending on whether we're
       in the old syntax or the new. */
    e->type_required = -1;
    stc->args[stc->num_args++] = e;
    return 1;
  }
}

static int __gal_add_num_arg(Gal_SimpleTestClause *stc,
			     Gal_ProgramEntity *e)
{
  if (stc->num_args == 2) {
    return 0;
  } else {
    /* I'm not comparing it to what's available, since
       the evaluation changes depending on whether we're
       in the old syntax or the new. */
    e->type_required = GAL_INT;
    stc->args[stc->num_args++] = e;
    return 1;
  }
}

static int __gal_add_list_arg(Gal_SimpleTestClause *stc,
			      Gal_ProgramEntity *e)
{
  if (stc->num_args == 2) {
    return 0;
  } else if ((stc->num_args == 1) &&
	     (e->entity_type == GAL_OBJECT_ENTITY) &&
	     (e->type_available != GAL_LIST)) {
    /* The RHS needs to be a list. */
    return 0;
  } else {
    e->type_required = GAL_LIST;
    stc->args[stc->num_args++] = e;
    return 1;
  }
}

static Gal_TestClause *__gal_make_simple_test_clause(int op, Gal_ProgramParser *pp)
{
  Gal_TestClause *tc = (Gal_TestClause *) calloc(1, sizeof(Gal_TestClause));
  Gal_SimpleTestClause *stc = (Gal_SimpleTestClause *) calloc(1, sizeof(Gal_SimpleTestClause));

  tc->clause_data = (void *) stc;
  tc->clause_type = GAL_SIMPLE_CLAUSE;
  stc->pred_type = op;
  stc->extended_syntax = pp->extended_syntax;
  stc->num_args = 0;
  /* A buffer size of 2 ought to be ok to start. */
  stc->arg_buffer_size = 2;
  stc->args = (Gal_ProgramEntity **) calloc(2, sizeof(Gal_ProgramEntity *));
  switch (op) {
  case GAL_TEST_HAS_KEY:
    stc->op = _gal_evaluate_has_key_test;
    stc->complete_test = __gal_is_1_arg_complete;
    stc->arg_adder = __gal_add_has_key_arg;
    break;
  case GAL_TEST_STRING_EQ:
  case GAL_TEST_STRSTR:
    stc->op = _gal_evaluate_string_test;
    stc->complete_test = __gal_is_2_arg_complete;
    stc->arg_adder = __gal_add_string_arg;
    break;
  case GAL_TEST_EQ:
    stc->op = _gal_evaluate_eq_test;
    stc->complete_test = __gal_is_2_arg_complete;
    stc->arg_adder = __gal_add_eq_arg;
    break;
  case GAL_TEST_GT:
  case GAL_TEST_LT:
  case GAL_TEST_GE:	
  case GAL_TEST_LE:
    stc->op = _gal_evaluate_num_test;
    stc->complete_test = __gal_is_2_arg_complete;
    stc->arg_adder = __gal_add_num_arg;
    break;    
  case GAL_TEST_MEMBER:
    stc->op = _gal_evaluate_member_test;
    stc->complete_test = __gal_is_2_arg_complete;
    stc->arg_adder = __gal_add_list_arg;
    break;
  }
  return tc;
}

static int __gal_add_simple_clause_argument(Gal_TestClause *new_clause,
					    Gal_ProgramEntity *cur_entity)
{
  Gal_SimpleTestClause *stc = (Gal_SimpleTestClause *) new_clause->clause_data;

  return (*stc->arg_adder)(stc, cur_entity);
}

/* I only use completion checks on predicates now. */

static int __gal_clause_complete(Gal_TestClause *c)
{
  Gal_SimpleTestClause *stc;
  
  switch (c->clause_type) {
  case GAL_SIMPLE_CLAUSE:
    stc = (Gal_SimpleTestClause *) c->clause_data;    
    return (*stc->complete_test)(stc);
  default:
    return 0;
  }
}

char *Gal_ProgramStringValue(Gal_Object obj)
{
  /* I'll get tokens and symbols now, when I wouldn't have before. */
  if (Gal_Stringp(obj) || Gal_Tokenp(obj))
  {
    return Gal_StringValue(obj);
  }
  else if (Gal_Symbolp(obj))
  {
    /* I "undid" this translation in read_program.c, so I
       need to "redo" it here. */
    return Gal_KeywordValue(obj);
  }
  else return (char *) NULL;
}

int Gal_ProgramStringp(Gal_Object o)
{
  return Gal_Stringp(o) || Gal_Symbolp(o) || Gal_Tokenp(o);
}

Gal_ConditionStruct *Gal_NewCondition(ProgramObject *p, Gal_ProgramParser *pp,
				      int *error_ptr)
{
  Gal_ConditionStruct *condition = NULL;
  int error = GAL_NO_ERROR;

  condition = (Gal_ConditionStruct *)calloc(1, sizeof(Gal_ConditionStruct));

  if (!condition)
  {
    *error_ptr = GAL_ALLOCATION_FAILED;
    return NULL;
  }

  condition->tests = Gal_NewTest(p, &error, 0, pp);

  if (!error && (p->index < p->size)) {
    int cur_tag = Gal_GetProgramObjectTag(p, p->index);
    char *op_name = (char *) NULL;
    
    if (cur_tag != GAL_RULE_OPERATION) {
      *error_ptr = GAL_BAD_RULE_TERMINATION;
      Gal_FreeConditionStruct(condition);
      return NULL;
    } else {
      p->index++;
      if (p->index < p->size)
	op_name = Gal_ProgramStringValue(p->values[p->index++]);
      
      if (op_name) {	
	condition->operation = Gal_SplitOperationName(op_name,
						      &condition->server);
	return condition;
      }
    }
  }

  if (!error)
    error = GAL_NO_OPERATION;

  *error_ptr = error;
  Gal_FreeConditionStruct(condition);
  return NULL;
}

void Gal_FreeConditionStruct(Gal_ConditionStruct *condition)
{
  if (condition->operation) {
    free(condition->operation);
    condition->operation = NULL;
  }
  if (condition->server) {
    free(condition->server);
    condition->server = NULL;
  }

  if (condition->tests) {
    Gal_FreeTests(condition->tests);
    condition->tests = NULL;
  }

  free(condition);
}

/* SAM 11/1/00: This function should digest the sequence of
   program objects and produce a Gal_TestClause * structure. */

/* Algorithm: we have a stack of clauses. We use the logical
   connectives as separators. */

/* Don't forget, "or" does closest binding (that is, a & b | c & d
   is recognized as a & (b | c) & d) */

typedef struct __condition_parse_state {
  Gal_TestClause *cur_predicate;
  Gal_TestClause *cur_or;
  Gal_TestClause *cur_not;
  Gal_ProgramEntity *cur_entity;
  Gal_TestClause *top_clause;
} __condition_parse_state;

enum {AWAITING_CLAUSE,
      AWAITING_OPERATOR,
      AWAITING_ENTITY,
      AWAITING_CONNECTIVE};

static int __gal_flatten_current_predicate(__condition_parse_state *ps,
					   Gal_TestClause *cur_predicate)
{
  Gal_TestClause *host_clause = ps->cur_or ? ps->cur_or : ps->top_clause;
  
  if (ps->cur_not) {
    if (!__gal_add_logical_clause_argument(ps->cur_not, cur_predicate)) {
      return GAL_INVALID_TEST_VALUE;
    } else {
      cur_predicate = ps->cur_not;
      ps->cur_not = (Gal_TestClause *) NULL;
    }
  }
  if (!__gal_add_logical_clause_argument(host_clause, cur_predicate))
    return GAL_INVALID_TEST_VALUE;
  else
    return 0;
}

static int __gal_flatten_current_entity(__condition_parse_state *ps, Gal_ProgramParser *pp)
{
  Gal_TestClause *new_clause;
  int res;
  
  if (ps->cur_entity) {
    /* If there's a current entity, it's dangling
       because no one ever found it a home. Coerce
       it into an exists predicate and put it on the stack.
       But make sure that it's a namespace reference; if it's
       a literal, barf. 
    */
    if (ps->cur_entity->entity_type == GAL_OBJECT_ENTITY) {
      return GAL_INVALID_TEST_VALUE;
    }
    new_clause = __gal_make_simple_test_clause(GAL_TEST_HAS_KEY, pp);
    if (!__gal_add_simple_clause_argument(new_clause, ps->cur_entity)) {
      return GAL_INVALID_TEST_VALUE;
    }
    res = __gal_flatten_current_predicate(ps, new_clause);
    ps->cur_entity = (Gal_ProgramEntity *) NULL;
    return res;
  }
  return 0;
}

static int __gal_almost_finish_clause(__condition_parse_state *ps, Gal_ProgramParser *pp)
{
  int res;
  
  /* If there's a dangling entity, insert it. */
  res = __gal_flatten_current_entity(ps, pp);
  if (res) return res;
  if (ps->cur_predicate) {
    res = __gal_flatten_current_predicate(ps, ps->cur_predicate);
    ps->cur_predicate = (Gal_TestClause *) NULL;
    if (res) return res;
  }
  if (ps->cur_not) {
    res = __gal_flatten_current_predicate(ps, ps->cur_not);
    ps->cur_not = (Gal_TestClause *) NULL;
    return res;
  }
  return 0;
}

static int __gal_finish_clause(__condition_parse_state *ps, Gal_ProgramParser *pp)
{
  int res = __gal_almost_finish_clause(ps, pp);

  if (res) return res;
  if (ps->cur_or) {
    /* Flatten this, too. */
    if (!__gal_add_logical_clause_argument(ps->top_clause, ps->cur_or)) {
      return GAL_INVALID_TEST_VALUE;
    }
    ps->cur_or = (Gal_TestClause *) NULL;
  }
  return 0;
}

/* SAM 3/5/02: The program entities can NEVER include
   any tags. This means we can convert the argument list
   here to an array of Gal_Objects. But that won't work,
   because of the way we're advancing the prograqm object pointer.
   Argh. So we fix it. */

/* Memory management note: Gal_CreateProgramEntity must create
   new memory for anything it saves away. */ 

Gal_ProgramEntity *
Gal_CreateProgramEntity(ProgramObject *p,
			Gal_ProgramParser *pp,
			int allow_complex,
			int allow_literal,
			int allow_key)
{
  Gal_ProgramEntity *new_entity = (Gal_ProgramEntity *) NULL;

  if (p->index == p->size) {
    return (Gal_ProgramEntity *) NULL;
  }
  
  /* At this point, if I have a special entity operator,
     I should try to build a special entity. */
  
  if (pp->extended_syntax && allow_complex && allow_key)
    new_entity = __gal_perhaps_make_complex_entity(p, pp);

  if (!new_entity) {
    Gal_Object o = p->values[p->index];
    if ((Gal_Keywordp(o) || Gal_Tagp(o) ||
	 Gal_Symbolp(o))) {
      if (allow_literal && !allow_key)
	new_entity = __gal_make_literal_entity(Gal_StringObject(Gal_KeywordValue(o)));
      else
	new_entity = __gal_make_namespace_entity(o, pp);
    } else if (allow_literal) {
      new_entity = __gal_make_literal_entity(Gal_CopyObject(o));
    }
    p->index++;
  }
  return new_entity;
}

Gal_ProgramEntity *
Gal_CopyProgramEntity(Gal_ProgramEntity *old_entity)
{
  Gal_ProgramEntity *e;
  Gal_NamespaceProgramEntity *ne, *old_ne;
  Gal_ComplexProgramEntity *ce, *old_ce;
  int i;
  
  if (!old_entity)
    return (Gal_ProgramEntity *) NULL;

  e = (Gal_ProgramEntity *) calloc(1, sizeof(Gal_ProgramEntity));

  switch (old_entity->entity_type) {
  case GAL_OBJECT_ENTITY:
    e->entity_data = (void *) Gal_CopyObject((Gal_Object) old_entity->entity_data);
    break;
  case GAL_NAMESPACE_ENTITY:
    ne = (Gal_NamespaceProgramEntity *) calloc(1, sizeof(Gal_NamespaceProgramEntity));
    old_ne = (Gal_NamespaceProgramEntity *) old_entity->entity_data;
    e->entity_data = (void *) ne;
    ne->namespace_obj = old_ne->namespace_obj;
    ne->is_default = old_ne->is_default;
    /* Copied, so it can be freed reliably. */
    ne->key = _gal_strdup(old_ne->key);
    break;
  case GAL_COMPLEX_ENTITY:
    ce = (Gal_ComplexProgramEntity *) calloc(1, sizeof(Gal_ComplexProgramEntity));
    old_ce = (Gal_ComplexProgramEntity *) old_entity->entity_data;
    e->entity_data = (void *) ce;    
    ce->num_args = 0;
    ce->descriptor = old_ce->descriptor;
    ce->arg_buffer_size = 0;
    for (i = 0; i < old_ce->num_args; i++) {
      __gal_add_complex_program_entity_argument(ce, Gal_CopyProgramEntity(old_ce->args[i]));
    }
    break;
  default:
    free(e);
    return (Gal_ProgramEntity *) NULL;
  }
  e->type_available = old_entity->type_available;
  e->type_required = old_entity->type_required;
  e->entity_type = old_entity->entity_type;
  return e;  
}

int Gal_ProgramEntitiesEqual(Gal_ProgramEntity *e1, Gal_ProgramEntity *e2)
{
  Gal_NamespaceProgramEntity *ne1, *ne2;
  Gal_ComplexProgramEntity *ce1, *ce2;
  int i;
  
  if (e1 == e2)
    return 1;

  if ((!e1) || (!e2))
    return 0;
  
  if (e1->entity_type != e2->entity_type)
    return 0;

  switch (e1->entity_type) {
  case GAL_OBJECT_ENTITY:
    return Gal_ObjectEqual((Gal_Object) e1->entity_data,
			   (Gal_Object) e2->entity_data);
  case GAL_NAMESPACE_ENTITY:
    ne1 = (Gal_NamespaceProgramEntity *) e1->entity_data;
    ne2 = (Gal_NamespaceProgramEntity *) e2->entity_data;

    if (ne1->is_default != ne2->is_default)
      return 0;
    if (strcmp(ne1->key, ne2->key))
      return 0;
    if (ne1->is_default)
      /* They're the same if their names are the same. */
      return 1;
    else if (ne1->namespace_obj == ne2->namespace_obj)
      /* If non-default, the namespaces must be identical. */
      return 1;
    else
      return 0;
  case GAL_COMPLEX_ENTITY:
    ce1 = (Gal_ComplexProgramEntity *) e1->entity_data;
    ce2 = (Gal_ComplexProgramEntity *) e2->entity_data;
    if (ce1->descriptor != ce2->descriptor)
      return 0;
    if (ce1->num_args != ce2->num_args)
      return 0;
    for (i = 0; i < ce1->num_args; i++) {
      if (!Gal_ProgramEntitiesEqual(ce1->args[i], ce2->args[i]))
	return 0;
    }
    return 1;
  default:
    return 0;
  }
}

/* SAM 6/28/02: Gal_NewTest creates new memory for any
   object it saves away. */

Gal_TestClause *Gal_NewTest(ProgramObject *p, int *error_ptr,
			    int in_group, Gal_ProgramParser *pp)
{
  /* Use a conjunction for the toplevel collection. */
  __condition_parse_state ps;
  Gal_LogicalOperatorTestClause *lotc;
  Gal_TestClause *cur_clause, *new_clause;
  int error = GAL_NO_ERROR;
  int done = 0;
  int allow_key;
  int parse_state = AWAITING_CLAUSE;
  Gal_ProgramEntity *new_entity;
  Gal_TestClause *old_clause;
  int start_index = p->index;
  
  ps.top_clause = __gal_make_logical_operator_clause(GAL_TEST_AND);
  lotc = (Gal_LogicalOperatorTestClause *) ps.top_clause->clause_data;
  ps.cur_or = (Gal_TestClause *) NULL;
  ps.cur_not = (Gal_TestClause *) NULL;
  ps.cur_predicate = (Gal_TestClause *) NULL;  
  ps.cur_entity = (Gal_ProgramEntity *) NULL;
  
  while ((error == GAL_NO_ERROR) && !done && (p->index < p->size)) {
    int cur_tag = Gal_GetProgramObjectTag(p, p->index);
    Gal_Object cur_value = p->values[p->index];
    
    if (cur_tag) {
      switch(cur_tag) {
      case GAL_RULE_OPERATION:
	/* If we've reached the operation arrow, we've
	   found the end of the conditions. If we found an
	   open parenthesis, but no close parenthesis, generate
	   an error. Otherwise, see if we can
	   "roll up" the stack. */
	if (in_group) {
	  error = GAL_BAD_RULE_GROUPING;
	} else {
	  error = __gal_finish_clause(&ps, pp);
	  if (error)
	    continue;
	  parse_state = AWAITING_CONNECTIVE;
	  done = 1;
	}
	break;
      case GAL_TEST_AND:
	p->index++;
	if (((parse_state == AWAITING_OPERATOR) && ps.cur_entity) ||
	    (parse_state == AWAITING_CONNECTIVE)) {
	  /* Flatten everything, including shutting down the open "or". */
	  error = __gal_finish_clause(&ps, pp);
	  if (error)
	    continue;
	  parse_state = AWAITING_CONNECTIVE;
	}
	if (parse_state != AWAITING_CONNECTIVE) {
	  /* If we're not at the right point, barf. */
	  error = GAL_MISSING_CONSTITUENT;
	} else {
	  parse_state = AWAITING_CLAUSE;
	}
	break;
      case GAL_TEST_OR:
	p->index++;
	/* If we find an "or", we need to be careful what we
	   shut down. We want to shut down open entities,
	   predicates and nots, but not ors. */
	if (((parse_state == AWAITING_OPERATOR) && ps.cur_entity) ||
	    (parse_state == AWAITING_CONNECTIVE)) {
	  error = __gal_almost_finish_clause(&ps, pp);
	  if (error)
	    continue;
	  parse_state = AWAITING_CONNECTIVE;
	}
	if (parse_state != AWAITING_CONNECTIVE) {
	  /* If we're not at the right point, barf. */
	  error = GAL_MISSING_CONSTITUENT;
	} else {
	  /* We've got an "or", so if there's no active "or",
	     take the most recent predicate and build one. */
	  if (!ps.cur_or) {
	    ps.cur_or = __gal_make_logical_operator_clause(GAL_TEST_OR);
	    if (!__gal_add_logical_clause_argument(ps.cur_or,
						   lotc->args[lotc->num_args - 1])) {
	      error = GAL_INVALID_TEST_VALUE;
	      continue;
	    }
	    lotc->num_args--;
	    lotc->args[lotc->num_args] = (Gal_TestClause *) NULL;
	  }
	  parse_state = AWAITING_CLAUSE;
	}
	break;
      case GAL_TEST_NOT:
	p->index++;
	/* If we get a not, it can be in one of two places.
	   In either case, we put a "not" on the stack. If there's
	   anything on the top of the stack besides "and" or "or",
	   there's something very wrong. Actually, there can
	   also be an open "not". */	
	if (ps.cur_not) {
	  error = GAL_DOUBLE_NEGATIVE;
	} else {
	  ps.cur_not = __gal_make_logical_operator_clause(GAL_TEST_NOT);
	  if (parse_state == AWAITING_CLAUSE) {
	    /* We're right at the beginning. */
	    parse_state = AWAITING_ENTITY;
	  } else if (parse_state != AWAITING_OPERATOR) {
	    /* If we're not awaiting a predicate, barf. */
	    error = GAL_ALREADY_HAVE_TEST;
	  }
	}
	break;
      case GAL_TEST_HAS_KEY:
      case GAL_TEST_STRING_EQ:
      case GAL_TEST_STRSTR:
      case GAL_TEST_EQ:
      case GAL_TEST_GT:
      case GAL_TEST_LT:
      case GAL_TEST_GE:	
      case GAL_TEST_LE:
      case GAL_TEST_MEMBER:
	p->index++;
	if (parse_state != AWAITING_OPERATOR) {
	  error = GAL_NO_TEST_KEY;
	} else if (!ps.cur_entity) {
	  error = GAL_NO_TEST_KEY;
	} else {
	  /* We must have a cur_entity if we're awaiting a predicate. */
	  ps.cur_predicate = __gal_make_simple_test_clause(cur_tag, pp);
	  if (!__gal_add_simple_clause_argument(ps.cur_predicate, ps.cur_entity)) {
	    error = GAL_INVALID_TEST_VALUE;
	    continue;
	  } 
	  ps.cur_entity = (Gal_ProgramEntity *) NULL;
	  if (__gal_clause_complete(ps.cur_predicate)) {
	    /* If we're done, then we await a connective and
	       collapse the predicate. */
	    parse_state = AWAITING_CONNECTIVE;
	    error = __gal_flatten_current_predicate(&ps, ps.cur_predicate);
	    ps.cur_predicate = (Gal_TestClause *) NULL;
	  } else {
	    /* We still need more. */
	    parse_state = AWAITING_ENTITY;
	  }
	}
	break;
      default:
	p->index++;
	error = GAL_INVALID_RULE_TAG;
      }
    } else if (cur_value && (Gal_GetObjectType(cur_value) == GAL_LIST) &&
	       (parse_state == AWAITING_CLAUSE)) {
      /* We've got a sublist. The elements will already
	 be tokenized correctly; we just need to translate
	 it into a child logical operator. */
      ProgramObject sub_objects;
      Gal_InstantiateProgramObjectFromList(cur_value, &sub_objects, 1);
      
      cur_clause = Gal_NewTest(&sub_objects, &error, 1, pp);
      p->index++;
      if (sub_objects.tags)
	free(sub_objects.tags);
      parse_state = AWAITING_CONNECTIVE;
      if (error != GAL_NO_ERROR)
	continue;
      if (!cur_clause)
	error = GAL_BAD_RULE_GROUPING;
      else
	error = __gal_flatten_current_predicate(&ps, cur_clause);
    } else if (cur_value) {
      if (parse_state == AWAITING_OPERATOR) {
	/* If there's already an entity, make an equality
	   predicate. */
	new_clause = __gal_make_simple_test_clause(GAL_TEST_EQ, pp);
	if (!__gal_add_simple_clause_argument(new_clause, ps.cur_entity)) {
	  error = GAL_INVALID_TEST_VALUE;
	  break;
	}
	ps.cur_entity = (Gal_ProgramEntity *) NULL;
	ps.cur_predicate = new_clause;
	parse_state = AWAITING_ENTITY;
      } else if (parse_state == AWAITING_CLAUSE) {
	/* Launch right in. */
	parse_state = AWAITING_ENTITY;
      }
      if (parse_state != AWAITING_ENTITY) {
	/* Well, now we're just screwed. */
	error = GAL_ALREADY_HAVE_VALUE;
      } else {
	/* The interpretation of the values depends on whether the
	   extended syntax flag is set. If it is, then RHS arguments
	   of simple predicates are interpreted like LHS arguments.
	   Otherwise, they're always treated as string literals. We know
	   we're on the RHS if the topmost element is an incomplete
	   simple predicate. */
	allow_key = 1;
	if (ps.cur_predicate &&
	    (ps.cur_predicate->clause_type == GAL_SIMPLE_CLAUSE) &&
	    !__gal_clause_complete(ps.cur_predicate)) {
	  /* We're on the RHS. If extended syntax isn't set, then
	     we need to interpret as literal. */
	  if (!pp->extended_syntax) {
	    allow_key = 0;
	  }
	}

	/* The program index will be incremented appropriately in
	   Gal_CreateProgramEntity. */
	new_entity = Gal_CreateProgramEntity(p, pp, pp->extended_syntax, 1, allow_key);
	/* If there's a current clause, add it. */
	if (ps.cur_predicate) {
	  if (!__gal_add_simple_clause_argument(ps.cur_predicate, new_entity)) {
	    /* Free the entity. */
	    if (new_entity)
	      Gal_FreeProgramEntity(new_entity);
	    error = GAL_INVALID_TEST_VALUE;
	    continue;
	  }
	  if (__gal_clause_complete(ps.cur_predicate)) {
	    /* If we're done, then we await a connective and
	       collapse the predicate. */
	    parse_state = AWAITING_CONNECTIVE;
	    error = __gal_flatten_current_predicate(&ps, ps.cur_predicate);
	    ps.cur_predicate = (Gal_TestClause *) NULL;
	  } else {
	    parse_state = AWAITING_ENTITY;
	  }
	} else {
	  ps.cur_entity = new_entity;
	  parse_state = AWAITING_OPERATOR;
	}
      }
    }
  }

  if (in_group && (p->index == p->size) && (error == GAL_NO_ERROR)) {
    /* We fell off the end. That's OK inside a group. */
    error = __gal_finish_clause(&ps, pp);
  }

  if (error == GAL_NO_ERROR) {
    switch (lotc->num_args) {
    case 0:
      if (start_index == p->index) {
	/* The first thing we found was the rule arrow.
	   There are no conditions, and that's fine. */
	break;
      }
      error = GAL_BAD_RULE_TERMINATION;
      break;
    case 1:
      /* Be sure to free the top clause itself. */
      old_clause = ps.top_clause;
      ps.top_clause = lotc->args[0];
      free(lotc->args);
      free(lotc);
      free(old_clause);
      break;
    default:
      break;
    }
  }

  /* If we're in a group, it's OK if we reached the
     end of the list. */

  if (!done && (error == GAL_NO_ERROR) && !(in_group && (p->index == p->size)))
    error = GAL_BAD_RULE_TERMINATION;
  *error_ptr = error;
  

  if (done || (in_group && (p->index == p->size))) {
    return ps.top_clause;
  }

  return NULL;
}

void Gal_FreeTests(Gal_TestClause *c)
{
  Gal_SimpleTestClause* stc;
  Gal_LogicalOperatorTestClause *lotc;
  int i;
  
  switch(c->clause_type) {
  case GAL_SIMPLE_CLAUSE:
    stc = (Gal_SimpleTestClause *)c->clause_data;
    for(i=0; i<stc->num_args; i++) {
      Gal_FreeProgramEntity(stc->args[i]);
    }
    free(stc->args);
    break;
  case GAL_LOGICAL_OPERATOR_CLAUSE:
    lotc = (Gal_LogicalOperatorTestClause*) c->clause_data;
    for(i=0; i<lotc->num_args; i++) {
      Gal_FreeTests(lotc->args[i]);
    }
    free(lotc->args);
    break;

  default:
    break;
  }

  free(c->clause_data);
  c->clause_data = NULL;
  free(c);
}

void Gal_FreeProgramEntity(Gal_ProgramEntity *e)
{
  switch(e->entity_type) {
  case GAL_OBJECT_ENTITY:
    if (e->entity_data) {
      Gal_FreeObject((Gal_Object) e->entity_data);
    }
    break;
  case GAL_NAMESPACE_ENTITY:
    if (e->entity_data) {
      Gal_NamespaceProgramEntity *ne = (Gal_NamespaceProgramEntity *) e->entity_data;
      free(ne->key);
      free(ne);
    }
    break;
  case GAL_COMPLEX_ENTITY:
    if (e->entity_data) {
      Gal_ComplexProgramEntity *pe = (Gal_ComplexProgramEntity *) e->entity_data;
      int i;
      
      for(i = 0; i < pe->num_args; i++) {
	Gal_FreeProgramEntity(pe->args[i]);
      }
      free(pe->args);
      free(pe);
    }
    break;
  }
  free(e);
}
    
      

/* Tools for setting and getting. */

void Gal_SetProgramEntityLocation(Gal_ProgramEntity *e, Gal_Object o,
				  Gal_Frame *namespace_array,
				  int newly_created)
{
  Gal_NamespaceProgramEntity *ne;
  /* char *namespace_name; */
  Gal_Frame f;
  
  switch(e->entity_type) {
  case GAL_OBJECT_ENTITY:
    /* Should never be here. */
    if (newly_created)
      Gal_FreeObject(o);    
    break;
  case GAL_NAMESPACE_ENTITY:
    ne = (Gal_NamespaceProgramEntity *) e->entity_data;
    if (ne->namespace_obj->writable) {
      f = namespace_array[ne->namespace_obj->namespace_int];
      if (!newly_created)
	o = Gal_CopyObject(o);
      Gal_SetProp(f, ne->key, o);
    }
    break;
  case GAL_COMPLEX_ENTITY:
    /* Can't do it. */
    if (newly_created)
      Gal_FreeObject(o);
    break;
  }
}

void Gal_DeleteProgramEntityLocation(Gal_ProgramEntity *e, 
				     Gal_Frame *namespace_array)
{
  Gal_NamespaceProgramEntity *ne;
  /* char *namespace_name; */
  Gal_Frame f;
  
  switch(e->entity_type) {
  case GAL_OBJECT_ENTITY:
    /* Should never be here. */
    break;
  case GAL_NAMESPACE_ENTITY:
    ne = (Gal_NamespaceProgramEntity *) e->entity_data;
    if (ne->namespace_obj->writable) {
      f = namespace_array[ne->namespace_obj->namespace_int];
      Gal_DelProp(f, ne->key);
    }
    break;
  case GAL_COMPLEX_ENTITY:
    /* Can't do it. */
    break;
  }
}

Gal_Object
_Gal_DefaultComplexProgramEntityEvaluator(Gal_ComplexProgramEntity *ce,
					  Gal_Frame *namespace_array)
{
  /* First, create an array of Gal_Objects. */
  Gal_Object *args = (Gal_Object *) calloc(ce->num_args, sizeof(Gal_Object));
  int i;
  Gal_Object res;
  int *newly_created_array = (int *) calloc(ce->num_args, sizeof(int));
  
  for (i = 0; i < ce->num_args; i++) {
    args[i] = Gal_GetProgramEntity(ce->args[i], namespace_array,
				   &(newly_created_array[i]));
  }
  res = (*ce->descriptor->desc_operator)(ce->num_args, args);
  for (i = 0; i < ce->num_args; i++) {
    if (newly_created_array[i])
      Gal_FreeObject(args[i]);
  }  
  free(args);
  free(newly_created_array);
  return res;
}

/* SAM 5/25/02: I've realized that in order to deal with
   these entities appropriately, I need to know if they've
   just been created or not. */

Gal_Object Gal_GetProgramEntity(Gal_ProgramEntity *e,
				Gal_Frame *namespace_array,
				int *newly_created)
{
  Gal_NamespaceProgramEntity *ne;
  Gal_ComplexProgramEntity * ce;
  Gal_Frame f;
  
  /* This flag is really important. */
  if (!newly_created)
    return (Gal_Object) NULL;

  *newly_created = 0;
  
  switch(e->entity_type) {
  case GAL_OBJECT_ENTITY:
    return (Gal_Object) e->entity_data;
  case GAL_NAMESPACE_ENTITY:
    ne = (Gal_NamespaceProgramEntity *) e->entity_data;
    if (ne->namespace_obj->readable) {
      f = namespace_array[ne->namespace_obj->namespace_int];
      if (f) {
	return Gal_GetObject(f, ne->key);
      }
    }
    break;
  case GAL_COMPLEX_ENTITY:
    ce = (Gal_ComplexProgramEntity *) e->entity_data;
    return (*ce->descriptor->evaluator)(ce, namespace_array, newly_created);
    break;
  }
  return (Gal_Object) NULL;
}    

/*  copy_program_objects is used by Gal_ReadProgramLine to return
 *  locally allocated objects to the caller.
 */

static ProgramObject *copy_program_objects(ProgramObject *objects)
{
  ProgramObject *new_objects = (ProgramObject *) calloc(1, sizeof(ProgramObject));
  int i;

  /* Only copy what we need. */
  new_objects->size = objects->index;
  new_objects->index = 0;
  if (objects->tags) {
    new_objects->tags = (int *) calloc(new_objects->size, sizeof(int));
  } else {
    new_objects->tags = (int *) NULL;
  }
  new_objects->values = (Gal_Object *) calloc(new_objects->size, sizeof(Gal_Object));
  for (i = 0; i < new_objects->size; i++) {
    if (new_objects->tags)
      new_objects->tags[i] = objects->tags[i];
    new_objects->values[i] = objects->values[i];
  }
  return new_objects;
}

static Gal_Object program_next_object(Gal_ProgramParser *pp)
{
  Gal_Object next_object = NULL;
  int escape = 0;

  while (!(next_object = __Gal_ReadObjectLine(pp, GAL_EOL_CHAR,
					      PGM_COMMENT_CHAR, PGM_ESC_CHAR)))
  {
    switch(pp->status)
    {
    case GAL_ESCAPE:
      if (pp->fs && Gal_FileCurrentLine(pp->gs, pp->fs->last_line, GAL_LINE_LENGTH))
      {
	if (pp->control_flags & GAL_PROGRAM_DEBUG)
	  GalUtil_PInfo1("%s", pp->fs->last_line);
	pp->fs->line_count++;
      }
      escape = 1;
      continue;
    case GAL_COMMENT:
      if (escape)
      {
	if (pp->control_flags & GAL_PROGRAM_VERIFY)
	  Gal_PrintProgramWarning(pp->fs, GAL_COMMENT_AFTER_ESCAPE);
	continue;
      }
      else
	return(NULL);
    case GAL_LINE_OK: case GAL_EOL: case GAL_EOF:
      return(NULL);
    default:
      GalUtil_WarnWithLocation(__FUNCTION__, "Found unknown line status %d while reading program file", pp->status);
      return(NULL);
    }
  }
  return(next_object);
}

/*
 *  Gal_ReadProgramLine reads a logical line from a program file
 *  and parses it into its constituent tokens.
 */

static void __Gal_InitializeProgramObject(ProgramObject *p)
{
  p->values = (Gal_Object *) calloc(PROGRAM_OBJECT_INCREMENT, sizeof(Gal_Object));
  p->size = PROGRAM_OBJECT_INCREMENT;
  p->index = 0;
  p->tags = (int *) calloc(PROGRAM_OBJECT_INCREMENT, sizeof(int));
}

static void __Gal_ClearProgramObject(ProgramObject *p)
{
  int i;

  for (i = 0; i < p->size; i++) {
    p->values[i] = (Gal_Object) NULL;
    p->tags[i] = 0;
  }
  p->index = 0;
}

void Gal_InstantiateProgramObjectFromList(Gal_Object list_obj,
					  ProgramObject *p,
					  int do_tags)
{
  int i;
  
  p->values = Gal_ListValue(list_obj, &(p->size));
  p->index = 0;
  if (do_tags) {
    p->tags = (int *) calloc(p->size, sizeof(int));
    for (i = 0; i < p->size; i++) {
      if (Gal_Tagp(p->values[i])) {
	p->tags[i] = Gal_GetProgramTag(Gal_KeywordValue(p->values[i]));
      } else {
	p->tags[i] = GAL_NO_PROGRAM_TAG;
      }
    }
  } else {
    p->tags = (int *) NULL;
  }
}

int Gal_GetProgramObjectTag(ProgramObject *p, int i)
{
  if (p->tags) {
    return p->tags[i];
  } else {
    return GAL_NO_PROGRAM_TAG;
  }
}

static void __Gal_AddToProgramObject(ProgramObject *p, Gal_Object o,
				     int tag)
{
  /* First, make sure it's big enough. */
  if (p->index >= p->size) {
    p->values = (Gal_Object *) realloc((void *) p->values, (p->size + PROGRAM_OBJECT_INCREMENT) * sizeof(Gal_Object));
    if (p->tags)
      p->tags = (int *) realloc((void *) p->tags, (p->size + PROGRAM_OBJECT_INCREMENT) * sizeof(int));
    p->size += PROGRAM_OBJECT_INCREMENT;
  }
  p->values[p->index] = o;
  if (p->tags)
    p->tags[p->index] = tag;
  p->index++;
}

void Gal_InitializeProgramParser(Gal_ProgramParser *pp,
				 Gal_InputStream gs, Gal_FileStruct *fs,
				 int default_tag, int control_flags)
{
  pp->error = GAL_NO_ERROR;
  pp->gs = gs;
  pp->fs = fs;
  pp->default_tag = default_tag;
  pp->control_flags = control_flags;
  pp->extended_syntax = 0;
  pp->namespace_entries = NULL;
  pp->default_namespace = NULL;
  pp->tokenizer = NULL;
  pp->descriptor_table = (Gal_ComplexEntityDescriptor *) NULL;
  pp->extra_choices = (Gal_TokenizerChoice *) NULL;
  __Gal_InitializeProgramObject(&(pp->objects));
}

void Gal_ClearProgramParser(Gal_ProgramParser *pp)
{
  __Gal_FreeProgramObjectContents(&(pp->objects));
}

static Gal_Object __ProgramInitialTokenizer(Gal_InputStream gs, char *token);
Gal_Object _Gal_ProgramRuleTokenizer(Gal_InputStream gs, char *token);

static Gal_TokenizerChoice __Gal_TokenizerChoices[] = {
  {GAL_PGM_RULE, _Gal_ProgramRuleTokenizer},
  {GAL_NO_PROGRAM_TAG, NULL}};

static void __Gal_UpdateTokenizer(Gal_ProgramParser *pp,
				  Gal_ProgramTag program_tag)
{
  /* We look first at the extra choices in the program parser,
     and then at the local choices. */
  int i;
  
  if (pp->extra_choices) {
    for (i = 0; pp->extra_choices[i].tag != GAL_NO_PROGRAM_TAG; i++) {
      if (pp->extra_choices[i].tag == program_tag) {
	pp->tokenizer = pp->extra_choices[i].tokenizer;
	return;
      }
    }
  }

  for (i = 0; __Gal_TokenizerChoices[i].tag != GAL_NO_PROGRAM_TAG; i++) {
    if (__Gal_TokenizerChoices[i].tag == program_tag) {
      pp->tokenizer = __Gal_TokenizerChoices[i].tokenizer;
      return;
    }
  }
  pp->tokenizer = NULL;
}

ProgramObject *Gal_ReadProgramLine(Gal_ProgramParser *pp, int *count)
{
  Gal_Object next_object;
  int program_tag = 0;
  Gal_FileStruct *fs = pp->fs;

  pp->status = GAL_LINE_OK;
  pp->error = GAL_NO_ERROR;
  __Gal_ClearProgramObject(&pp->objects);

  if (fs && Gal_FileCurrentLine(pp->gs, fs->last_line, GAL_LINE_LENGTH))
  {
    if (pp->control_flags & GAL_PROGRAM_DEBUG)
      GalUtil_PInfo1("%s", fs->last_line);
    fs->line_count++;
  }

  if (pp->default_tag == GAL_PGM_RULE)
    pp->tokenizer = _Gal_ProgramRuleTokenizer;
  else
    pp->tokenizer = __ProgramInitialTokenizer;

  while (!pp->error && (next_object = program_next_object(pp))) 
  {
    if (pp->control_flags & GAL_PROGRAM_DEBUG)
       GalUtil_PInfo1("%s: %s\n",
		      Gal_ObjectTypeString(Gal_GetObjectType(next_object)),
		      Gal_ObjectToString(next_object));
    
    /* Each line should begin with a program tag. If the tag
       is not recognized, search for a colon and treat the
       remainder of the token as a program value.  If no tag
       is recognized, return an error.                       */

    if (pp->objects.index == 0 && pp->default_tag)
    {
      __Gal_AddToProgramObject(&pp->objects, (Gal_Object) NULL,
			       pp->default_tag);
      program_tag = pp->default_tag;
    }

    if (pp->objects.index == 0)
      program_tag = add_first_object(next_object, pp);
    else
      add_next_object(next_object, program_tag, pp);
    __Gal_UpdateTokenizer(pp, program_tag);
  }

  if (pp->error)
  {
    if (count)
      *count = 0;
    return(NULL);
  }

  if (pp->objects.index == 0)
  {
    switch(pp->status)
    {
    case GAL_EOL:
      __Gal_AddToProgramObject(&pp->objects, (Gal_Object) NULL,
			       GAL_PGM_BLANK_LINE);
      break;
    case GAL_COMMENT:
      __Gal_AddToProgramObject(&pp->objects, (Gal_Object) NULL,
			       GAL_PGM_COMMENT_LINE);
      break;
    case GAL_EOF:
      __Gal_AddToProgramObject(&pp->objects, (Gal_Object) NULL,
			       GAL_PGM_EOF);
      break;
    default:
      GalUtil_WarnWithLocation(__FUNCTION__, "Found unexpected line status %d wile reading program file", pp->status);
    }
  }
  if (count)
    *count = pp->objects.index;
  return(copy_program_objects(&pp->objects));
}

/* Don't free the objects themselves. */

static void __Gal_FreeProgramObjectContents(ProgramObject *p)
{
  if (p->tags)
    free(p->tags);
  if (p->values)
    free(p->values);
}

void Gal_FreeProgramLine(ProgramObject *p)
{
  __Gal_FreeProgramObjectContents(p);
  free(p);
}

/* This only works on the output of Gal_ReadProgramLine. */

void Gal_FreeProgramObject(ProgramObject *p, int starting_from)
{  
  p->index = starting_from;
  
  while (p->index < p->size) {
    Gal_FreeObject(p->values[p->index++]);
  }
  Gal_FreeProgramLine(p);
}  

/* SAM 11/1/00: This function and the next one are responsible for
   tokenizing the Gal_Objects into the array of ProgramObjects. For
   some reason, there doesn't seem to be a one-to-one correspondence
   between meaningful tokens and program objects; some operations
   are "prefixed" to their arguments, which makes the code
   REALLY hard to understand. We're going to undo this. This is a
   monster change, so bear with me. */

/* Here, we attempt to compensate for people glomming
   things together in the leading program file tag.
   The first token needs to be a program tag. */

static Gal_Object __ProgramInitialTokenizer(Gal_InputStream gs, char *token)
{
  char *cp;
  char old_char;
  
  /* If we already have a program tag, return it. */
  if (Gal_GetProgramTag(token)) {
    return Gal_TagObject(token);
  } else if ((cp = strchr(token, ':')) && cp[1]) {
    /* Otherwise, if there's an occurrence of : in
       the token, see if the stuff before it is
       a program tag. If so, rewind the appropriate distance
       and return. */
    old_char = cp[1];
    cp[1] = '\0';
    if (Gal_GetProgramTag(token)) {
      (*gs->fn_pkg->rewind_fn)(gs, 1 + strlen(cp + 2));
      return Gal_TagObject(token);
    } else {
      cp[1] = old_char;
      return NULL;
    }
  }
  return NULL;
}

static int add_first_object(Gal_Object object, Gal_ProgramParser *pp)
{
  int program_tag = GAL_NO_PROGRAM_TAG;
  pp->error = GAL_NO_ERROR;

  if (Gal_Tagp(object)) {
    program_tag = Gal_GetProgramTag(Gal_KeywordValue(object));
    __Gal_AddToProgramObject(&pp->objects, object, program_tag);
  } else {
    pp->error = GAL_INVALID_PROGRAM_TAG;  
  }
  return program_tag;
}

/* In here, we need to handle finding the rule arrow, and
   making sure that the tests are tokenized appropriately. */

Gal_Object _Gal_ProgramRuleTokenizer(Gal_InputStream gs, char *token)
{
  char *op_ptr = strstr(token, "-->");

  if (op_ptr && (op_ptr == token)) {
    /* If there's an arrow, and it's the first thing, then
       rewind the rest of it and keep going. */
    (*gs->fn_pkg->rewind_fn)(gs, strlen(token + 3));
    return Gal_TagObject("-->");
  } else if (op_ptr) {
    /* If there's an arrow, but it's not the first thing,
       then modify the token, rewind and continue. */
    (*gs->fn_pkg->rewind_fn)(gs, strlen(op_ptr));
    token[strlen(token) - strlen(op_ptr)] = '\0';
    return (Gal_Object) NULL;
  } else if ((!strncmp("&&", token, 2)) || (!strncmp("||", token, 2)) || \
	     (!strncmp("==", token, 2)) || (!strncmp(">=", token, 2)) || \
	     (!strncmp("<=", token, 2))) {
    /* If it's not an arrow, but a two-character operation,
       rewind and return tag. */
    (*gs->fn_pkg->rewind_fn)(gs, strlen(token + 2));
    token[2] = '\0';
    return Gal_TagObject(token);
  } else if (strchr("!%^|&=><", token[0])) {
    /* If there's no arrow, but it's a one-character operation,
       rewind and return tag. */
    (*gs->fn_pkg->rewind_fn)(gs, strlen(token + 1));
    token[1] = '\0';
    return Gal_TagObject(token);
  } else {
    return (Gal_Object) NULL;
  }
}

static void add_next_object(Gal_Object object, int program_tag,
			    Gal_ProgramParser *pp)     
{
  int tag = 0;
  
  pp->error = GAL_NO_ERROR;
  
  /* We'll return this line, since it's too
     hard not to, but we'll also digest it here, so
     that everyone will have the benefit of it. */
     
  if (program_tag == GAL_PGM_PGM_SYNTAX) {
    if (Gal_ProgramStringp(object)) {
      if (!strcmp(Gal_ProgramStringValue(object), "extended")) {
	pp->extended_syntax = 1;
      }
    }
  }
  if (Gal_Tagp(object)) {
    tag = Gal_GetProgramTag(Gal_KeywordValue(object));
  }
  __Gal_AddToProgramObject(&pp->objects, object, tag);
}

/* print read errors */

void Gal_PrintProgramWarning(Gal_FileStruct *fs, int error)
{
  char *tag = Gal_ErrorTagString(error);
  char temp[256];

  if (!tag)
  {
    sprintf(temp, "Unknown Tag %d", error);
    tag = temp;
  }
  GalUtil_Warn("%s (%s:%d)", tag, fs->filename, fs->line_count);
  GalUtil_Print(GAL_WARNING_LEVEL, "%s", fs->last_line);
}

void Gal_PrintProgramError(Gal_FileStruct *fs, int error)
{
  char *tag = Gal_ErrorTagString(error);
  char temp[256];

  if (!tag)
  {
    sprintf(temp, "Unknown Tag %d", error);
    tag = temp;
  }
  GalUtil_Error("%s (%s:%d)", tag, fs->filename, fs->line_count);
  GalUtil_Print(GAL_WARNING_LEVEL, "%s", fs->last_line);
}

/* print program objects (for debugging) */

void Gal_PrintProgramObject(ProgramObject *obj)
{
  int i;
  for (i = 0; i < obj->index; i++) {
    if (obj->tags && obj->tags[i]) {
      if (Gal_ProgramTagString(obj->tags[i]))
	GalUtil_Print(-1,"TAG: %s\n", Gal_ProgramTagString(obj->tags[i]));
      else
	GalUtil_Print(-1,"TAG: TAG_%d\n", obj->tags[i]);
    } else if (obj->values[i] != (Gal_Object) NULL) {
      GalUtil_Print(-1,"VALUE: ");
      Gal_PrObject(obj->values[i]);
      GalUtil_Print(-1,"\n");
    }
  }
}

/*
 *  open_control_file opens the top level program file,
 *  sets the working directory for any included files,
 *  sets the Gal_FileStruct for printing error messages,
 *  and sets up the TagObjectHash for reading special
 *  symbols.
 */

Gal_FileStruct *Gal_OpenControlFile(const char *filename,
				    char *directory)
{
  FILE *fp;
  Gal_FileStruct *fs = NULL;
  char program_directory[MAX_FNAME_LENGTH];
  char program_file[MAX_FNAME_LENGTH];

  if (directory && directory[0]) {
    /* Use this function to trim a possible trailing directory separator. */
    _Gal_SplitPath(program_directory, MAX_FNAME_LENGTH, directory);
  } else {
    _Gal_SplitPath(program_directory, MAX_FNAME_LENGTH, filename);
  }

  if (!Gal_IsFullPath(filename) && directory && directory[0])
    _Gal_CreateFullPath(program_file, MAX_FNAME_LENGTH, 2,
			directory, filename);
  else
    strcpy(program_file, filename);

  if (directory && !directory[0])
    strcpy(directory, program_directory);

  /* SAM 10/22/01: Opening in binary mode is harmless on Unix,
     but crucial on Windows, since ftell() is used by the file reader
     and doesn't work correctly on Windows in text mode when the
     file doesn't contain "proper" line terminations (i.e., only
     \n instead of \r\n, or whatever it is. */
  
  if ((fp = fopen(program_file, "rb")) == NULL)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to open program file %s", program_file);
    return(NULL);
  }
  else
  {
    Gal_HashTable temp_hash;
    
    fs = Gal_PushProgramFile(program_file, fp, NULL);    
    /* SAM 2/4/00: Gal_GetProgramTagHash() returns a copy,
       and Gal_SetTagObjectHash() overwrites whatever value
       it has. So you might end up with a memory leak. The
       only robust solution is to copy the object in
       Gal_SetTagObjectHash(), and to store the copy
       here and free it after it's passed. */
    temp_hash = Gal_GetProgramTagHash();    
    Gal_SetTagObjectHash(temp_hash);
    Gal_FreeHash(temp_hash);    
  }
  return(fs);
}

/*
 *  Gal_CloseControlFile prints the error message (if any),
 *  unsets TagObjectHash, and closes the file.
 */

void Gal_CloseControlFile(Gal_FileStruct *fs)
{
  Gal_SetTagObjectHash(NULL);

  if (fs)
  {
    Gal_PopProgramFile(fs);
  }
}


/*  Read an object from the current line.
 *  eol_char designates the end-of-line character.
 *  if comment_char is read, the reader skips to the end of the line.
 *  if esc_char is read, the reader continues on to the next line.
 */

/* SAM 7/26/02: Note that we're reading in binary mode. This
   means on Windows that we need to drop the penultimate \r.
   We need to read in binary mode in order for ftell to do
   the right thing. */

static Gal_Object __Gal_ReadObjectLine(Gal_ProgramParser *pp, int eol_char,
				       int comment_char, int esc_char)
{
  int next_char;

  pp->status = GAL_LINE_OK;

  while (isspace((next_char = (*pp->gs->fn_pkg->next_char_fn)(pp->gs))))
  {
    if (next_char == eol_char)
    {
      pp->status = GAL_EOL;
      return NULL;
    }
  }

  if (next_char == comment_char)
  {
    while (next_char > 0 && next_char != eol_char)
      next_char = (*pp->gs->fn_pkg->next_char_fn)(pp->gs);
    pp->status = GAL_COMMENT;
    return NULL;
  }

  if (next_char == esc_char)
  {
    while (next_char > 0 && next_char != eol_char)
      next_char = (*pp->gs->fn_pkg->next_char_fn)(pp->gs);
    pp->status = GAL_ESCAPE;
    return NULL;
  }

  if (next_char <= 0)
  {
    pp->status = GAL_EOF;
    return NULL;
  }

  (*pp->gs->fn_pkg->rewind_fn)(pp->gs, 1);
  return _Gal_ReadObjectWithTags(pp->gs, pp->tokenizer);
}


/* 
 *  for Emacs...
 *  Local Variables:
 *  mode: c
 *  fill-column: 110
 *  comment-column: 80
 *  c-indent-level: 2
 *  c-continued-statement-offset: 2
 *  c-brace-offset: -2
 *  c-argdecl-indent: 2
 *  c-label-offset: -2
 *  End:
 */
