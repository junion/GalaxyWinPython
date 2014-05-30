/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _PROGRAM_H
#define _PROGRAM_H

#include "galaxy/galaxy.h"
#include "galaxy/tag_enum.h"

#define PGM_COMMENT_CHAR ';'
#define PGM_ESC_CHAR '\\'

#define MAX_TOKENS 1024
#define PROGRAM_OBJECT_INCREMENT 128

/* These flags are for the program parser stuff.
   They're OR'ed together. */

#define GAL_PROGRAM_VERIFY 1
#define GAL_PROGRAM_DEBUG 2

typedef struct file_struct
{
  char filename[MAX_FNAME_LENGTH];
  FILE *fp;
  char last_line[GAL_LINE_LENGTH];
  int line_count;
  struct file_struct *included_from;
} Gal_FileStruct;

/* SAM 11/1/00: Previously, program objects
   could hold multiple elements simultaneously. This was WAY
   too hard to understand. */

/* A program object can be one of two things:
   (a) Gal_Object - a literal or key
   (b) a tag, which can be one of a number of elements,
   including syntax elements like parentheses and
   any other meaningful stuff. */

/* SAM 3/5/02: Can no longer deal with the
   annoying pointer manipulations in the program file
   parser. Changing the program object to have three
   elements: an array of tags, an array of objects, and
   a program counter. */

typedef struct __ProgramObject {
  Gal_Object *values;
  int *tags;
  int index;
  int size;
} ProgramObject;

#if 0  
typedef struct program_object
{
  Gal_Object value;
  int tag;
} ProgramObject;
#endif

typedef struct key_pair
{
  char *key;
  Gal_Object value;
  int tag;
  /* Currently, this will only be used in the
     key pair arrays in the log, but we may use it later.
     We may also do the right thing with memory management
     for the objects... */
  int newly_created;  
} KeyPair;

/* New condition organization. */

/* This is for namespace tables. */

typedef struct __gal_namespace_entry {
  int namespace_int;
  char *namespace_name;
  int readable;
  int writable;
} Gal_NamespaceEntry;

#define GAL_DEFAULT_NAMESPACE -1

enum {GAL_OBJECT_ENTITY = 1,
      GAL_NAMESPACE_ENTITY,
      GAL_COMPLEX_ENTITY};

typedef struct __gal_program_entity {
  /* Either a literal (Gal_Object), or a simple location,
     or a complex location */
  int entity_type;
  int type_required;
  int type_available;
  void *entity_data;
} Gal_ProgramEntity;

typedef struct entity_pair
{
  Gal_ProgramEntity *target;
  Gal_ProgramEntity *source;
  int tag;
} EntityPair;

typedef struct __gal_namespace_program_entity {
  Gal_NamespaceEntry *namespace_obj;
  char *key;
  int is_default;
  Gal_Object extra_arg;
} Gal_NamespaceProgramEntity;

/* Might not build a complex entity. For instance, $at builds a
   namespace entity. */

struct __gal_program_parser;
struct __gal_complex_program_entity;

typedef struct __gal_complex_entity_descriptor {
  char *name;
  Gal_ProgramEntity *(*builder)(struct __gal_complex_entity_descriptor *,
				Gal_Object,
				struct __gal_program_parser *);
  Gal_Object (*evaluator)(struct __gal_complex_program_entity *, Gal_Frame *, int *);
  Gal_Object (*desc_operator)(int, Gal_Object *);
} Gal_ComplexEntityDescriptor;

typedef struct __gal_complex_program_entity {
  Gal_ComplexEntityDescriptor *descriptor;
  int arg_buffer_size;
  int num_args;
  Gal_ProgramEntity **args;
} Gal_ComplexProgramEntity;  

enum {
  GAL_SIMPLE_CLAUSE = 1,
  GAL_LOGICAL_OPERATOR_CLAUSE,
  GAL_QUANTIFIER_CLAUSE
};

typedef struct __gal_test_clause {
  /* Either a logical operator clause or a simple clause */
  int clause_type;
  void *clause_data;
} Gal_TestClause;

typedef struct __gal_logical_operator_test_clause {
  int logical_operator;
  int num_args;
  int arg_buffer_size;
  Gal_TestClause **args;
  Gal_Boolean (*op)(struct __gal_logical_operator_test_clause *, Gal_Frame *, int);
} Gal_LogicalOperatorTestClause;

typedef struct __gal_simple_test_clause {
  int pred_type;
  int num_args;
  int extended_syntax;
  int arg_buffer_size;
  Gal_ProgramEntity **args;
  Gal_Boolean (*op)(struct __gal_simple_test_clause *, Gal_Frame *, int);
  int (*complete_test)(struct __gal_simple_test_clause *);
  int (*arg_adder)(struct __gal_simple_test_clause *, Gal_ProgramEntity *);
} Gal_SimpleTestClause;

typedef struct condition_struct
{
  Gal_TestClause *tests;
  char *operation;
  char *server;
  Gal_IntFnPtr fn_ptr;
  int valid;
} Gal_ConditionStruct;

/* line status tags for __Gal_ReadObjectLine */
typedef enum
{
  GAL_EOF = EOF,
  GAL_LINE_OK = 0,
  GAL_EOL,
  GAL_COMMENT,
  GAL_ESCAPE
} Gal_LineStatus;

typedef Gal_Object (*Gal_SpecialTokenizer)(Gal_InputStream gs, char *token);

typedef struct __gal_tokenizer_choice {
  Gal_ProgramTag tag;
  Gal_SpecialTokenizer tokenizer;
} Gal_TokenizerChoice;

typedef struct __gal_program_parser {
  Gal_FileStruct *fs;
  Gal_InputStream gs;
  int error;
  int default_tag;
  Gal_LineStatus status;
  ProgramObject objects;
  int control_flags;
  int extended_syntax;
  Gal_NamespaceEntry *namespace_entries;
  Gal_NamespaceEntry *default_namespace;
  Gal_SpecialTokenizer tokenizer;
  Gal_ComplexEntityDescriptor *descriptor_table;
  Gal_TokenizerChoice *extra_choices;
} Gal_ProgramParser;  

/* error_tags.c */
void Gal_AddErrorTags(Gal_TagMap *tmap);
char *Gal_ErrorTagString(int tag);
void Gal_FreeErrorTags();

/* program_tags.c */
void Gal_AddProgramTags(Gal_TagMap *tmap);
char *Gal_ProgramTagString(int tag);
int Gal_GetProgramTag(const char *tag_name);
Gal_HashTable Gal_GetProgramTagHash(void);
void Gal_FreeProgramTags();

/* read_program.c */
Gal_FileStruct *Gal_PushProgramFile(const char *filename, FILE *fp, Gal_FileStruct *current_files);
Gal_FileStruct *Gal_PopProgramFile(Gal_FileStruct *current_files);
int Gal_IsFullPath(const char *path);
Gal_ConditionStruct *Gal_NewCondition(ProgramObject *values, Gal_ProgramParser *pp,
				  int *error_ptr);
Gal_TestClause *Gal_NewTest(ProgramObject *values, int *error_ptr,
			    int in_group, Gal_ProgramParser *pp);
ProgramObject *Gal_ReadProgramLine(Gal_ProgramParser *pp, int *count_ptr);
void Gal_FreeProgramLine(ProgramObject *p);
void Gal_FreeProgramObject(ProgramObject *p, int starting_from);
void Gal_PrintProgramWarning(Gal_FileStruct *fs, int error);
void Gal_PrintProgramError(Gal_FileStruct *fs, int error);
void Gal_PrintProgramObject(ProgramObject *obj);
void Gal_ClearProgramParser(Gal_ProgramParser *pp);
Gal_FileStruct *Gal_OpenControlFile(const char *filename,
				    char *directory);
void Gal_CloseControlFile(Gal_FileStruct *fs);
char *Gal_SplitOperationName(const char *op_name, char **server_name);
char *Gal_SplitServiceTypeName(const char *type_name, int *provider_int,
			       char **provider_name);
char *Gal_SplitLocation(const char *location_string, int *port);
char *Gal_ProgramStringValue(Gal_Object obj);
int Gal_ProgramStringp(Gal_Object o);
int Gal_GetProgramObjectTag(ProgramObject *p, int i);
void Gal_InstantiateProgramObjectFromList(Gal_Object list_obj,
					  ProgramObject *p,
					  int do_tags);

Gal_Object Gal_GetProgramEntity(Gal_ProgramEntity *e,
				Gal_Frame *namespace_array,
				int *newly_created);
void Gal_SetProgramEntityLocation(Gal_ProgramEntity *e, Gal_Object o,
				  Gal_Frame *namespace_array,
				  int newly_created);
void Gal_DeleteProgramEntityLocation(Gal_ProgramEntity *e, 
				     Gal_Frame *namespace_array);
Gal_ProgramEntity *
Gal_CreateProgramEntity(ProgramObject *p,
			Gal_ProgramParser *pp,
			int allow_complex,
			int allow_literal,
			int allow_key);
Gal_ProgramEntity *
Gal_CopyProgramEntity(Gal_ProgramEntity *old_entity);
int Gal_ProgramEntitiesEqual(Gal_ProgramEntity *e1, Gal_ProgramEntity *e2);
void Gal_InitializeProgramParser(Gal_ProgramParser *pp,
				 Gal_InputStream gs, Gal_FileStruct *fs,
				 int default_tag, int control_flags);
Gal_NamespaceEntry *Gal_FindNamespaceEntry(const char *name,
					   Gal_NamespaceEntry *ne);
Gal_NamespaceEntry *Gal_FindProgramNamespace(Gal_Object o, Gal_ProgramParser *pp);
Gal_ProgramEntity *Gal_CreateNamespaceProgramEntity(const char *key,
						    Gal_ProgramParser *pp);

void Gal_FreeConditionStruct(Gal_ConditionStruct *cond);
void Gal_FreeTests(Gal_TestClause *c);
void Gal_FreeProgramEntity(Gal_ProgramEntity *e);

/* print_program.c */
void Gal_PrintTests(FILE *fp, Gal_TestClause *tests, int pedantic);
/* I should be able to make this a const char *, but I'd
   have to recast all sorts of things. Too lazy. */
void Gal_PrintCondition(FILE *fp, Gal_ConditionStruct *condition, char *tag, int pedantic);
char *Gal_FormatEntity(Gal_ProgramEntity *e, int pedantic, Gal_StringBuffer **bufptr);
void Gal_PrintEntity(Gal_ProgramEntity *e, int pedantic);

/* test_conditions.c */
Gal_Boolean Gal_TestCondition(Gal_Frame frame, Gal_TestClause *tests);
Gal_Boolean Gal_TestConditionInNamespaces(Gal_TestClause *tests,
					  Gal_Frame *frame_array, int default_namespace);

/* nfio.c. */

Gal_Object _Gal_ReadObjectWithTags(Gal_InputStream gs, Gal_SpecialTokenizer special_tokenizer);


#endif  /* _PROGRAM_H */
