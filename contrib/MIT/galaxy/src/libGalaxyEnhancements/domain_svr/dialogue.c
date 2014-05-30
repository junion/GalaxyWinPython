/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <galaxy/util.h>
#include <galaxy/program.h>
#include <domain_svr/domain_svr.h>

static int extract_list_from_string( char *string_in, Gal_Object **tlist_out, int *num_out );
static void merge_key_into_frame(Gal_Frame frame, char *key, char *string);

#define DCTL_DIRECTORY "../System/dctl/"

static Gal_HashTable Dialogue_Function_Hash = NULL;

/* Utilities. See dialogue_functions.h. */

static char *Server_Name = NULL;

char *Gal_GetServerName(void)
{
  return(Server_Name);
}

void Gal_SetServerName(char *name)
{
  Server_Name = name;
}

/* **************************************************************** */

void Gal_InitializeDialogueDefaults(GAL_DIALOGUE_FUNCTION_MAP *map)
{
  int i;

  Dialogue_Function_Hash = Gal_MakeHash(1000);

  if (Dialogue_Function_Hash)
  {
    for (i=0; map[i].name; i++)
      Gal_SetHash(map[i].name, Gal_PointerObject(map[i].fn), Dialogue_Function_Hash);
  }
}

Gal_IntFnPtr Gal_GetDialogueFunction(char *fn_name)
{
  Gal_Object fn_object = Gal_GetHash(fn_name, Dialogue_Function_Hash);

  if (Dialogue_Function_Hash == NULL)
    GalUtil_Warn("%s: dialogue function hash not initialized!", __FUNCTION__);

  if (fn_object)
    return (Gal_IntFnPtr)Gal_PointerValue(fn_object);
  else
    GalUtil_Warn("%s: dialogue function %s not found!", __FUNCTION__, fn_name);

  return NULL;
}

/* **************************************************************** */

/* This section is concerned with low-level access routines */

int Gal_VarIsSet(Gal_Frame dialogue_state, char *key)
{ 
  if (Gal_GetObject(dialogue_state, key)) 
    return(1);
  else
    return(0);
}

void Gal_VarSetValue(Gal_Frame dialogue_state, char *key, char *value, int int_value)
{ 
  if (value) 
    Gal_SetProp(dialogue_state, key, Gal_StringObject(value));
  else 
    Gal_SetProp(dialogue_state, key, Gal_IntObject(int_value));
}

char *Gal_VarGetValue(Gal_Frame dialogue_state, char *key)
{ 
  Gal_Object thing;
  char temp[100];

  thing = Gal_GetObject(dialogue_state, key);
  if (!thing) return(NULL);
  if (Gal_Stringp (thing)) { 
    return(Gal_StringValue(thing));
  }
  else if (Gal_Intp(thing)) { 
    sprintf(temp, "%d", Gal_IntValue(thing));
    return(strdup(temp));
  }
  return(NULL);
}

/* only applies to strings */
void Gal_VarAugmentValue(Gal_Frame dialogue_state, char *key, char *value)
{ 
  char *entry;
  char temp[1000];

  entry = Gal_VarGetValue(dialogue_state, key);
  if (entry) { 
    strcpy(temp, entry);
    strcat(temp, " ");
    strcat(temp, value);
    Gal_VarSetValue(dialogue_state, key, temp, 0); 
  }
  else 
    Gal_VarSetValue(dialogue_state, key, value, 0);
}

int Gal_VarUnsetValue(Gal_Frame dialogue_state, char *key)
{ 
  if (Gal_GetObject(dialogue_state, key))
    Gal_DelProp(dialogue_state, key);
  return(1);
}

int Gal_VarMatchValue(Gal_Frame dialogue_state, char *key, char *match_value)
{ 
  char *value;

  value = Gal_GetString(dialogue_state, key);

  return(Gal_StringEq(value, match_value));
}


/* **************************************************************** */
/* **************************************************************** */

/* This section is concerned with loading the dialogue control table */

#define ALLOC_INC 5

static void merge_key_into_frame(Gal_Frame frame, char *key, char *string)
{ 
  TObj old_value;
  int old_int, new_int = 0;
  char *old_string;
  char *new_string;

  old_value = Gal_GetObject(frame, key);

  /* If the key doesn't exist yet, set the key to the value to the new value */
  if ( !old_value ) {
    if ( Gal_DigitStringp(string) )
      Gal_SetProp(frame, key, Gal_IntObject(atoi(string)));
    else
    { if (string[0] && (string[0] == '"')) /* Likely a quoted integer */
      { if (string[strlen(string)-1] == '"')
	{ string[strlen(string)-1] = '\0';
	}
	Gal_SetProp(frame, key, Gal_StringObject(&string[1]));
      }
      else Gal_SetProp(frame, key, Gal_StringObject(string));
    }
  }
  /* If the key shows up for the second time, append the new value to the old value!! 
     arrival_time: 7:00 p m   arrival_time: which  */
  else { 
    if ( Gal_Stringp(old_value) || Gal_Intp(old_value) ) {
      if (Gal_Stringp(old_value)) {
	old_string = Gal_StringValue(old_value);
	if (!strcmp(old_string, string))
	  return;
	new_string = (char *) calloc( strlen(string) + strlen(old_string) + 2 , sizeof(char));
	sprintf(new_string,"%s %s",old_string,string);
      }
      else {
	old_int = Gal_IntValue(old_value);
	new_int = atoi(string);
	if (old_int == new_int) return;
	new_string = (char *) calloc( strlen(string) + 52 , sizeof(char));
	sprintf(new_string,"%d %s",old_int,string);
      }
      Gal_SetProp(frame, key, Gal_StringObject(new_string));
      free(new_string);
    }
    else {
      GalUtil_Warn("merge_key_into_frame: can't merge new value with existing value - skipping merge\n");
    }
  }

  return;
}


/* This converts the key value paraphrase into an eform frame structure */
void Gal_Para2Fact (Gal_Frame dc, char *paraphrase)	
{ 
  Gal_Frame frame = dc;
  Gal_Frame or_frame;
  Gal_Object list;
  Gal_Object *tlist;
  int index, num, increment;
  char *orig_string = paraphrase;
  char *temp = NULL, *string = NULL;
  int end_of_string = 0;
  unsigned int next_len = 1000;
  unsigned int key_len = 1000;
  unsigned int value_len = 1000;
  char *next, *key, *value, *token;
  int nfound = 0;
  int i;
  Gal_Object *my_tlist, *big_list;
  int new_nfound;

  if (!paraphrase) return; 
  next = (char *) calloc( next_len, sizeof(float) );
  key = (char *) calloc( key_len, sizeof(float) );
  value = (char *) calloc( value_len, sizeof(float) );

  index = 0;		/* index is current position in the paraphrase string */
  key[0] = '\0';
  value[0] = '\0';
  next[0] = '\0';

  /* Loop through the items in the paraphrase string extracting
     keys and values and placing them in the frame structure */
  while ( !end_of_string ) {

    /* Check if the end of the paraphrase string has been reached */
/*     if ( index+1 >= strlen(orig_string) ) {  THIS WAS INCORRECT!! */
  if ( index >= (int)strlen(orig_string) ) { 
      next[0] = '\0';
      string = NULL;
    }
    /* If not end of string then get the next token */
    else {
      string = strdup(&orig_string[index]);
      temp = string;
      token = strtok(temp, " \t\n");
      if (!token)
      { next[0] = '\0';
	string = NULL;
      }
      else
      { while ( (strlen(token) + 1) > next_len )
	{ next = realloc(next,(next_len+=1000)*sizeof(char));
	}
	strcpy( next, token );
	free(string);
	index += strlen(next) + 1;
	string = &orig_string[index];
      }
    }

    /* Added this for Lockheed domain */
    if ( next[0] && Gal_StringEq(next, "$CONJN$") ) {
      or_frame = Gal_MakeFrame("eform", GAL_CLAUSE);
      Gal_Para2Fact(or_frame, &next[8]);
      Gal_SetProp(frame, ":or", Gal_FrameObject(or_frame));
    }

    /* Process the next token */
    else if ( next[0] ) {

      /* Process tokens following a key */
      if ( key[0] ) {

	/* Token is the start of list marker */
      	if ( Gal_StringEq(next, "<start>") ) {
	  if ( value[0] ) {
	    GalUtil_Fatal("Gal_Para2Fact: Can't find key to associate with new `<start>' - quitting!!!\n");
	  }
	  if ( index >= (int)strlen(orig_string) ) {
	    GalUtil_Warn("Gal_Para2Fact: Last item in string is `<start>' - ignoring start!!!\n");
	  }	    
	  /* Recursively extract list from paraphrase string */
	  increment = extract_list_from_string ( string, &tlist, &num ); 

	  if((my_tlist = Gal_GetList(frame, key, &nfound))) {
	    new_nfound = nfound+num;
	    
	    big_list = (Gal_Object *) calloc (new_nfound +1, sizeof(Gal_Object));
	    for(i=0;i<nfound;i++) {
	      big_list[i] = Gal_CopyObject (my_tlist[i]);
	    }
	    for (i=0;i<num;i++) {
	      big_list[i+nfound] = Gal_CopyObject(tlist[i]);
	    }
	    list = Gal_ListObject(big_list, new_nfound);
	    /*	    Gal_FreeObject(my_tlist);
	    Gal_FreeObject(tlist);*/
	  }
	  else { list = Gal_ListObject ( tlist, num ); }
	  Gal_SetProp ( frame, key, Gal_CopyObject(list) ); 
	  Gal_FreeObject ( list );
	  key[0] = '\0';
	  index += increment;
	}

	/* Token is an illegal list marker */
	else if ( Gal_StringEq(next, "<and>") || 
		  Gal_StringEq(next, "<end>") ) {
	  GalUtil_Warn("Gal_Para2Fact: Item `%s' can't be associated with a `<start>' - ignoring!!!",next); 
	}

	/* Token is key */
	else if( next[0] == ':' || 
		 next[strlen(next)-1] == ':' ) {
	  /* If a current key value pair exists then add it to the frame */
	  if ( value[0] ) {
	    merge_key_into_frame(frame, key, value);
	  }
	  /* Else the previous key never saw a value to go with it */
	  else {
	    GalUtil_Warn("Gal_Para2Fact: No value specified for key `%s' - ignoring!!!\n",key);
	  }
	  /* Clear the current value and process the new key name */
	  value[0] = '\0';
	  while ( strlen(next) + 2 > key_len ) 
	    key = realloc( key,(key_len+=1000)*sizeof(char));
	  if ( next[strlen(next)-1] == ':' ) {
	    sprintf(key,":%s",next);
	    key[strlen(key)-1] = '\0';
	  }
	  else strcpy ( key, next );
	} 

	/* Generic token which must be appended to current value */
	else {
	  while ( strlen(value) + strlen(next) + 2 > value_len ) 
	    value = realloc( value,(value_len+=1000)*sizeof(char));
	  if ( value[0] ) strcat(value, " "); 
	  strcat(value,next);
	}

      }

      /* No current key so we're expecting to see a key next */
      else {		
	while ( strlen(next) + 2 > key_len ) 
	  key = realloc( key,(key_len+=1000)*sizeof(char));
	
	/* Get the next key */
	if ( next[strlen(next)-1] == ':' ) {
	  sprintf(key,":%s",next);
	  key[strlen(key)-1] = '\0';
	}
	else if ( next[0] == ':' ) {
	  strcpy ( key, next );
	} 		
	/* Expecting a key but didn't get one - quitting */
	else {
	  GalUtil_Warn("Gal_Para2Fact: Item `%s' can't be associated with a key - ignoring!!!",next);
	}
      }

    }

    else { 
      /* Done - no more words */	
      end_of_string = 1;
      /* Add the final key value pair to the frame if one exists */
      if ( key[0] && value[0] ) {
	merge_key_into_frame(frame, key, value);
      }
    }

  }

  free(next);
  free(key);
  free(value);

  return;

}

static int extract_list_from_string( char *string_in, Gal_Object **tlist_out, int *num_out ) 
{
  char *orig_string = string_in;
  char *string = NULL, *temp = NULL;
  char *next, *key, *value, *token; 
  Gal_Object *tlist, *sub_tlist;
  Gal_Object list;
  Gal_Frame frame;
  unsigned int next_len = 1000;
  unsigned int key_len = 1000;
  unsigned int value_len = 1000;
  int num, num_alloc, sub_num, increment;
  int end = 0;
  int index = 0;
  int end_of_string = 0;

  next = (char *) calloc( next_len, sizeof(float) );
  key = (char *) calloc( key_len, sizeof(float) );
  value = (char *) calloc( value_len, sizeof(float) );

  next[0] = '\0';
  key[0] = '\0';
  value[0] = '\0';


  /* Allocate the initial tlist */
  tlist = (Gal_Object *) calloc(num_alloc = ALLOC_INC, sizeof(Gal_Object));
  num = 0;
  frame = Gal_MakeFrame("list_item", GAL_TOPIC);

  /* Loop through the items in the paraphrase string extracting
     keys and values and placing them in the list of frames structure */
  while ( !end_of_string && !end ) 
  {
    if ( index >= (int)strlen(orig_string) ) 
    {
      next[0] = '\0';
    }
    else 
    {
      string = strdup(&orig_string[index]);
      temp = string;
      token = strtok(temp, " \t\n");
      if (!token)
      { next[0] = '\0';
      }
      else
      { while ((strlen(token) + 1) > next_len)
	 {
	   next = realloc(next,(next_len+=1000)*sizeof(char));
	 }
	 strcpy( next, token );
	 free(string);
	 index += strlen(next) + 1;
	 string = &orig_string[index];
       }
    }

    /* Process the next token */
    if ( next[0] ) 
    {
      /* See if current list item is to be closed */
      if( Gal_StringEq(next,"<and>") ||
	  Gal_StringEq(next,"<end>") )

      { if (value[0] && !key[0])
	{ tlist[num++] = Gal_StringObject(value);
	  value[0] = '\0';
	  if (Gal_StringEq(next, "<end>")) Gal_FreeFrame(frame);
	}
      else
        { if (value[0] && key[0])
	  { merge_key_into_frame(frame, key, value);
	    key[0] = '\0';
	    value[0] = '\0';
	  }
	  else if (key[0] && !value[0])  /* no value defined -- trouble here */
	  { GalUtil_Warn("Gal_Para2Fact: Saw `%s' before key `%s' was given a value - ignoring `%s'\n",
		     next, key, key);
	    key[0] = '\0';
	  }

	  tlist[num++] = Gal_FrameObject(Gal_CopyFrame(frame));
	  Gal_FreeFrame(frame);
	}
	if ( Gal_StringEq(next,"<and>")) 
	{ frame = Gal_MakeFrame("list_item", GAL_TOPIC);
	  if (num >= num_alloc)
	  { /* need to add more */
	    tlist = realloc(tlist,(num_alloc+=ALLOC_INC)*sizeof(Gal_Object));
	  }
	}
	else if (Gal_StringEq(next, "<end>")) end = 1;
      }

    /* Process tokens following a key */
      else
      {
	/* Token is the start of list marker - recurse */
      	if ( Gal_StringEq(next, "<start>") ) 
	{
	  if ((value[0]) || (!key[0]))
	  {
	    GalUtil_Fatal("Gal_Para2Fact: Can't find key to associate with new `<start>' - quitting!\n");
	  }
	  if ( index >= (int)strlen(orig_string) ) 
	  {
	    GalUtil_Warn("Gal_Para2Fact: Last item in string is `<start>' - ignoring `<start>'!\n");
	    end_of_string = 1;
	  }	    
	  increment = extract_list_from_string ( string, &sub_tlist, &sub_num );
	  list = Gal_ListObject ( sub_tlist, sub_num );
	  Gal_SetProp ( frame, key, Gal_CopyObject(list) ); 
	  Gal_FreeObject ( list );
	  key[0] = '\0';
	  index += increment;
	}
	/* Token is key */
	else if( next[0] == ':' || 
		 next[strlen(next)-1] == ':' ) 
	{
	  /* If a current key value pair exists then add it to the frame */
	  if (key[0] && value[0] )
	  {
	    merge_key_into_frame(frame, key, value);
	  }
	  /* Else the previous key never saw a value to go with it */
	  else if (key[0])
	  {
	    GalUtil_Warn("Gal_Para2Fact: No value specified for key `%s' - ignoring\n",key);
	  }
	  else if (value[0])
	  {
	    GalUtil_Warn("Gal_Para2Fact: No key specified for value `%s' - ignoring\n", value);
	  }
	  /* Clear the current value and process the new key name */
	  value[0] = '\0';
	  while ( strlen(next) + 2 > key_len ) 
	    key = realloc( key,(key_len+=1000)*sizeof(char));
	  if ( next[strlen(next)-1] == ':' ) 
	  {
	    sprintf(key,":%s",next);
	    key[strlen(key)-1] = '\0';
	  }	
	  else strcpy ( key, next);
	}
	/* Generic token which must be appended to current value */
	else 
	{
	  while ( strlen(value) + strlen(next) + 2 > value_len ) 
	    value = realloc( value,(value_len+=1000)*sizeof(char));
	  if ( value[0] ) strcat (value, " "); 
	  strcat(value, next);
	}
      }
    }
    else 
    { 
      GalUtil_Warn("Gal_Para2Fact: Reached end of list without closing `<start>' with `<end>'\n");
      end_of_string = 1;
    }
  }

  *tlist_out = tlist;
  *num_out = num;

  free(next);
  free(key);
  free(value);

  return index;

}

Gal_Frame Gal_FillDialogueState(GAL_DIALOG_CONTROL *dc, int clear)
{ 
  Gal_Frame ds;

  ds = dc->dialogue_state;
  if (clear && ds)
  { Gal_FreeFrame(ds);
    ds = NULL;
  }
  if (!ds)
  { dc->dialogue_state = (Gal_Frame) Gal_MakeFrame("eform", GAL_CLAUSE);
    ds = dc->dialogue_state;
  }
  if (clear)
  { Gal_SetProp(ds, ":num_found", Gal_IntObject(-1));
  }
  if (dc->para) {
    Gal_Para2Fact(ds, dc->para);
  }
  else GalUtil_Warn("No dialogue paraphrase given!!");
  return(ds);
}

/* dialogue control program loading and execution */

static int
execute_command(Gal_Frame dialogue_state, Gal_ConditionStruct *condition, Gal_Data data)
{
  Gal_Object conditions = NULL;

  if (!condition)
    return(DIALOGUE_STOP);
  
  if (Gal_TestCondition(dialogue_state, condition->tests) != GAL_TRUE)
    return(DIALOGUE_CONTINUE);

  conditions = Gal_CreateConditionObject(condition->tests);

  if (conditions)
    Gal_SetProp(dialogue_state, ":*conditions*", conditions);

  GalUtil_PPFrame(GAL_PINFO1_LEVEL, dialogue_state);

  GalUtil_CPInfo1(4,0,"Fn: %s\n", condition->operation);

  if (data)
    return(condition->fn_ptr(data));

  GalUtil_Warn("%s: no data being passed to %s!", __FUNCTION__, condition->operation);
  return(DIALOGUE_STOP);
}

int
Gal_DialogueLoop(GAL_DIALOG_CONTROL *dc, Gal_Data data)
{
  int i = 0;
  int state;
  Gal_ConditionStruct *condition;

  while ((i >= 0) && (i < dc->num_conditions))
  {
    condition = dc->conditions[i++];
    state = execute_command(dc->dialogue_state, condition, data);
    switch (state)
    {
    case DIALOGUE_CONTINUE:
      continue;
    case DIALOGUE_RESTART:
      i = 0;
      break;
    case DIALOGUE_STOP:
      return(DIALOGUE_STOP);
    default:
      GalUtil_Warn("%s: Unknown dialogue state %d!", __FUNCTION__, state);
      return(DIALOGUE_STOP);
    }
  }
  return(DIALOGUE_CONTINUE);
}

GAL_DIALOG_CONTROL *Gal_InstantiateSystem(char *dialogue_script_filename)
{ 
  char dialogue_filename[1000];
  GAL_DIALOG_CONTROL *dc;

  if (dialogue_script_filename) { 
    strcpy(dialogue_filename, dialogue_script_filename);
  }
  else { 
    strcpy(dialogue_filename, DCTL_DIRECTORY);
    strcat(dialogue_filename, Gal_GetServerName());
    strcat(dialogue_filename, ".dctl");
  }
  GalUtil_PInfo1("Gal_InstantiateSystem: Loading dialogue control file %s\n", dialogue_filename);
  dc = (GAL_DIALOG_CONTROL *) calloc(1, sizeof(GAL_DIALOG_CONTROL));
  dc->dialog_control_file_name = strdup(dialogue_filename);
  dc->conditions = Gal_LoadDialogueControl(dialogue_filename, &dc->num_conditions);
  if (dc->conditions == NULL)
  {
    free(dc->dialog_control_file_name);
    free(dc);
    return(NULL);
  }
  return(dc);
}

void Gal_FreeSystem(GAL_DIALOG_CONTROL *dc){
  int i;

  if (!dc)
    return;
  
  if (dc->dialog_control_file_name){
    free(dc->dialog_control_file_name);
    dc->dialog_control_file_name = NULL;
  }

  for(i=0; i<dc->num_conditions; i++){
    Gal_FreeConditionStruct(dc->conditions[i]);
    dc->conditions[i] = NULL;
  }
  free(dc->conditions);
  dc->num_conditions = 0;

  if(dc->dialogue_state){
    Gal_FreeFrame(dc->dialogue_state);
    dc->dialogue_state = NULL;
  }    

  if(dc->eform_to_semantic_file){
    free(dc->eform_to_semantic_file);
    dc->eform_to_semantic_file = NULL;
  }

  if(dc->para){
    free(dc->para);
    dc->para = NULL;
  }
   
  dc->num_conditions = 0;
  dc->conditions = NULL;
}

/* SAM 10/18/01: Moved these here from core lib in MIT distribution of
   3.1 patches. */

static Gal_Object _gal_create_entity_object(Gal_ProgramEntity *e){
  switch(e->entity_type){
  case GAL_OBJECT_ENTITY:
    return Gal_CopyObject((Gal_Object) e->entity_data);
  case GAL_NAMESPACE_ENTITY:{
    Gal_NamespaceProgramEntity *ne = (Gal_NamespaceProgramEntity*)e->entity_data;
    if (ne->is_default){
      return Gal_StringObject(ne->key);
    } else {
      Gal_Frame frame = Gal_MakeClauseFrame("$in");
      Gal_SetProp(frame, ":key", Gal_StringObject(ne->key));
      Gal_SetProp(frame, ":namespace", Gal_StringObject(ne->namespace_obj->namespace_name));
      return Gal_FrameObject(frame);
    }
  }
  default:
    return Gal_FrameObject(Gal_MakeClauseFrame("***Unkown***"));
  }
}

Gal_Object Gal_CreateConditionObject(Gal_TestClause *c){
  switch(c->clause_type){
  case GAL_SIMPLE_CLAUSE:{
    Gal_SimpleTestClause* stc = (Gal_SimpleTestClause *)c->clause_data;
    Gal_Frame frame = Gal_MakeClauseFrame(Gal_ProgramTagString(stc->pred_type));
    if (stc->pred_type == GAL_TEST_HAS_KEY){
      Gal_Object key = _gal_create_entity_object(stc->args[0]);
      Gal_SetProp(frame, ":key", key);
    } else {
      int nargs = stc->num_args;
      Gal_Object *args = (nargs > 0) ? calloc(stc->num_args-1, sizeof(Gal_Object)) : NULL;
      int i = 0;
      for(i=0; i<nargs; i++){
	args[i] = _gal_create_entity_object(stc->args[i]);
      }
      if (nargs > 0){
	Gal_SetProp(frame, ":args", Gal_ListObject(args, nargs));
	free(args);
      }
    }
    return Gal_FrameObject(frame);
  }
  
  case GAL_LOGICAL_OPERATOR_CLAUSE:{
    Gal_LogicalOperatorTestClause *lotc = (Gal_LogicalOperatorTestClause*) c->clause_data;
    Gal_Frame frame = Gal_MakeClauseFrame(Gal_ProgramTagString(lotc->logical_operator));
    int nargs = lotc->num_args;
    Gal_Object *args = (nargs > 0) ? calloc(lotc->num_args, sizeof(Gal_Object)) : NULL;
    int i;
    for(i=0; i<nargs; i++){
      args[i] = Gal_CreateConditionObject(lotc->args[i]);
    }
    if (nargs > 0){
      Gal_SetProp(frame, ":args", Gal_ListObject(args, nargs));
      free(args);
    }
    return Gal_FrameObject(frame);
  }

  default:
    return Gal_IntObject(0);
  }
}

Gal_Object Gal_LookupConditionsKey(Gal_Frame conditions, char *key_name) {
  char *my_key;
  Gal_Object *args=NULL, *subargs=NULL, *orargs=NULL, obj;
  Gal_Frame argframe=NULL;
  int nargs, nsubargs, i, norargs;


  if (!conditions) return(NULL);
  if(Gal_FrameNameEq(conditions, "&")) {
    args = Gal_GetList(conditions, ":args", &nargs);
    for(i=0;i<nargs;i++) {
      argframe = Gal_FrameValue(args[i]);
      if(Gal_FrameNameEq(argframe, "|")) {
	if((obj = Gal_LookupConditionsKey(argframe, key_name)))
	  return(obj);
      }
    }
  } else if (Gal_FrameNameEq(conditions, "|")) {
    orargs = Gal_GetList(conditions, ":args", &norargs);
    for(i=0;i<norargs;i++) {
      argframe = Gal_FrameValue(orargs[i]);
      if(Gal_FrameNameEq(argframe, "=")) {
	subargs = Gal_GetList(Gal_FrameValue(orargs[i]), ":args", &nsubargs);
	if(subargs) {
	  my_key = Gal_StringValue(subargs[0]);
	  if(my_key && (!strcmp(my_key, key_name))) {
	    return(Gal_CopyObject(subargs[1]));
	  }
	}
      }
    }
  }
  return(NULL);
}

/* 
 *  for Emacs...
 *  Local Variables:
 *  mode: c
 *  fill-column: 110
 *  comment-column: 80
 *  c-tab-always-indent: nil
 *  c-indent-level: 2
 *  c-continued-statement-offset: 2
 *  c-brace-offset: -2
 *  c-argdecl-indent: 2
 *  c-label-offset: -2
 *  End:
 */
