/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* SAM 7/18/01: The idea here is to write code which interprets the 
   template files in such a way that we can create dummy
   "engines" which we can provide sample, realistic wrappers
   for. I've decided to abandon the original way of implementing
   the canned dialogues because I had originally developed
   the toy travel demo only to illustrate the program files,
   but it's much more useful pedagogically to illustrate both
   the program files and server implementations simultaneously. */

#include "galaxy/sysdep.h"
#include <stdio.h>
#include "component_engine.h"

static Exchange *__CE_NewExchange(Gal_Object exchange_list);
static void __CE_FreeIOSequence(Exchange **io_sequence);
static void __CE_FreeExchange(Exchange *exchange);
static AudioDescriptor *__CE_DigestAudioDescriptor(Gal_Frame step_frame);
static ParseTree *__CE_DigestParseTree(Gal_Frame step_frame);
static DBResult *__CE_DigestDBResult(Gal_Frame step_frame,
				    char *col_name_key,
				    char *nfound_key,
				    char *tuples_key);
static char *__CE_DigestString(Gal_Frame step_frame, char *key);
static void __CE_FreeDBResult(DBResult *r);

/* 
 *
 *    READING THE EXCHANGE FILE 
 *
 */

/* Each file consists of a single list, each of whose elements
   is a list. The elements of each sublist are frames, whose
   name corresponds to the name of the step, and whose keys
   and values populate that step of the exchange. */

static Exchange **IOSequence = (Exchange **) NULL;

void CE_InitializeIOSequence(char *filename)
{
  IOSequence = CE_ReadIOSequence(filename);
  if (!IOSequence) {
    GalUtil_Fatal("No exchange sequence. Exiting.");
  }
}

Exchange **CE_ReadIOSequence(char *filename)
{
  FILE *fp;
  Gal_Object temp;
  Exchange **io_sequence;
  int i, j;

  /* SAM 10/22/01: Opening in binary mode is harmless on Unix,
     but crucial on Windows, since ftell() is used by the file reader
     and doesn't work correctly on Windows in text mode when the
     file doesn't contain "proper" line terminations (i.e., only
     \n instead of \r\n, or whatever it is. */

  if (!(fp = fopen(filename, "rb"))) {
    fprintf(stderr, "Can't open %s; failing.\n", filename);
    fflush(stderr);
    return (Exchange **) NULL;
  }

  temp = Gal_ReadObjectFromFile(fp);
  fclose(fp);
  /* This should be a single list. */
  if (!Gal_Listp(temp)) {
    Gal_FreeObject(temp);
    fprintf(stderr, "Object in file is not list; failing.\n");
    fflush(stderr);
    return (Exchange **) NULL;
  }
  i = Gal_ListLength(temp);
  /* Final NULL for termination. */
  io_sequence = (Exchange **) calloc(i + 1, sizeof(Exchange *));
  for (j = 0; j < i; j++) {
    Gal_Object element = Gal_GetListObject(temp, j);
    Exchange *new_exchange;
    
    if (!Gal_Listp(element)) {
      __CE_FreeIOSequence(io_sequence);
      Gal_FreeObject(temp);
      fprintf(stderr, "Object in list is not list; failing.\n");
      fflush(stderr);
      return (Exchange **) NULL;
    }

    new_exchange = __CE_NewExchange(element);
    if (!new_exchange) {
      __CE_FreeIOSequence(io_sequence);
      Gal_FreeObject(temp);
      fprintf(stderr, "Couldn't translate exchange %d; failing.\n", j);
      fflush(stderr);
      return (Exchange **) NULL;
    }
    io_sequence[j] = new_exchange;
  }
  return io_sequence;
}

static Exchange *__CE_NewExchange(Gal_Object exchange_list)
{
  int i = Gal_ListLength(exchange_list);
  int j;
  Exchange *exchange = (Exchange *) calloc(1, sizeof(Exchange));
  Gal_Frame step_frame;
  char *frame_name;
  int status = 0;
  char *step = "";
  
  for (j = 0; j < i; j++) {
    Gal_Object exchange_step = Gal_GetListObject(exchange_list, j);
    if (!Gal_Framep(exchange_step)) {
      __CE_FreeExchange(exchange);
      fprintf(stderr, "Exchange step is not a frame.\n");
      fflush(stderr);
      return (Exchange *) NULL;
    }
    /* OK, we know the list element is a frame. Now, we need
       to digest the frame depending on its name. */
    step_frame = Gal_FrameValue(exchange_step);
    frame_name = Gal_FrameName(step_frame);
    if (!strcmp(frame_name, "audio_input")) {
      /* Keys are :sample_rate, :encoding_format, :num_samples */
      exchange->audio_input = __CE_DigestAudioDescriptor(step_frame);
      step = "audio input";
      status = exchange->audio_input ? 1 : 0;
    } else if (!strcmp(frame_name, "text_input")) {
      /* Keys are :input_string */
      exchange->text_input = __CE_DigestString(step_frame, ":input_string");
      step = "text input";
      status = exchange->text_input ? 1 : 0;
    } else if (!strcmp(frame_name, "recognizer_output")) {
      exchange->recognizer_output = __CE_DigestString(step_frame,
						      ":input_string");
      step = "recognizer output";
      status = exchange->recognizer_output ? 1 : 0;
    } else if (!strcmp(frame_name, "parser_output")) {
      /* Keys are :frame */
      exchange->parser_output = __CE_DigestParseTree(step_frame);
      step = "parser output";
      status = exchange->parser_output ? 1 : 0;
    } else if (!strcmp(frame_name, "backend_query")) {
      /* Keys are :sql_query */
      exchange->sql_query = __CE_DigestString(step_frame, ":sql_query");
      step = "backend query";
      status = exchange->sql_query ? 1 : 0;
    } else if (!strcmp(frame_name, "backend_output")) {
      /* Keys are :column_names, :nfound, :values */
      exchange->backend_output = __CE_DigestDBResult(step_frame,
						     ":column_names",
						     ":nfound",
						     ":values");
      step = "backend output";
      status = exchange->backend_output ? 1 : 0;
    } else if (!strcmp(frame_name, "dialogue_output")) {
      /* Keys are :frame */
      exchange->dialogue_output = __CE_DigestParseTree(step_frame);
      step = "dialogue output";
      status = exchange->dialogue_output ? 1 : 0;
    } else if (!strcmp(frame_name, "generator_output")) {
      /* Keys are :output_string */
      exchange->generator_output = __CE_DigestString(step_frame,
						     ":output_string");
      step = "generator output";
      status = exchange->generator_output ? 1 : 0;
    } else if (!strcmp(frame_name, "synthesizer_output")) {
      /* Keys are :sample_rate, :encoding_format, :num_samples */      
      exchange->synthesizer_output = __CE_DigestAudioDescriptor(step_frame);
      step = "synthesizer output";
      status = exchange->synthesizer_output ? 1 : 0;
    } else {
      __CE_FreeExchange(exchange);
      fprintf(stderr, "Exchange name %s unknown.\n", frame_name);
      fflush(stderr);
      return (Exchange *) NULL;
    }
    if (!status) {
      __CE_FreeExchange(exchange);
      fprintf(stderr, "Ill-formed %s exchange step.\n", step);
      fflush(stderr);
      return (Exchange *) NULL;
    }
  }
  return exchange;
}

static void __CE_FreeIOSequence(Exchange **io_sequence)
{
  int i = 0;
  while (io_sequence[i]) {
    __CE_FreeExchange(io_sequence[i]);
  }
  free(io_sequence);
}

static void __CE_FreeDBResult(DBResult *r)
{
  int i, j;

  if (r->column_names) {    
    for (i = 0; r->column_names[i]; i++) {
      free(r->column_names[i]);
    }
  }

  free(r->column_names);
  if (r->values) {
    for (i = 0; r->values[i]; i++) {
      for (j = 0; r->values[i][j]; j++) {
	free(r->values[i][j]);
      }
      free(r->values[i]);
    }
    free(r->values);
  }
  free(r);
}

static void __CE_FreeExchange(Exchange *exchange)
{
  if (exchange->audio_input)
    free(exchange->audio_input);
  if (exchange->text_input)
    free(exchange->text_input);
  if (exchange->recognizer_output)
    free(exchange->recognizer_output);
  if (exchange->parser_output)
    free(exchange->parser_output);
  if (exchange->sql_query)
    free(exchange->sql_query);
  if (exchange->backend_output)
    __CE_FreeDBResult(exchange->backend_output);
  if (exchange->dialogue_output)
    free(exchange->dialogue_output);
  if (exchange->generator_output)
    free(exchange->generator_output);
  if (exchange->synthesizer_output)
    free(exchange->synthesizer_output);
  free(exchange);
}

static AudioDescriptor *__CE_DigestAudioDescriptor(Gal_Frame step_frame)
{
  /* Keys are :sample_rate, :encoding_format, :num_samples */
  Gal_Object sample_rate = Gal_GetObject(step_frame, ":sample_rate");
  char *encoding_format = Gal_GetString(step_frame, ":encoding_format");
  Gal_Object num_samples = Gal_GetObject(step_frame, ":num_samples");

  if ((!sample_rate) || (!Gal_Intp(sample_rate)) ||
      (!encoding_format) ||
      (!num_samples) || (!Gal_Intp(num_samples))) {
    return (AudioDescriptor *) NULL;
  } else {
    AudioDescriptor *d = (AudioDescriptor *) calloc(1, sizeof(AudioDescriptor));
    
    d->sample_rate = Gal_IntValue(sample_rate);
    if (encoding_format)
      d->encoding_format = _gal_strdup(encoding_format);
    d->num_samples = Gal_IntValue(num_samples);
    return d;
  }
}

static ParseTree *__CE_DigestParseTree(Gal_Frame step_frame)
{
  /* Keys are :frame */
  Gal_Frame parse = Gal_GetFrame(step_frame, ":frame");

  if (!parse) {
    return (ParseTree *) NULL;
  } else {
    ParseTree *t = (ParseTree *) calloc(1, sizeof(ParseTree));

    t->tree = Gal_CopyFrame(parse);
    return t;
  }    
}

static DBResult *__CE_DigestDBResult(Gal_Frame step_frame,
				     char *col_name_key,
				     char *nfound_key,
				     char *tuples_key)
{
  /* Keys are :column_names, :nfound, :values */
  int col_len, val_len;
  int i, j;
  DBResult *r;
  
  Gal_Object *column_names = (Gal_Object *) NULL;
  Gal_Object nfound = (Gal_Object) NULL;
  Gal_Object *values = (Gal_Object *) NULL;

  if (col_name_key)
    column_names = Gal_GetList(step_frame, col_name_key, &col_len);
  if (nfound_key)
    nfound = Gal_GetObject(step_frame, nfound_key);
  if (tuples_key)
    values = Gal_GetList(step_frame, tuples_key, &val_len);

  if ((!column_names) || (!Gal_Intp(nfound)) || (!values)) {
    return (DBResult *) NULL;
  }

  r = (DBResult *) calloc(1, sizeof(DBResult));
  r->column_names = (char **) calloc(col_len + 1, sizeof(char *));
  for (i = 0; i < col_len; i++) {
    if (!Gal_Stringp(column_names[i])) {
      __CE_FreeDBResult(r);
      return (DBResult *) NULL;
    }
    r->column_names[i] = _gal_strdup(Gal_StringValue(column_names[i]));
  }
  
  if (nfound) {
    r->nfound = Gal_IntValue(nfound);
  } else {
    r->nfound = val_len;
  }
  
  r->values = (char ***) calloc(val_len + 1, sizeof(char **));
  for (i = 0; i < val_len; i++) {
    int entry_len;
    Gal_Object *vals = Gal_ListValue(values[i], &entry_len);

    /* I should test to see if the entry lens are the same
       as the column name length. */
    if ((!vals) || (col_len != entry_len)) {
      __CE_FreeDBResult(r);
      return (DBResult *) NULL;
    }
    
    r->values[i] = (char **) calloc(entry_len + 1, sizeof(char *));
    for (j = 0; j < entry_len; j++) {
      if (!Gal_Stringp(vals[j])) {
	__CE_FreeDBResult(r);
	return (DBResult *) NULL;
      }
      r->values[i][j] = _gal_strdup(Gal_StringValue(vals[j]));      
    }
  }
  return r;				       
}

static char *__CE_DigestString(Gal_Frame step_frame, char *key)
{
  char *s = Gal_GetString(step_frame, key);
  if (s) {
    return _gal_strdup(s);
  } else {
    return (char *) NULL;
  }
}

/* 
 *
 *    INDIVIDUAL ENGINES 
 *
 */

/*
 * Parsing
 */

void InitializeParser(char *filename)
{
  CE_InitializeIOSequence(filename);
}

ParseTree *ParseSentence(char *input_string)
{
  int i;

  if (!input_string) {
    return (ParseTree *) NULL;
  }
  
  for (i = 0; IOSequence[i]; i++) {
    /* If the input string is equal to the text input
       or the recognizer output, and if there's a
       parse output, return it. */
    if ((((IOSequence[i]->recognizer_output) &&
	  !_gal_strcasecmp(input_string, IOSequence[i]->recognizer_output)) ||
	 ((IOSequence[i]->text_input) &&
	  !_gal_strcasecmp(input_string, IOSequence[i]->text_input))) &&
	IOSequence[i]->parser_output) {
      return IOSequence[i]->parser_output;
    }
  }
  return (ParseTree *) NULL;
}

Gal_Frame ParseTreeToFrame(ParseTree *p)
{
  if (p->tree) {
    return Gal_CopyFrame(p->tree);
  } else {
    return (Gal_Frame) NULL;
  }
}

/*
 * Generation
 */

void InitializeGenerator(char *filename)
{
  CE_InitializeIOSequence(filename);
}

char *GenerateSentenceFromTree(ParseTree *output_tree)
{
  int i;

  if (!output_tree) {
    return (char *) NULL;
  }
  
  for (i = 0; IOSequence[i]; i++) {
    /* If the input string is equal to the text input
       or the recognizer output, and if there's a
       parse output, return it. */
    if ((IOSequence[i]->dialogue_output) &&
	(Gal_FrameEqual(IOSequence[i]->dialogue_output->tree,
			output_tree->tree)) &&
	(IOSequence[i]->generator_output)) {
      return IOSequence[i]->generator_output;
    }
  }
  return (char *) NULL;
}

static int __CE_DBResultsMatch(DBResult *db1, DBResult *db2)
{
  int i, j;
  
  if (db1->nfound != db2->nfound)
    return 0;

  for (i = 0; db1->column_names[i] || db2->column_names[i]; i++) {
    if (!(db1->column_names[i] && db2->column_names[i])) {
      return 0;
    }
    if (strcmp(db1->column_names[i], db2->column_names[i])) {
      return 0;
    }
  }

  for (i = 0; db1->values[i] || db2->values[i]; i++) {
    if (!(db1->values[i] && db2->values[i])) {
      return 0;
    }
    for (j = 0; db1->values[i][j] || db2->values[i][j]; j++) {
      if (!(db1->values[i][j] && db2->values[i][j])) {
	return 0;
      }
      if (strcmp(db1->values[i][j], db2->values[i][j])) {
	return 0;
      }
    }
  }
  return 1;
}

char *GenerateSentenceFromDBResult(DBResult *db)
{
  int i;

  if (!db) {
    return (char *) NULL;
  }
  
  for (i = 0; IOSequence[i]; i++) {
    /* If the input string is equal to the text input
       or the recognizer output, and if there's a
       parse output, return it. */
    if ((IOSequence[i]->backend_output) &&
	__CE_DBResultsMatch(db, IOSequence[i]->backend_output) &&
	(IOSequence[i]->generator_output)) {
      return IOSequence[i]->generator_output;
    }
  }
  return (char *) NULL;
}

ParseTree *FrameToParseTree(Gal_Frame frame)
{
  ParseTree *p = (ParseTree *) calloc(1, sizeof(ParseTree));

  p->tree = frame;
  return p;
}

void FreeParseTree(ParseTree *p)
{
  Gal_FreeFrame(p->tree);
  free(p);
}

static ParseTree *__CopyParseTree(ParseTree *p)
{
  Gal_Frame f = Gal_CopyFrame(p->tree);
  return FrameToParseTree(f);
}

DBResult *FrameToDBResult(Gal_Frame frame)
{
  return __CE_DigestDBResult(frame, ":column_names",
			     (char *) NULL, ":tuples");
}

/*
 * Backend
 */


void InitializeBackend(char *filename)
{
  CE_InitializeIOSequence(filename);
}

DBResult *RetrieveDBResult(char *sql_query)
{
  int i;

  if (!sql_query) {
    return (DBResult *) NULL;
  }
  
  for (i = 0; IOSequence[i]; i++) {
    /* If the input string is equal to the text input
       or the recognizer output, and if there's a
       parse output, return it. */
    if ((IOSequence[i]->sql_query) &&
	(IOSequence[i]->backend_output) &&
	!strcmp(sql_query, IOSequence[i]->sql_query)) {
      return IOSequence[i]->backend_output;
    }
  }
  return (DBResult *) NULL;
}

static Gal_Frame __CE_DBResultToFrame(DBResult *db,
				      char *frame_name,
				      char *col_name_key,
				      char *nfound_key,
				      char *values_key)
{
  Gal_Frame reply = Gal_MakeFrame(frame_name, GAL_CLAUSE);
  Gal_Object column_names;
  Gal_Object values;
  int i, j;
  
  column_names = Gal_CreateListObject((Gal_Object *) NULL, 0,
				      _gal_free_object, 1);
  for (i = 0; db->column_names[i]; i++) {
    Gal_ListObjectAdd(column_names, Gal_StringObject(db->column_names[i]));
  }

  values = Gal_CreateListObject((Gal_Object *) NULL, 0,
				_gal_free_object, 1);
  for (i = 0; db->values[i]; i++) {
    Gal_Object row = Gal_CreateListObject((Gal_Object *) NULL, 0,
					  _gal_free_object, 1);
    for (j = 0; db->values[i][j]; j++) {
      Gal_ListObjectAdd(row, Gal_StringObject(db->values[i][j]));
    }
    Gal_ListObjectAdd(values, row);
  }    

  if (nfound_key)
    Gal_SetProp(reply, nfound_key, Gal_IntObject(db->nfound));
  Gal_SetProp(reply, col_name_key, column_names);
  Gal_SetProp(reply, values_key, values);
  return reply;
}


Gal_Frame DBResultToFrame(DBResult *db)
{
  return __CE_DBResultToFrame(db, "reply",
			      ":column_names", ":nfound", ":values");
}

/*
 * Dialogue
 */

void InitializeDialogue(char *filename)
{
  CE_InitializeIOSequence(filename);
}

char *DialogueStepName(int step_type)
{
  switch (step_type) {
  case USER_INPUT:
    return "USER_INPUT";
  case CONSULT_BACKEND:
    return "CONSULT_BACKEND";
  case BACKEND_RESPONSE:
    return "BACKEND_RESPONSE";
  case PRESENT_OUTPUT:
    return "PRESENT_OUTPUT";
  default:
    return "<unknown>";
  }
}

DialogueAction *NextDialogueStep(void *step_data, int step_type)
{
  int i;
  DialogueAction *a;
  ParseTree *p;
  DBResult *db;
  
  /* Right now, there will be only two steps. If the step is
     USER_INPUT, find the next dialogue action, whether it
     be PRESENT_OUTPUT or CONSULT_BACKEND. If the step is
     BACKEND_RESPONSE, only look for DB result. */
  switch (step_type) {
  case USER_INPUT:
    p = (ParseTree *) step_data;
    for (i = 0; IOSequence[i]; i++) {
      /* If the input string is equal to the text input
	 or the recognizer output, and if there's a
	 parse output, return it. */
      if ((IOSequence[i]->parser_output) &&
	  Gal_FrameEqual(p->tree, IOSequence[i]->parser_output->tree)) {
	if (IOSequence[i]->sql_query) {
	  a = (DialogueAction *) calloc(1, sizeof(DialogueAction));
	  a->step_type = CONSULT_BACKEND;
	  a->step_data = (void *) _gal_strdup(IOSequence[i]->sql_query);
	  return a;
	} else if (IOSequence[i]->dialogue_output) {
	  a = (DialogueAction *) calloc(1, sizeof(DialogueAction));
	  a->step_type = PRESENT_OUTPUT;
	  a->step_data = (void *) __CopyParseTree(IOSequence[i]->dialogue_output);
	  return a;
	}
      }
    }
    return (DialogueAction *) NULL;
    break;
  case BACKEND_RESPONSE:
    db = (DBResult *) step_data;
    for (i = 0; IOSequence[i]; i++) {
      /* If the DB response is equal to an existing DB response,
	 turn it into a frame and return it. */
      if ((IOSequence[i]->backend_output) &&
	  __CE_DBResultsMatch(db, IOSequence[i]->backend_output)) {
	Gal_Frame f_response = __CE_DBResultToFrame(db, "db_result",
						    ":column_names",
						    (char *) NULL,
						    ":tuples");
	
	a = (DialogueAction *) calloc(1, sizeof(DialogueAction));
	a->step_type = PRESENT_OUTPUT;
	a->step_data = (void *) FrameToParseTree(f_response);
	return a;
      }
    }
    return (DialogueAction *) NULL;
    break;
  default:
    return (DialogueAction *) NULL;
  }
}

DBResult *HubFrameToDBResult(Gal_Frame f)
{
  return __CE_DigestDBResult(f, ":column_names",
			     ":nfound", ":values");
}

void FreeDBResult(DBResult *db)
{
  __CE_FreeDBResult(db);
}

void FreeDialogueAction(DialogueAction *a)
{
  free(a);
}

/*
 * Recognizer
 */

void InitializeRecognizer(char *filename)
{
  CE_InitializeIOSequence(filename);
}

void IncrementalRecognize(RecognizerRecord *rec, void *data, int n_samples)
{
  if (rec)
    rec->num_samples += n_samples;
}

RecognizerRecord *InitializeRecognition(char *encoding_format, int sample_rate)
{
  RecognizerRecord *rec = (RecognizerRecord *) calloc(1, sizeof(RecognizerRecord));

  rec->num_samples = 0;
  rec->sample_rate = sample_rate;
  if (encoding_format)
    rec->encoding_format = _gal_strdup(encoding_format);
  return rec;
}

void FreeRecognizerRecord(RecognizerRecord *rec)
{
  if (rec->encoding_format)
    free(rec->encoding_format);
  free(rec);
}

char *FinishRecognition(RecognizerRecord *rec)
{
  int i;

  if (!rec) {
    return (char *) NULL;
  }
  
  for (i = 0; IOSequence[i]; i++) {
    /* If the input string is equal to the text input
       or the recognizer output, and if there's a
       parse output, return it. */
    if ((IOSequence[i]->audio_input) &&
	(rec->num_samples == IOSequence[i]->audio_input->num_samples) &&
	(rec->sample_rate == IOSequence[i]->audio_input->sample_rate) &&
	(!strcmp(rec->encoding_format,
		 IOSequence[i]->audio_input->encoding_format)) &&
	IOSequence[i]->recognizer_output) {
      return IOSequence[i]->recognizer_output;
    }
  }
  return (char *) NULL;
}

/*
 * Audio
 */

/* The audio "device" should be keeping track of where
   we are in the input sequence, and how much audio we've
   processed from the "device" for each element. The callback
   is responsible for sending the messages, establishing
   the brokers, etc. */

char *AudioDeviceStateName(int audio_state)
{
  switch(audio_state) {
  case AUDIO_IDLE:
    return "AUDIO_IDLE";
  case AUDIO_RECORDING:
    return "AUDIO_RECORDING";
  case AUDIO_PLAYING:
    return "AUDIO_PLAYING";
  case AUDIO_UNAVAILABLE:
    return "AUDIO_UNAVAILABLE";
  default:
    return "<unknown>";
  }
}

static void __AudioSendTask(Gal_TaskPkg *pkg)
{
  AudioDevice *a = (AudioDevice *) Gal_TaskPkgData(pkg);
  int previous_device_state = a->current_device_state;
  int current_device_state = AUDIO_RECORDING;
  char *data_buf;
  int total_samples_to_read = IOSequence[a->input_id]->audio_input->num_samples;
  int samples_left = total_samples_to_read - a->samples_read;
  int this_time, bytes_this_time, i;
  
  /* Every 100 ms, this will fire until we run out
     of data to send. We'll send in 1024 sample chunks. */
  if (samples_left <= 1024) {
    this_time = samples_left;
    current_device_state = AUDIO_IDLE;
    a->samples_read += samples_left;
  } else {
    this_time = 1024;
    a->samples_read += 1024;
  }
  bytes_this_time = this_time * 2;
  
  data_buf = (char *) calloc(bytes_this_time, sizeof(char));
  for (i = 0; i < bytes_this_time; i++) {
    data_buf[i] = (char) _gal_random() & 256;
  }
  a->current_device_state = current_device_state;
  a->previous_device_state = previous_device_state;
  (*a->callback_fn)(a, data_buf, this_time);
  if (current_device_state == AUDIO_RECORDING) {
    /* If we're not done, re-add the task. */
    Gal_ReAddTask(pkg, (void *) a, 100, 0, NULL);    
  } else {
    a->input_id++;
  }
}

static void __AudioCallback(AudioDevice *a)
{
  /* So if the audio state is AUDIO_IDLE, we send the next element. */
  if (a->current_device_state == AUDIO_IDLE) {
    /* Find the current exchange with audio input. */
    while (IOSequence[a->input_id] &&
	   !IOSequence[a->input_id]->audio_input) {
      a->input_id++;
    }
    if (!IOSequence[a->input_id]) {
      /* If we've run out of inputs, shut down the audio
	 device. */
      a->previous_device_state = a->current_device_state;
      a->current_device_state = AUDIO_UNAVAILABLE;
      (*a->callback_fn)(a, (void *) NULL, 0);
    } else {
      /* The current descriptor now has an audio segment. */
      a->samples_read = 0;      
      a->sample_rate = IOSequence[a->input_id]->audio_input->sample_rate;
      if (a->encoding_format)
	free(a->encoding_format);
      if (IOSequence[a->input_id]->audio_input->encoding_format)
	a->encoding_format = _gal_strdup(IOSequence[a->input_id]->audio_input->encoding_format);
      Gal_AddTask(__AudioSendTask, (void *) a, 100, 0, NULL);
    }
  }
}

static Gal_Frame __AudioStdinCallback(char *str,
				      MGal_StdinPoll *poll_obj)
{
  /* It doesn't matter what str is. We make our judgment based
     solely on the audio state. */
  AudioDevice *a = (AudioDevice *) MGal_GetStdinPollData(poll_obj);
  __AudioCallback(a);
  return (Gal_Frame) NULL;
}

AudioDevice *InitializeAudio(char *filename, AudioCallbackFn callback_fn,
			     int num_batch)
{
  AudioDevice *a;
  
  CE_InitializeIOSequence(filename);
  a = (AudioDevice *) calloc(1, sizeof(AudioDevice));
  a->callback_fn = callback_fn;
  a->current_device_state = AUDIO_IDLE;
  a->previous_device_state = AUDIO_IDLE;
  a->input_id = 0;
  a->batch = num_batch;
  /* It's a prompt interaction. */
  /* Because I don't have a
     connection object, I'll have to make sure to do
     all my work inside __StdinCallback, and make sure it
     always returns NULL. */
  if (!a->batch) {
    a->stdin_poll = MGalIO_CreateStdinPoll("Hit <return> to send speech: ",
					   (GalIO_CommStruct *) NULL,
					   __AudioStdinCallback,
					   500, 0);
    /* These need to point to each other. */ 
    MGal_SetStdinPollData(a->stdin_poll, (void *) a);
  }
  return a;  
}

void ReinitializeAudio(AudioDevice *a)
{
  a->input_id = 0;
  a->current_device_state = AUDIO_IDLE;
  a->previous_device_state = AUDIO_IDLE;
}

/* This is a half-duplex "audio device". */

void EnableAudioInput(AudioDevice *a)
{
  if (a->current_device_state == AUDIO_IDLE) {
    if (!a->batch) {
      MGal_ActivateStdinPoll(a->stdin_poll);
    }
  }
}

void EnableAudioOutput(AudioDevice *a, char *encoding_format,
		       int sample_rate)
{
  /* I don't think I really need to do anything here. */
  if (a->current_device_state == AUDIO_IDLE) {
    a->sample_rate = sample_rate;
    if (a->encoding_format)
      free(a->encoding_format);
    if (encoding_format)
      a->encoding_format = _gal_strdup(encoding_format);
    a->samples_read = 0;
    a->previous_device_state = a->current_device_state;
    a->current_device_state = AUDIO_PLAYING;
  }
}
    
void PlayAudio(AudioDevice *a, void *data, int num_samples)
{
  /* I don't think I really need to do anything here. */
  if (a->current_device_state == AUDIO_PLAYING) {
    a->samples_read += num_samples;
  }
}

void PlayingIsDone(AudioDevice *a)
{
  if (a->current_device_state == AUDIO_PLAYING) {
    a->previous_device_state = a->current_device_state;
    a->current_device_state = AUDIO_IDLE;
  }
  if (a->batch)
    __AudioCallback(a);
}

void ShutdownAudioInput(AudioDevice *a)
{
  if (!a->batch)
    MGal_FreeStdinPoll(a->stdin_poll);
}

/*
 * Synthesizer
 */

void InitializeSynthesizer(char *filename)
{
  CE_InitializeIOSequence(filename);
}

SynthesisRecord *InitializeSynthesis(char *output_string)
{
  int i;

  if (!output_string) {
    return (SynthesisRecord *) NULL;
  }
  
  for (i = 0; IOSequence[i]; i++) {
    /* If the output string is found, and if there's a
       synthesizer output, use it. */
    if (IOSequence[i]->generator_output &&
	(!_gal_strcasecmp(IOSequence[i]->generator_output, output_string)) &&
	IOSequence[i]->synthesizer_output) {
      SynthesisRecord *s = (SynthesisRecord *) calloc(1, sizeof(SynthesisRecord));
      s->sample_rate = IOSequence[i]->synthesizer_output->sample_rate;
      s->num_samples = IOSequence[i]->synthesizer_output->num_samples;
      s->encoding_format = IOSequence[i]->synthesizer_output->encoding_format;
      s->samples_written = 0;
      return s;
    }
  }
  return (SynthesisRecord *) NULL;
}

void *PollSynthesis(SynthesisRecord *s, int *num_samples)
{
  if (s->samples_written < s->num_samples) {
    int samples_left = s->num_samples - s->samples_written;
    int this_time, bytes_this_time, i;
    char *data_buf;
  
    /* Every 100 ms, this will fire until we run out
       of data to send. We'll send in 1024 sample chunks. */
    if (samples_left <= 1024) {
      this_time = samples_left;
      s->samples_written += samples_left;
    } else {
      this_time = 1024;
      s->samples_written += 1024;
    }
    bytes_this_time = this_time * 2;
  
    data_buf = (char *) calloc(bytes_this_time, sizeof(char));
    for (i = 0; i < bytes_this_time; i++) {
      data_buf[i] = (char) _gal_random() & 256;
    }
    *num_samples = this_time;
    return data_buf;
  } else {
    *num_samples = 0;
    return (void *) NULL;
  }
}

int SynthesisIsDone(SynthesisRecord *s)
{
  return (s->samples_written == s->num_samples);
}

void FreeSynthesis(SynthesisRecord *s)
{
  free(s);
}

/*
 * UI
 */


static char *__UICallback(UIDevice *a)
{  
  /* Find the current exchange with audio input. */
  while (IOSequence[a->input_id] &&
	 !IOSequence[a->input_id]->text_input) {
    a->input_id++;
  }
  if (!IOSequence[a->input_id]) {
    /* If we've run out of inputs, shut down the audio
       device. */
    (*a->callback_fn)(a, (char *) NULL);
    return (char *) NULL;
  } else {
    /* The current descriptor now has an audio segment. */
    char *next = IOSequence[a->input_id]->text_input;
    
    (*a->callback_fn)(a, next);
    a->input_id++;
    return next;
  }
}

static Gal_Frame __UIStdinCallback(char *str,
				   MGal_StdinPoll *poll_obj)
{
  /* It doesn't matter what str is. We make our judgment based
     solely on the audio state. */
  UIDevice *a = (UIDevice *) MGal_GetStdinPollData(poll_obj);
  __UICallback(a);
  return (Gal_Frame) NULL;
}


UIDevice *InitializeUI(char *filename, UICallbackFn callback_fn, int batch)
{
  UIDevice *a;
  
  CE_InitializeIOSequence(filename);  
  a = (UIDevice *) calloc(1, sizeof(UIDevice));
  a->callback_fn = callback_fn;
  a->input_id = 0;
  a->batch = batch;
  /* It's a prompt interaction. */
  /* Because I don't have a
     connection object, I'll have to make sure to do
     all my work inside __StdinCallback, and make sure it
     always returns NULL. */
  if (!batch) {
    a->stdin_poll = MGalIO_CreateStdinPoll("Hit <return> to send text: ",
					   (GalIO_CommStruct *) NULL,
					   __UIStdinCallback,
					   500, 0);
    /* These need to point to each other. */ 
    MGal_SetStdinPollData(a->stdin_poll, (void *) a);
  }
  return a;
}

void ReinitializeUI(UIDevice *a)
{
  a->input_id = 0;
}

void UIPresent(UIDevice *a, char *output_string)
{
  /* I don't think this does anything. */
  /* Unless it's batch. Then, send the next thing. */
  if (a->batch)
    __UICallback(a);
}

void EnableUIInput(UIDevice *a)
{
  if (!a->batch)
    MGal_ActivateStdinPoll(a->stdin_poll);
}

void ShutdownUIInput(UIDevice *a)
{
  if (!a->batch)
    MGal_FreeStdinPoll(a->stdin_poll);
}
