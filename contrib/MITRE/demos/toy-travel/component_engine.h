/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include "galaxy/galaxy_all.h"
#include "MITRE_galaxy.h"

/* I'm building these structures to be at least
   remotely plausible from the wrapper point of view. */

#ifndef __COMPONENT_ENGINE_H__
#define __COMPONENT_ENGINE_H__

typedef struct __AudioDescriptor {
  int sample_rate;
  char *encoding_format;
  int num_samples;
} AudioDescriptor;

typedef struct __ParseTree {
  Gal_Frame tree;
} ParseTree;

typedef struct __DBResult {
  char **column_names;
  int nfound;
  char ***values;
} DBResult;

typedef struct __Exchange {
  AudioDescriptor *audio_input;
  char *text_input;
  char *recognizer_output;
  ParseTree *parser_output;
  char *sql_query;
  DBResult *backend_output;
  ParseTree *dialogue_output;
  char *generator_output;
  AudioDescriptor *synthesizer_output;
} Exchange;

void CE_InitializeIOSequence(char *filename);
Exchange **CE_ReadIOSequence(char *filename);

/* Individual engines */

/* Parser */

void InitializeParser(char *filename);
ParseTree *ParseSentence(char *input_string);
Gal_Frame ParseTreeToFrame(ParseTree *p);

/* Generator */

void InitializeGenerator(char *filename);
char *GenerateSentenceFromTree(ParseTree *output_tree);
char *GenerateSentenceFromDBResult(DBResult *db);
ParseTree *FrameToParseTree(Gal_Frame frame);
void FreeParseTree(ParseTree *p);
ParseTree *CopyParseTree(ParseTree *p);
DBResult *FrameToDBResult(Gal_Frame frame);

/* Backend */

void InitializeBackend(char *filename);
DBResult *RetrieveDBResult(char *sql_query);
Gal_Frame DBResultToFrame(DBResult *db);

/* Dialogue */

typedef struct __DialogueAction {
  int step_type;
  void *step_data;
} DialogueAction;

enum {USER_INPUT, CONSULT_BACKEND, BACKEND_RESPONSE, PRESENT_OUTPUT};

void InitializeDialogue(char *filename);
char *DialogueStepName(int step_type);
DialogueAction *NextDialogueStep(void *step_data, int step_type);
DBResult *HubFrameToDBResult(Gal_Frame f);
void FreeDBResult(DBResult *db);
void FreeDialogueAction(DialogueAction *a);

/* Recognizer */

typedef AudioDescriptor RecognizerRecord;

void InitializeRecognizer(char *filename);
void IncrementalRecognize(RecognizerRecord *rec, void *data, int n_samples);
RecognizerRecord *InitializeRecognition(char *encoding_format, int sample_rate);
void FreeRecognizerRecord(RecognizerRecord *rec);
char *FinishRecognition(RecognizerRecord *rec);

/* Audio */

enum {AUDIO_IDLE, AUDIO_RECORDING, AUDIO_PLAYING, AUDIO_UNAVAILABLE};

struct __AudioDevice;
typedef void (*AudioCallbackFn)(struct __AudioDevice *a,
				void *data, int num_samples);

typedef struct __AudioDevice {
  int input_id;
  int batch;
  int current_device_state;
  int previous_device_state;
  AudioCallbackFn callback_fn;
  int samples_read;
  int sample_rate;
  char *encoding_format;
  MGal_StdinPoll *stdin_poll;
  void *client_data;
} AudioDevice;

AudioDevice *InitializeAudio(char *filename, AudioCallbackFn callback_fn,
			     int num_batch);
char *AudioDeviceStateName(int audio_state);
void EnableAudioInput(AudioDevice *a);
void EnableAudioOutput(AudioDevice *a, char *encoding_format,
		       int sample_rate);
void PlayAudio(AudioDevice *a, void *data, int num_samples);
void PlayingIsDone(AudioDevice *a);
void ReinitializeAudio(AudioDevice *a);
void ShutdownAudioInput(AudioDevice *a);

/* Synthesizer */

typedef struct __SynthesisRecord {
  int sample_rate;
  char *encoding_format;
  int num_samples;
  int samples_written;
} SynthesisRecord;

void InitializeSynthesizer(char *data_file);
SynthesisRecord *InitializeSynthesis(char *output_string);
void *PollSynthesis(SynthesisRecord *s, int *num_samples);
int SynthesisIsDone(SynthesisRecord *s);
void FreeSynthesis(SynthesisRecord *s);

/* UI */

struct __UIDevice;
typedef void (*UICallbackFn)(struct __UIDevice *a, char *input);

typedef struct __UIDevice {
  int input_id;
  UICallbackFn callback_fn;
  MGal_StdinPoll *stdin_poll;
  int batch;
  void *client_data;
} UIDevice;

UIDevice *InitializeUI(char *filename, UICallbackFn callback_fn, int batch);
void UIPresent(UIDevice *a, char *output_string);
void EnableUIInput(UIDevice *a);
void ReinitializeUI(UIDevice *a);
void ShutdownUIInput(UIDevice *a);
#endif
