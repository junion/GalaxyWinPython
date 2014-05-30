/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include <stdio.h>

#include "gal_internal.h"

#include "galaxy/gthread.h"

#define FL_FREE 0
#define FL_ALLOC 1

/* Static Function Prototypes */
static int is_valid_type(int type);
static Gal_Frame alloc_frame(void);
static Gal_Symbol *fr_get_atts(Gal_Frame fr, int *nkeys);
static Gal_Object fr_setprop(Gal_Frame fr, Gal_Symbol key, Gal_Object val);

static _Gal_LocalMemory *__Gal_FrameMemory = (_Gal_LocalMemory *) NULL;

#define CHUNK 1000

/* Let's be anal retentive with the mutexes here. */
static Gal_Symbol wild = NULL;

static GalUtil_LocalMutex wild_mutex;
static GalUtil_LocalMutex nframe_mutex;

void _Gal_init_nframe(void)
{
  __Gal_FrameMemory = _Gal_LMCreate(sizeof(NFRAME), CHUNK);
  GalUtil_InitLocalMutex(&wild_mutex);
  GalUtil_InitLocalMutex(&nframe_mutex);
}

static Gal_Frame alloc_frame(void)
{
  Gal_Frame new_fr;
  int serial;

  GalUtil_LockLocalMutex(&nframe_mutex);
  if (!__Gal_FrameMemory) {
    GalUtil_UnlockLocalMutex(&nframe_mutex);
    GalUtil_Fatal("Frame memory not initialized; call Gal_InitializeStatics()");
  }

  new_fr = (Gal_Frame) _Gal_LMAllocate(__Gal_FrameMemory, &serial);
  GalUtil_UnlockLocalMutex(&nframe_mutex);

  if (!new_fr) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Gal_Frame allocation error");
    return (Gal_Frame) NULL;
  }
  new_fr->flags |= FL_ALLOC;
  new_fr->npred = 0;
  new_fr->ftype = 0;
  new_fr->serial = serial;
  return new_fr;
}

int _Gal_FreeAllFrames()
{
  int success = 0;
  
  GalUtil_LockLocalMutex(&nframe_mutex);
  if (__Gal_FrameMemory) {
    int active_elements = _Gal_LMFree(__Gal_FrameMemory, NULL, 1);

    if (active_elements > 0) {
      GalUtil_Warn("Couldn't free frame repository; %d left", active_elements);
    } else {
      __Gal_FrameMemory = (_Gal_LocalMemory *) NULL;
      success = 1;
    }
  }
  GalUtil_UnlockLocalMutex(&nframe_mutex);
  return success;
}

void Gal_FreeFrame(Gal_Frame to)
{
  _gal_free_frame_internal(to, 1);
}

/* SAM 9/14/99: The mutex for freeing frames and objects must be VERY
   local, otherwise you run the risk of getting deadlocks between
   threads (where one thread starts with freeing a frame object
   and another starts with freeing a frame) and within threads
   (when you recursively try to free a frame or an object).
   This might be computationally inefficient, but it is the safest
   thing. Thread-specific data is another option, but passing frames
   or objects from thread to thread would become a Pandora's box. */

void _gal_free_frame_internal(Gal_Frame fr, int level)
{
  int i;
  int n;
  void **vdata;

  if(fr == NULL) return;

  if(!(fr->flags & FL_ALLOC)){
    GalUtil_Print(-1,"Attempting to free unregistered nframe\n");
    return;
  }
  
  if(fr->plist != NULL)
  {
    vdata = (void **)_gal_get_vdata(fr->plist,&n);

    for(i=0;i<n;i++){
      if(vdata[i] == NULL) continue;
      _gal_free_object_internal(_gal_av_val(vdata[i]), level + 1);
      _gal_free_avpair(vdata[i]);
    }
    
    _gal_free_vlist(fr->plist);
    fr->plist = NULL;
  }
  fr->plist = NULL;
  for(i=0;i<fr->npred;i++){
    if(fr->pred[i] != NULL)
      _gal_free_object_internal(fr->pred[i], level + 1);
  }
  fr->npred = 0;
  fr->name = NULL;
  fr->flags = FL_FREE;
  _Gal_LMDeallocate(__Gal_FrameMemory, (void *) fr);
}

/* laborious method to ensure we've been passed a valid type */
static int is_valid_type(int type)
{
  if ((type != GAL_TOPIC) && (type != GAL_CLAUSE) && (type != GAL_PRED))
    return(GAL_FALSE);
  else
    return(GAL_TRUE);
}

/* type calls tp allocator  */
Gal_Frame Gal_MakeFrame(const char *name, Gal_FrameType type)
{
  Gal_Frame fr;

  if (!is_valid_type(type)) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Found invalid frame type %d while trying to make frame\n", type);
    return(NULL);
  }

  fr = alloc_frame();
  fr->ftype = type;
  fr->name = add_sym(name);
  return(fr);
}

/*
 *  convenience functions
 */

Gal_Frame Gal_MakeClauseFrame(const char *name)
{
  return Gal_MakeFrame(name, GAL_CLAUSE);
}

Gal_Frame Gal_MakeTopicFrame(const char *name)
{
  return Gal_MakeFrame(name, GAL_TOPIC);
}

Gal_Frame Gal_MakePredFrame(const char *name)
{
  return Gal_MakeFrame(name, GAL_PRED);
}

/*--------------------------------------------------------------------------------
 * Predicate (in the LISP sense). Determines if the passed Gal_Frame (arg 1)
 * is of type 'type' (arg 2). If the supplied type is invalid, returns FALSE
 * without checking to see if Gal_Frame has same bogus type.
 * 
 * RETURNS: TRUE (1) or FALSE (0).
 * REQUIRES: a valid Gal_Frame and Gal_Frame type to be given
 * SIDE EFFECTS: None
 * FUTURE: 
 --------------------------------------------------------------------------------*/
int Gal_FrameIsType(Gal_Frame fr, Gal_FrameType type)
{

  /* Warn if we're passed a bogus type, and return FALSE */
  if (!is_valid_type(type)) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Found invalid frame type %d while testing frame type\n", type);
    return(GAL_FALSE);
  }

  /* Next, check the (NFRAME *) for validity */
  if (fr == NULL) 
    return(GAL_FALSE);

  return(fr->ftype == type);  
}

/*
 *  convenience functions
 */

int Gal_ClauseFramep(Gal_Frame fr)
{
  if (fr && fr->ftype == GAL_CLAUSE)
    return GAL_TRUE;
  return GAL_FALSE;
}

int Gal_TopicFramep(Gal_Frame fr)
{
  if (fr && fr->ftype == GAL_TOPIC)
    return GAL_TRUE;
  return GAL_FALSE;
}

int Gal_PredFramep(Gal_Frame fr)
{
  if (fr && fr->ftype == GAL_PRED)
    return GAL_TRUE;
  return GAL_FALSE;
}

/*************************************************************
 *
 * Accessors
 *
 *************************************************************
 */

Gal_Symbol _gal_fr_sym_name(Gal_Frame fr)
{
  if(fr == NULL) return(NULL);
  return(fr->name);
}

char *Gal_FrameName(Gal_Frame fr)
{
  if(fr == NULL) return(NULL);
  if (fr->name == NULL) return(NULL);
  return(sym_name(fr->name));
}

Gal_Frame Gal_SetFrameName(Gal_Frame fr, const char *name)
{
  if(fr == NULL) return(NULL);
  fr->name = add_sym(name);
  return(fr);
}

Gal_Frame Gal_SetFrameType(Gal_Frame fr, Gal_FrameType type)
{
  if(fr == NULL) return(NULL);
  fr->ftype = type;
  return(fr);
}

Gal_FrameType Gal_GetFrameType(Gal_Frame fr)
{
  if(fr == NULL) return(GAL_NULLFRAME);

  return(fr->ftype);
}

Gal_Frame fr_set_name(Gal_Frame fr, Gal_Symbol name)
{
  if(fr == NULL) return(NULL);
  fr->name = name;
  return(fr);
}

int Gal_FrameNameEq(Gal_Frame fr, const char *name)
{
  if(fr == NULL) return(0);
  if(name == NULL) return(0);
  return (fr->name == add_sym(name));
}

int Gal_FrameNamesEq(Gal_Frame fr1, Gal_Frame fr2)
{
  if(fr1 && fr2 && (fr1->name == fr2->name))
    return(1);
  return(0);
}

void Gal_ClearPreds(Gal_Frame fr)
{
  int i;

  if(fr == NULL) return;
  for(i=0;i<fr->npred;i++){
    Gal_FreeObject(fr->pred[i]);
    fr->pred[i] = NULL;
  }
  fr->npred = 0;
}

/*
 * Routines to get at pred field
 */
int Gal_NumPreds(Gal_Frame fr)
{
  if(fr == NULL) return(0);
  return(fr->npred);
}

Gal_Object fr_get_pred(Gal_Frame fr, int i)
{
  if(fr == NULL) return(NULL);
  if((i < 0) || (i >= fr->npred)) return(NULL);
  return(fr->pred[i]);
}

Gal_Frame Gal_GetPred(Gal_Frame fr, int i)
{
  if(fr == NULL) return(NULL);
  if((i < 0) || (i >= fr->npred)) return(NULL);
  return(Gal_PredValue(fr->pred[i]));
}

Gal_Object fr_add_pred(Gal_Frame fr, Gal_Object pred)
{
  Gal_Object *temp;
  int n;
  int i;
  if(fr == NULL) return(NULL);
  if(pred == NULL) return(NULL);
  if(fr->npred >= fr->maxpred){
    n = (2 * fr->maxpred) + 10;
    temp = (Gal_Object *) calloc(n,sizeof(Gal_Object));
    for(i=0;i<fr->npred;i++)
      temp[i] = fr->pred[i];
    if(fr->pred)
      free(fr->pred);
    fr->pred = temp;
    fr->maxpred = n;
  }
  fr->pred[fr->npred++] = pred;
  return(pred); 
}

Gal_Frame Gal_AddPred(Gal_Frame fr, Gal_Frame pred)
{
  Gal_Object *temp;
  Gal_Object pred_object;
  int n;
  int i;
  if(fr == NULL) return(NULL);
  if(!Gal_PredFramep(pred))
  {
    if (pred)
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Not add non-predicate frame to frame predicate list");
      GalUtil_PPFrame(GAL_WARNING_LEVEL, pred);
    }
    return(NULL);
  }
  pred_object = Gal_FrameObject(pred);
  if(fr->npred >= fr->maxpred){
    n = (2 * fr->maxpred) + 10;
    temp = (Gal_Object *) calloc(n,sizeof(Gal_Object));
    for(i=0;i<fr->npred;i++)
      temp[i] = fr->pred[i];
    if(fr->pred)
      free(fr->pred);
    fr->pred = temp;
    fr->maxpred = n;
  }
  fr->pred[fr->npred++] = pred_object;
  return(pred);
}

Gal_Object Gal_RemPred(Gal_Frame fr, int i)
{
  Gal_Object pred;
  int j;

  if(fr == NULL) return(NULL);
  if((i < 0) || (i >= fr->npred)) return(NULL);
  pred = fr->pred[i];
  for(j=i+1;j<fr->npred;j++)
    fr->pred[j-1] = fr->pred[j];
  fr->npred--;
  fr->pred[fr->npred] = NULL;
  return(pred); 
}

void Gal_DelPred(Gal_Frame fr, int i)
{
  Gal_Object pred;

  pred = Gal_RemPred(fr, i);
  Gal_FreeObject(pred);
}

/*
 *  Routines to get at plist entries
 */

Gal_Object _gal_fr_getprop(Gal_Frame fr, Gal_Symbol key)
{
  if(fr == NULL) return(NULL);
  if(fr->plist == NULL) return(NULL);
  return(_gal_getprop(fr->plist, (char *) key));
}

/*
 *  string version of above
 */
Gal_Object Gal_GetObject(Gal_Frame fr, const char *key)
{
  if(fr == NULL) return(NULL);
  if(fr->plist == NULL) return(NULL);
  return(_gal_getprop(fr->plist, add_sym(key)));
}

/*
 *  same as above and strips off Gal_Object wrapper
 */
void *_gal_fr_getsvalue(Gal_Frame fr, char *key)
{
  if(fr == NULL) return(NULL);
  if(fr->plist == NULL) return(NULL);
  return(_gal_to_value(_gal_getprop(fr->plist, add_sym(key))));
}

/*
 *  same as above with type checking
 */
static void *_gal_getvalue(Gal_Frame fr, const char *key, Gal_ObjectType type)
{
  if(fr == NULL) return(NULL);
  if(fr->plist == NULL) return(NULL);
  return(_gal_object_value_warn(_gal_getprop(fr->plist, add_sym(key)), type, __FUNCTION__,key));
}

/*
 *  convenience functions
 */

Gal_Frame Gal_GetFrame(Gal_Frame fr, const char *key)
{
  return((Gal_Frame)_gal_getvalue(fr, key, GAL_FRAME));
}

Gal_Frame Gal_GetTopicFrame(Gal_Frame fr, const char *key)
{
  return((Gal_Frame)_gal_getvalue(fr, key, GAL_TOPIC_FRAME));
}

char *Gal_GetString(Gal_Frame fr, const char *key)
{
  return((char *)_gal_getvalue(fr, key, GAL_STRING));
}

int Gal_GetInt(Gal_Frame fr, const char *key)
{
  return((int)_gal_getvalue(fr, key, GAL_INT));
}

float Gal_GetFloat(Gal_Frame fr, const char *key)
{
  Gal_Object float_obj;

  float_obj = Gal_GetObject(fr, key);
  return(Gal_FloatValue(float_obj));
}

Gal_Object *Gal_GetList(Gal_Frame fr, const char *key, int *length)
{
  Gal_Object list_obj;

  list_obj = Gal_GetObject(fr, key);
  return(Gal_ListValue(list_obj, length));
}

void *Gal_GetBinary(Gal_Frame fr, const char *key, int *size)
{
  Gal_Object binary_obj;

  binary_obj = Gal_GetObject(fr, key);
  return(Gal_BinaryValue(binary_obj, size));
}

void *Gal_GetInt16(Gal_Frame fr, const char *key, int *size)
{
  Gal_Object binary_obj;

  binary_obj = Gal_GetObject(fr, key);
  return(Gal_Int16Value(binary_obj, size));
}

void *Gal_GetInt32(Gal_Frame fr, const char *key, int *size)
{
  Gal_Object binary_obj;

  binary_obj = Gal_GetObject(fr, key);
  return(Gal_Int32Value(binary_obj, size));
}

void *Gal_GetInt64(Gal_Frame fr, const char *key, int *size)
{
  Gal_Object binary_obj;

  binary_obj = Gal_GetObject(fr, key);
  return(Gal_Int64Value(binary_obj, size));
}

void *Gal_GetFloat32(Gal_Frame fr, const char *key, int *size)
{
  Gal_Object binary_obj;

  binary_obj = Gal_GetObject(fr, key);
  return(Gal_Float32Value(binary_obj, size));
}

void *Gal_GetFloat64(Gal_Frame fr, const char *key, int *size)
{
  Gal_Object binary_obj;

  binary_obj = Gal_GetObject(fr, key);
  return(Gal_Float64Value(binary_obj, size));
}

GalSS_BrokerProxy *Gal_GetProxy(Gal_Frame fr, const char *key)
{
  Gal_Object proxy;
  
  proxy = Gal_GetObject(fr, key);
  return Gal_ProxyValue(proxy);
}


/*
 *  Like GetProp, but removes the property from the frame
 */
Gal_Object Gal_RemProp(Gal_Frame fr, const char *key)
{
  if(fr == NULL) return(NULL);
  if(fr->plist == NULL) return(NULL);
  return((Gal_Object)_gal_remprop(fr->plist, add_sym(key)));
}

/*
 *  Like RemProp, but also frees the value
 */
int Gal_DelProp(Gal_Frame fr, const char *key)
{
  Gal_Object to;
  to = Gal_RemProp(fr,key);	/* occassionally, remprop doesn't seem to return the right thing!? */
  if(to)
    Gal_FreeObject(to);
  return to != NULL;
}

/*
 * get predicate frame by name
 */
Gal_Frame Gal_GetPredByName(Gal_Frame fr, const char *name)
{
  int i;
  Gal_Frame pred;

  if(fr == NULL) return(NULL);
  for(i=0;i<fr->npred;i++){
    pred = Gal_PredValue(fr->pred[i]);
    if(Gal_FrameNameEq(pred, name))
      return(pred);
  }
  return(NULL);
}

/*
 * removes predicate frame by name
 * and returns it WITHOUT freeing it
 */
Gal_Object Gal_RemPredByName(Gal_Frame fr, const char *name)
{
  int i;
  Gal_Frame pred;

  if(fr == NULL) return(NULL);
  for(i=0;i<fr->npred;i++){
    pred = Gal_PredValue(fr->pred[i]);
    if(Gal_FrameNameEq(pred, name))
      return(Gal_RemPred(fr,i));
  }
  return(NULL);
}

void _gal_fr_delpred(Gal_Frame fr, Gal_Symbol key)
{
  int i;
  if(fr == NULL) return;
  for(i=0;i<fr->npred;){
    if(!Gal_Framep(fr->pred[i]))
      i++;
    else if(_gal_fr_sym_name(Gal_PredValue(fr->pred[i])) == key)
    {
      Gal_FreeObject(Gal_RemPred(fr,i));
    }
    else
      i++;
  }
}

void Gal_DelPredByName(Gal_Frame fr, const char *name)
{
  Gal_Object pred;

  pred = Gal_RemPredByName(fr, name);
  Gal_FreeObject(pred);
}

/* SAM 10/31/00: HUGE bug here. I was getting the size of
   the plist, which may have empty values. So I wrote a
   function which counts the properties the way Gal_GetProperties
   does. */

int Gal_NumProperties(Gal_Frame fr)
{
  if (fr && fr->plist)
  {
    return(_gal_get_num_atts(fr->plist, 1));
  }
  return(0);
}

int Gal_NumNonNullProperties(Gal_Frame fr)
{
  if (fr && fr->plist)
  {
    return(_gal_get_num_atts(fr->plist, 0));
  }
  return(0);
}

char **Gal_GetProperties(Gal_Frame fr, int *nkeys)
{
  Gal_Symbol *atts;
  char **keys;
  int i, natts;

  if (nkeys)
    *nkeys = 0;
  else
    return(NULL);

  if (fr)
  {
    atts = (Gal_Symbol *)_gal_get_atts(fr->plist, &natts);
    if (atts)
    {
      keys = (char **)calloc(natts, sizeof(char *));
      for (i=0; i<natts; i++)
	keys[i] = sym_name((Gal_Symbol)atts[i]);
      free(atts);
      *nkeys = natts;
      return(keys);
    }
  }
  return(NULL);
}

static Gal_Symbol *fr_get_atts(Gal_Frame fr, int *nkeys)
{
  if (nkeys)
    *nkeys = 0;
  else
    return(NULL);

  if (fr)
    return((Gal_Symbol *)_gal_get_atts(fr->plist, nkeys));
  else
    return(NULL);
}

/*
 * Setprop routine for frames; frees old value if setting an old value
 */
static Gal_Object fr_setprop(Gal_Frame fr, Gal_Symbol key, Gal_Object val)
{
  Gal_Object old_to;
  if(fr == NULL) return(NULL);
  if(fr->plist == NULL){
    fr->plist = _gal_alloc_plist();
    return(_gal_setprop(fr->plist, key, val));
  }
  old_to = _gal_getprop(fr->plist, key);
  if (old_to)
    Gal_FreeObject (old_to);
  return(_gal_setprop(fr->plist, key, val));
}

/*
 * string version of above
 */
Gal_Object Gal_SetProp(Gal_Frame fr, const char *key, Gal_Object val)
{
  Gal_Object old_to;
  Gal_Symbol skey;

  if (val)
    val->count++;

  skey = add_sym(key);
  if(fr == NULL) return(NULL);
  if(fr->plist == NULL){
    fr->plist = _gal_alloc_plist();
    return(_gal_setprop(fr->plist, skey, val));
  }
  old_to = _gal_getprop(fr->plist, skey);
  if (old_to)
    Gal_FreeObject (old_to);
  return(_gal_setprop(fr->plist, skey, val));
}

/* IO support routines  */
Gal_Frame Gal_CopyFrame(Gal_Frame fr)
{
  Gal_Frame nf;
  int i;
  Gal_Symbol *atts;
  int natts;

  if(fr == NULL) return(NULL);
  nf = alloc_frame();
  nf->ftype = fr->ftype;
  nf->name = fr->name;
  if(fr->plist != NULL){
    atts = (Gal_Symbol *)_gal_get_atts(fr->plist, &natts);
    for(i=0;i<natts;i++){
      fr_setprop(nf,atts[i],Gal_CopyObject(_gal_fr_getprop(fr,atts[i])));
    }
    free(atts);
  }
  nf->npred = fr->npred;
  nf->maxpred = fr->npred;
  if(nf->maxpred > 0) {
    if(nf->pred)
      free(nf->pred);
    nf->pred = (Gal_Object *)calloc(nf->maxpred,sizeof(Gal_Object));
  }
  for(i=0;i<fr->npred;i++){
    nf->pred[i] = Gal_CopyObject(fr->pred[i]);
  }
  return((Gal_Frame)nf);
}

/* Frame update function. */

static int _gal_set_property(const char *key, Gal_Object value,
			     void *caller_data)
{
  Gal_Frame f = (Gal_Frame) caller_data;
  Gal_SetProp(f, key, Gal_CopyObject(value));
  return 1;
}

/* Delete happens before set. */

void Gal_UpdateFrameProperties(Gal_Frame target_frame,
			       Gal_Frame source_properties,
			       char ** delete_properties)
{
  if (target_frame) {     
    if (delete_properties) {
      int i;
    
      for (i = 0; delete_properties[i]; i++) {
	Gal_DelProp(target_frame, delete_properties[i]);
      }
    }
    if (source_properties) {
      Gal_DoProperties(source_properties, _gal_set_property,
		       (void *) target_frame);
    }
  }
}

/* compariison of frames, order does not matter for atts and preds  */
int Gal_FrameEqual(Gal_Frame sf, Gal_Frame sem)
{
  int n;
  int i;
  int j;
  Gal_Object to1;
  Gal_Object to2;
  Gal_Symbol *atts;
  int ok;
  if(0){
    GalUtil_Print(-1,"Testing equality of\n");
    Gal_PrFrame(sf);
    GalUtil_Print(-1,"and\n");
    Gal_PrFrame(sem);
  }
  if(!Gal_FrameNamesEq(sf, sem)){
    if(0) GalUtil_Print(-1,"fails\n");
    return(0);
  }
  atts = fr_get_atts(sem, &n);
  for(i=0;i<n;i++){
    to1 = _gal_fr_getprop(sf,atts[i]);
    if(to1 == NULL){
      if(0) GalUtil_Print(-1,"fails\n");
      return(0);
    }
    to2 = _gal_fr_getprop(sem,atts[i]);
    if(to2 == NULL){
      if(0) GalUtil_Print(-1,"fails\n");
      return(0);
    }
    if(!Gal_ObjectEqual(to1,to2)){
      if(0) GalUtil_Print(-1,"fails\n");
      return(0);
    }
  }
  free(atts);

  /* for each pred in sf we must find an equal in sem */
  for(i=0;i<sf->npred;i++){
    ok = 0;
    for(j=0;j<sem->npred;j++){
      if(Gal_FrameEqual(Gal_PredValue(sf->pred[i]), Gal_PredValue(sem->pred[i]))){
	ok++;
	break;
      }
    }
    if(!ok){
      if(0) GalUtil_Print(-1,"fails\n");
      return(0);
    }
  }
  if(0) GalUtil_Print(-1,"succeeds\n");
  return(1);
}

/*
 * All atts of sem must be present and match in sf
 * "*" Gal_FrameName is treated as wildcard
 */

int Gal_MatchFrame(Gal_Frame sf, Gal_Frame sem)
{
  int n;
  int i;
  Gal_Object to1;
  Gal_Object to2;
  Gal_Symbol *atts;

  if (sf == sem)
    return 1;

  GalUtil_LockLocalMutex(&wild_mutex);
  if(wild == NULL) wild = add_sym("*");
  GalUtil_UnlockLocalMutex(&wild_mutex);
  if(_gal_fr_sym_name(sf) != wild)
    if(_gal_fr_sym_name(sem) != wild)
      if(_gal_fr_sym_name(sf) != _gal_fr_sym_name(sem)) return(0);

  if (Gal_ClauseFramep(sem) && !Gal_ClauseFramep(sf)) 
    return(0);
  if (Gal_PredFramep(sem) && !Gal_PredFramep(sf)) 
    return(0);
  if (Gal_TopicFramep(sem) && !Gal_TopicFramep(sf)) 
    return(0);
  
  atts = fr_get_atts(sem, &n);
  for(i=0;i<n;i++){
    to1 = _gal_fr_getprop(sf,atts[i]);
    if(to1 == NULL) return(0);
    to2 = _gal_fr_getprop(sem,atts[i]);
    if(!_gal_match_tobj(to1,to2)) return(0);
  }
  free(atts);

  return(1);
}

/* Internal support for mapping through properties.
   I don't want this code repeated over and over.
   The prop_fn should return 1 to continue, 0 to halt. */

void Gal_DoProperties(Gal_Frame fr,
		      int (*prop_fn)(const char *, Gal_Object, void *),
		      void *caller_data)
{
  Av *vdata;
  int num_props, i;
  char *key;

  if (fr->plist != NULL) {
    vdata = (Av *)_gal_get_vdata(fr->plist, &num_props);
    for (i = 0; i < num_props; i++) {
      /* If the slot is empty, continue. */
      if (vdata[i] == NULL) continue;
      /* If the value has been removed, continue.
	 This case is a safeguard. */
      key = sym_name((Gal_Symbol) _gal_av_att(vdata[i]));
      if (_gal_av_val(vdata[i]) == NULL) {
	GalUtil_WarnWithLocation(__FUNCTION__, "Skipping key %s with NULL value while applying function to frame properties", key);
	continue;
      }
      if (!((*prop_fn)(key, (Gal_Object) _gal_av_val(vdata[i]),
		       caller_data))) {
	break;
      }
    }
  }
}

/* Ditto for preds. */

void Gal_DoPreds(Gal_Frame fr, int (*pred_fn)(Gal_Object, void *),
		 void *caller_data)
{
  int i;

  for (i=0;i<fr->npred;i++) {
    if (!((*pred_fn)(fr->pred[i], caller_data))) {
      break;
    }
  }
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
