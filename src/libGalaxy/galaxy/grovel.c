/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gal_internal.h"

static Gal_Frame sfind_pred(Gal_Object to, Sym psym);
static void sdelete_pred(Gal_Object to, Sym psym);
static Gal_Frame sfind_topic(Gal_Object to, Sym tsym);
static Gal_Object sfind_key(Gal_Object to, Sym ksym);
static Gal_Object smatch_key_value(Gal_Object to, Sym ksym, Gal_Object match);

/*  recursively search for a predicate with the specified name
 *  the first predicate found is returned
 */

Gal_Frame Gal_FindPredParent(Gal_Frame frame, const char *name, Gal_Frame parent, int findpar, int nth)
{
  Gal_Frame pred;
  Gal_Object to_frame, to_parent;
  int i;
  
  to_frame = Gal_FrameObject(frame);
  to_parent = Gal_FrameObject(frame);

  if (Gal_ClauseFramep(frame)) {
    /* Check the predicates of the clause */
    for (i=0; i<Gal_NumPreds(frame); i++) {
      pred = Gal_FindPredParent (Gal_GetPred(frame, i), name, frame, findpar, nth);
      if (pred) {
	return (pred);
      }
    }
    /* Check the topic of the clause */
    return (Gal_FindPredParent(Gal_GetTopicFrame(frame, ":topic"), name, frame, findpar, nth));
  }	
  else if (Gal_TopicFramep(frame)) {
    /* Check the predicates of the topic */
    for (i=0; i<Gal_NumPreds(frame); i++) {
      pred = Gal_FindPredParent (Gal_GetPred(frame, i), name, frame, findpar, nth);
      if (pred) {
	return (pred);
      }
    }

    /* Return NULL on failure */
    return (NULL);
  }
  else if (Gal_PredFramep(frame)) {
    pred = Gal_PredValue(to_frame);
    /* If the predicate matches, return it */
    if ( Gal_StringEq ( Gal_FrameName (frame) , name) ) {
      nth--;
      if(nth <= 0)
	return (findpar ? Gal_FrameValue(to_parent) : frame);
    }	
    /* Check the predicates of the predicate */
    for (i=0; i<Gal_NumPreds(frame); i++) {
      pred = Gal_FindPredParent (Gal_GetPred(frame, i), name, frame, findpar,nth);
      if (pred) {
	return (pred);
      }
    }
    /* Check the topic of the predicate */
    return (Gal_FindPredParent(Gal_GetTopicFrame(frame, ":topic"), name, frame, findpar,nth));
  }      	

  return (NULL);
}

Gal_Frame Gal_FindPred(Gal_Frame fr, const char *pred_name)
{
  return sfind_pred(Gal_FrameObject(fr), add_sym(pred_name));
}

static Gal_Frame sfind_pred(Gal_Object to, Sym psym)
{
  Gal_Frame fr, pred;
  int i;
  
  if (Gal_Framep(to))
  {
    fr = Gal_FrameValue(to);
    if (Gal_Clausep (to))
    {
      /* Check the predicates of the clause */
      for (i=0; i<Gal_NumPreds(fr); i++)
      {
	pred = sfind_pred(fr_get_pred(fr, i), psym);
	if (pred) {
	  return (pred);
	}
      }
      /* Check the topic of the clause */
      return (sfind_pred(Gal_GetObject(fr, ":topic"), psym));
    }
    else if (Gal_Topicp(to))
    {
      /* Check the predicates of the topic */
      for (i=0; i<Gal_NumPreds(fr); i++)
      {
	pred = sfind_pred(fr_get_pred(fr, i), psym);
	if (pred) {
	  return (pred);
	}
      }

      /* Return NULL on failure */
      return (NULL);
    }
    else if (Gal_Predp (to))
    {
      pred = Gal_FrameValue(to);
      /* If the predicate matches, return it */
      if (_gal_fr_sym_name(fr) == psym) {
	  return (fr);
      }
      /* Check the predicates of the predicate */
      for (i=0; i<Gal_NumPreds(fr); i++)
      {
	pred = sfind_pred(fr_get_pred(fr, i), psym);
	if (pred) {
	  return (pred);
	}
      }
      /* Check the topic of the predicate */
      return (sfind_pred(Gal_GetObject(fr, ":topic"), psym));
    }      
  }

  return (NULL);
}

/* recursively delete all predicates with the specified name */

void Gal_DeletePreds(Gal_Frame fr, const char *pred_name)
{
  sdelete_pred(Gal_FrameObject(fr), add_sym(pred_name));
}

static void sdelete_pred(Gal_Object to, Sym psym)
{
  Gal_Frame fr;
  int i;
  
  if (Gal_Framep(to))
  {
    fr = Gal_FrameValue(to);
    if (Gal_Clausep (to))
    {
      /* Check the predicates of the clause */
      _gal_fr_delpred(fr, psym);

      for (i=0; i<Gal_NumPreds(fr); i++)
	sdelete_pred (fr_get_pred(fr, i), psym);

      /* Check the topic of the clause */
      sdelete_pred (Gal_GetObject(fr, ":topic"), psym);
    }
    else if (Gal_Topicp(to))
    {
      /* Check the predicates of the topic */
      _gal_fr_delpred(fr, psym);

      for (i=0; i<Gal_NumPreds(fr); i++)
	sdelete_pred(fr_get_pred(fr, i), psym);

      /* Check the superlative of the topic */
      sdelete_pred(Gal_GetObject(fr, ":superlative"), psym);
    }
    else if (Gal_Predp(to))
    {
      /* Check the predicates of the predicate */
      _gal_fr_delpred(fr, psym);

      for (i=0; i<Gal_NumPreds(fr); i++)
	sdelete_pred(fr_get_pred(fr, i), psym);

      /* Check the topic of the predicate */
      sdelete_pred(Gal_GetObject(fr, ":topic"), psym);
    }      
  }
}

/* recursively search for a topic with the specified name */

Gal_Frame Gal_FindTopic(Gal_Frame fr, const char *topic_name)
{
  return(sfind_topic(Gal_FrameObject(fr), add_sym(topic_name)));
}

static Gal_Frame sfind_topic(Gal_Object to, Sym tsym)
{
  Gal_Frame fr, topic;
  int i;

  if (Gal_Framep(to))
  {
    fr = Gal_FrameValue(to);
    if (Gal_Clausep (to))
    {
      if ((topic = sfind_topic (Gal_GetObject(fr, ":topic"), tsym)))
	return topic;

      /* Check the predicates of the clause */
      for (i=0; i<Gal_NumPreds(fr); i++)
      {
	topic = sfind_topic (fr_get_pred(fr, i), tsym);
	if (topic)
	  return (topic);
      }
    }
    else if (Gal_Topicp(to))
    {
      if (_gal_fr_sym_name(fr) == tsym)
	return (fr);

      /* Check the predicates of the topic */
      for (i=0; i<Gal_NumPreds(fr); i++)
      {
	topic = sfind_topic(fr_get_pred(fr, i), tsym);
	if (topic)
	  return (topic);
      }
#if 0
      /* only look under a "for" predicate for the specified topic */
      if (topic = sfind_pred(to, add_sym("for")))
	return (sfind_topic (Gal_FrameObject (topic), tsym));
#endif
      /* Return NULL on failure */
      return (NULL);
    }
    else if (Gal_Predp (to))
    {
      if ((topic = sfind_topic (Gal_GetObject(fr, ":topic"), tsym)))
	return topic;

      /* Check the predicates of the predicate */
      for (i=0; i<Gal_NumPreds(fr); i++)
      {
	topic = sfind_topic(fr_get_pred(fr, i), tsym);
	if (topic)
	  return (topic);
      }
    }
  }
  return (NULL);
}

/*  recursively search for the specified key
 *  if found, the corresponding value is returned
 */

Gal_Object Gal_FindKey(Gal_Frame fr, const char *key_name)
{
  return(sfind_key(Gal_FrameObject(fr), add_sym(key_name)));
}

static Gal_Object sfind_key(Gal_Object to, Sym ksym)
{
  Gal_Frame fr;
  Gal_Object tvalue;
  int i;
  
  if (Gal_Framep(to))
  {
    fr = Gal_FrameValue(to);
    tvalue = _gal_fr_getprop(fr, ksym);

    if (tvalue)
      return(tvalue);
    
    /* Check the predicates of the frame */
    for (i=0; i<Gal_NumPreds(fr); i++)
    {
      tvalue = sfind_key(fr_get_pred(fr, i), ksym);
      if (tvalue)
	return (tvalue);
    }
    /* Check the topic of the frame */
    return (sfind_key(Gal_GetObject(fr, ":topic"), ksym));
  }

  return (NULL);
}

/*  recursively search for the specified key value pair
 *  if found, the value is returned
 */

Gal_Object Gal_MatchKeyValue(Gal_Frame fr, const char *key_name, Gal_Object match)
{
  return(smatch_key_value(Gal_FrameObject(fr), add_sym(key_name), match));
}

static Gal_Object smatch_key_value(Gal_Object to, Sym ksym, Gal_Object match)
{
  Gal_Frame fr;
  Gal_Object tvalue;
  int i;

  if (Gal_Framep(to))
  {
    fr = Gal_FrameValue(to);
    tvalue = _gal_fr_getprop(fr, ksym);

    if (Gal_ObjectEqual(tvalue, match))
      return(tvalue);
    
    /* Check the predicates of the frame */
    for (i=0; i<Gal_NumPreds(fr); i++)
    {
      tvalue = smatch_key_value(fr_get_pred(fr, i), ksym, match);
      if (tvalue)
	return (tvalue);
    }
    /* Check the topic of the frame */
    return (smatch_key_value(Gal_GetObject(fr, ":topic"), ksym, match));
  }

  return (NULL);
}

#if 0
/* I think these are no longer used - CP */
int Gal_RSGetLength(Gal_Frame fr)
{
  Gal_Object to;
  if(fr == NULL) return(0);
  to = Gal_GetObject(fr,":values");
  return(Gal_ListLength(to));
}

Gal_Frame Gal_MakeResultSet(char *name, Gal_Object *tlist, int n)
{
  Gal_Frame res;

  res = Gal_MakeFrame(name, GAL_CLAUSE);
  Gal_SetProp(res,":nfound",Gal_IntObject(n));
  Gal_SetProp(res,":values",Gal_ListObject(tlist,n));
  return(res);
}

Gal_Frame Gal_RsGetElt(Gal_Frame fr, int n)
{
  Gal_Object to;
  if(fr == NULL) return(NULL);
  to = Gal_GetObject(fr,":values");
  if(to == NULL) return(NULL);
  return(Gal_FrameValue(Gal_ListElt(to,n)));
}
#endif

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
