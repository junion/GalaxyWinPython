/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy.h"

/* Wrappers for dialogue.c moved to server_name_barrier.c */

/* NO Wrappers for grovel.c */

/* No Wrappers for nfio.c */

/* Wrappers for nframe.c */

Gal_Frame fr_getspred(Gal_Frame fr, const char *key)
{
  return(Gal_GetPredByName(fr, key));
}

Gal_Frame copy_nframe(Gal_Frame fr)
{
  return(Gal_CopyFrame(fr));
}

Gal_Object fr_remipred(Gal_Frame fr, int i)
{
  return(Gal_RemPred(fr, i));
}

Gal_Object fr_getsprop(Gal_Frame fr, const char *key)
{
  return(Gal_GetObject(fr, key));
}

Gal_Object Gal_GetProp(Gal_Frame fr, const char *key)
{
  return(Gal_GetObject(fr, key));
}

Gal_Object fr_remsprop(Gal_Frame fr, const char *key)
{
  return(Gal_RemProp(fr, key));
}

Gal_Object fr_remspred(Gal_Frame fr, const char *key)
{
  return(Gal_RemPredByName(fr, key));
}

Gal_Object fr_setsprop(Gal_Frame fr, const char *key, Gal_Object val)
{
  return(Gal_SetProp(fr, key, val));
}

int fr_qsetp(Gal_Frame fr)
{
  return Gal_TopicFramep(fr);
}

int fr_clausep(Gal_Frame fr)
{
  return Gal_ClauseFramep(fr);
}

int fr_predp(Gal_Frame fr)
{
  return Gal_PredFramep(fr);
}

int fr_npred(Gal_Frame fr)
{
  return(Gal_NumPreds(fr));
}

int fr_equal(Gal_Frame sf, Gal_Frame sem)
{
  return(Gal_FrameEqual(sf, sem));
}

int match_nframe(Gal_Frame sf, Gal_Frame sem)
{
  return(Gal_MatchFrame(sf, sem));
}

void free_nframe(Gal_Frame fr)
{
  Gal_FreeFrame(fr);
}

/* Wrappers for pr_util.c */

void pp_nframe(Gal_Frame fr)
{
  Gal_PPFrame(fr);
}

void pr_nframe(Gal_Frame fr)
{
  Gal_PrFrame(fr);
}

void pptf_nframe(Gal_Frame fr, FILE *fp)
{
  Gal_PPFrameToFile(fr, fp);
}

void prtf_nframe(Gal_Frame fr, FILE *fp)
{
  Gal_PrFrameToFile(fr, fp);
}

void outline_nframe(Gal_Frame fr)
{
  Gal_OutlineFrame(fr, GAL_PINFO1_LEVEL);
}

void pr_tobj(Gal_Object to)
{
  Gal_PrObject(to);
}

/* NO Wrappers for sym.c */

/* NO Wrappers for timed_tasks.c */

/* Wrappers for tobj.c */

Gal_Object copy_tobj(Gal_Object to)
{
  return(Gal_CopyObject(to));
}

Gal_Object make_tfloat(float val)
{
  return(Gal_FloatObject(val));
}

Gal_Object make_tframe(Gal_Frame val)
{
  return(Gal_FrameObject(val));
}

Gal_Object make_tint(int val)
{
  return(Gal_IntObject(val));
}

Gal_Object make_tlist(Gal_Object *values, int n)
{
  return(Gal_ListObject(values, n));
}

Gal_Object make_tptr(void *val)
{
  return(Gal_PointerObject(val));
}

Gal_Object make_tstring(const char *val)
{
  return(Gal_StringObject(val));
}

void free_tobj(Gal_Object to)
{
  Gal_FreeObject(to);
}

/* Wrappers for domain.c moved to server_name_barrier.c */

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
