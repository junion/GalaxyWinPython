/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _NAME_BARRIER_H
#define _NAME_BARRIER_H

/* wrappers defined in name_barrier.c */

/* Function protos for file nframe.c */
Nframe copy_nframe(Nframe fr);
Nframe fr_getspred(Nframe fr, const char *key);
TObj   fr_get_pred(Nframe fr, int i); /* not in name_barrier.c */
TObj   fr_add_pred(Nframe fr, TObj pred); /* not in name_barrier.c */
TObj   fr_remipred(Nframe fr, int i);
TObj   fr_getsprop(Nframe fr, const char *key);
TObj   fr_remsprop(Nframe fr, const char *key);
TObj   fr_remspred(Nframe fr, const char *key);
TObj   fr_setsprop(Nframe fr, const char *key, TObj val);
int    fr_qsetp(Nframe fr);
int    fr_clausep(Nframe fr);
int    fr_predp(Nframe fr);
int    fr_npred(Nframe fr);
int    fr_equal(Nframe sf, Nframe sem);
int    match_nframe(Nframe sf, Nframe sem);
void   free_nframe(Nframe fr);

/* Function protos for file pr_util.c */
void pp_nframe(Nframe fr);
void pr_nframe(Nframe fr);
void pptf_nframe(Nframe fr, FILE *fp);
void prtf_nframe(Nframe fr, FILE *fp);
void outline_nframe(Nframe fr);
void pr_tobj(TObj to);

/* Function protos for file tobj.c */
TObj  copy_tobj(TObj to);
TObj  make_tfloat(float val);
TObj  make_tframe(Nframe val);
TObj  make_tint(int val);
TObj  make_tlist(TObj *values, int n);
TObj  make_tptr(void *val);
TObj  make_tstring(const char *val);
void  free_tobj(TObj to);

#endif  /* _GALAXY_H */
