/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>

#include "gal_internal.h"

static struct avpair *alloc_avpair()
{
  struct avpair *ap;
  ap = (struct avpair *)calloc(1,sizeof(struct avpair));
  return(ap);
}

/* does not free value storage  */
void _gal_free_avpair(Av ap)
{
  free(ap);
}

/* Called for freeing value storage  */
void *_gal_av_val(Av ap)
{
  return(ap->val);
}

void *_gal_av_att(Av ap)
{
  return(ap->att);
}


/*
 * PLIST routines
 * a PLIST is basically a VLIST of avpairs; 
 *   supports 
 *     _gal_getprop(plist,key)
 *     _gal_setprop(plist,key,val)   ; created entry if none, else replace value
 */

Plist _gal_alloc_plist(void)
{
  return((Plist)_gal_alloc_vlist());
}

void *_gal_getprop(Plist plist, void *key)
{
  int i;
  int n;
  struct avpair **vdata;
  if(plist == NULL) return(NULL);
  vdata = (struct avpair **)_gal_get_vdata(plist,&n);
  for(i=0;i<n;i++){
    if(vdata[i] == NULL) continue;
    if(vdata[i]->att == key) return(vdata[i]->val);
  }
  return(NULL);
}

/*
 * Like _gal_getprop, but removes att-value pair from frame.
 *  -does not GC storage for value -- value Gal_Object is returned
 *    to the user who must free it
 */
void *_gal_remprop(Plist plist, void *key)
{
  int i;
  int n;
  struct avpair **vdata;
  void *val = NULL;
  if(plist == NULL) return(NULL);
  vdata = (struct avpair **)_gal_get_vdata(plist,&n);
  for(i=0;i<n;i++){
    if(vdata[i] == NULL) continue;
    if(vdata[i]->att == key){
      val = vdata[i]->val;
      /*
       * free avpair and stomp ptr
       *  hopefully every skips over NULL ptrs
       */
      _gal_free_avpair(vdata[i]);
      vdata[i] = NULL;
      return(val);
    }
  }
  return(NULL);
}

void *_gal_setprop(Plist plist, void *key, void *value)
{
  int i;
  int n;
  struct avpair **vdata;
  struct avpair *ap;
  if(plist == NULL) return(NULL);
  vdata = (struct avpair **)_gal_get_vdata(plist,&n);
  for(i=0;i<n;i++){
    if(vdata[i] == NULL) continue;
    if(vdata[i]->att == key){
      vdata[i]->val = value;
      return(vdata[i]->val);      
    }
  }
  ap = alloc_avpair();
  ap->att = key;
  ap->val = value;
  _gal_add_vdata(plist,ap);
  return(value);
}

int _gal_get_num_atts(Plist plist, int count_null_values)
{
  int num_atts = 0;
  int n, i;
  
  struct avpair **vdata = (struct avpair **)_gal_get_vdata(plist,&n);
  if (n) {
    for (i = 0; i < n; i++) {
      if (vdata[i] == NULL) continue;
      if ((!count_null_values) && (_gal_av_val(vdata[i]) == NULL)) continue;
      num_atts++;
    }
  }
  return num_atts;
}

void **_gal_get_atts(Plist plist, int *num_atts)
{
  int i, n, natts = 0;
  void **atts = NULL;
  struct avpair **vdata;

  if (num_atts)
    *num_atts = 0;
  else
    return(NULL);

  if(plist == NULL) return(NULL);
  vdata = (struct avpair **)_gal_get_vdata(plist,&n);
  if (n)
  {
    atts = (void **)calloc(n, sizeof(void *));
    for(i=0; i<n; i++)
    {
      if(vdata[i] == NULL) continue;
      atts[natts++] = vdata[i]->att;
    }
  }
  if (natts)
  {
    *num_atts = natts;
    return(atts);
  }
  if (atts)
    free(atts);
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
