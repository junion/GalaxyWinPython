/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* MITRE added this file to handle processing of message signatures
   and dispatch function tables. This file also provides support for
   encoding and decoding of message signatures in the transport
   between Hub and server. */

#include "galaxy/sysdep.h"
#include <stdarg.h>
#include "galaxy/galaxy.h"
#include "galaxy/util.h"
#include "galaxy/program.h"

/* SAM 10/2/99: This is a little more convoluted than one might think
   it needs to be. Instead of having a single array with signature
   and functions, I'm going to keep a parallel array of signatures
   and function mappings for two reasons: (1) I prefer not to
   alter the MIT structure GAL_SERVER_FUNCTION_MAP in case there
   are dependencies I'm not aware of, and (2) I need to pass
   signatures back and forth from Hub to server, and I don't need
   the function mappings. I'll take this opportunity to cache
   the encoding of the signature for sending to each server which
   attaches (be sure not to free it when you free the frame!).

   SAM 8/31/01: I finally convinced myself that I could extend the
   GAL_SERVER_FUNCTION_MAP function safely, and cleaned up a bunch
   of stuff in the context of adding the ability to override the
   invocation process. I'm still keeping the name in the
   signatures, because they still need to be free-standing. */

/* This function copies the key array. In server_functions.h, we pass in
   static memory, and I'd like to make sure that in case somebody frees
   something, nothing bad will happen. The array is terminated by
   a null key slot. See __Gal_SetMapCell. */

Gal_DispatchFnSignatureKeyEntry *Gal_CopyDispatchFnKeyArray(Gal_DispatchFnSignatureKeyEntry *array)
{
  int i, j;
  Gal_DispatchFnSignatureKeyEntry *new_entry;

  if (!array)
    return (Gal_DispatchFnSignatureKeyEntry *) NULL;

  /* First we compute the length. */
  for (i = 0; array[i].key; i++);
  new_entry = (Gal_DispatchFnSignatureKeyEntry *) calloc(i + 1, sizeof(Gal_DispatchFnSignatureKeyEntry));
  for (j = 0; j < i; j++) {    
    new_entry[j].key = _gal_strdup(array[j].key);
    new_entry[j].value_type = array[j].value_type;
    new_entry[j].obligatory = array[j].obligatory;
  }
  return new_entry;
}

/* I ignore the first argument. I include it because stdarg requires the name
   of the last argument that we know the type of. */

/* SAM 6/8/00: I need to extend the API here in order to
   do this cleanly for embedded scripting languages. */

Gal_DispatchFnSignatureKeyEntry *_Gal_CreateEmptyDispatchFnKeyArray(int i)
{
  if (i == 0) {
    return (Gal_DispatchFnSignatureKeyEntry *) NULL;
  } else {
    return (Gal_DispatchFnSignatureKeyEntry *) calloc(i+1, sizeof(Gal_DispatchFnSignatureKeyEntry));
  }
}

void _Gal_PopulateDispatchFnKeyArrayCell(Gal_DispatchFnSignatureKeyEntry *array, int index,
					 const char *key, Gal_ObjectType t,
					 int obligatory)
{
  array[index].key = _gal_strdup(key);
  array[index].value_type = t;
  array[index].obligatory = obligatory;
}

Gal_DispatchFnSignatureKeyEntry *Gal_CreateDispatchFnKeyArray(int ignore, ...)
{
  va_list args;
  int i = 0, j;
  char *key;
  Gal_ObjectType value_type;
  int obligatory;
  Gal_DispatchFnSignatureKeyEntry *new_entry;

  va_start(args, ignore);
  /* I will go through and count first. */
  while (1) {
    key = va_arg(args, char *);
    if (!key) break;
    i++;
    value_type = va_arg(args, Gal_ObjectType);
    obligatory = va_arg(args, int);
  }
  va_end(args);
  new_entry = _Gal_CreateEmptyDispatchFnKeyArray(i);
  va_start(args, ignore);
  for (j = 0; j < i; j++) {
    key = va_arg(args, char *);
    value_type = va_arg(args, Gal_ObjectType);
    obligatory = va_arg(args, int);    
    _Gal_PopulateDispatchFnKeyArrayCell(new_entry, j,
					key, value_type, obligatory);
  }
  return new_entry;
}

/* Free the key array. Anyone who calls create should also call free. */

void Gal_FreeDispatchFnKeyArray(Gal_DispatchFnSignatureKeyEntry *entry)
{
  int i = 0;

  if (!entry) return;
  while (1) {
    if (!entry[i].key) break;
    free(entry[i++].key);
  }
  free(entry);
}

void Gal_FreeDispatchFnSignature(Gal_DispatchFnSignature *sigs)
{
  int i = 0;
  if (!sigs) return;
  while (1) {
    if (!sigs[i].name) break;
    free(sigs[i].name);
    Gal_FreeDispatchFnKeyArray(sigs[i].in_key_array);
    Gal_FreeDispatchFnKeyArray(sigs[i].out_key_array);
    i++;
  }
  free(sigs);
}

static void __Gal_SetSigCell(Gal_DispatchFnSignature *sig,
			     const char *name,
			     Gal_DispatchFnSignatureKeyEntry *in_key_array,
			     int allow_other_in_keys, 
			     int reply_provided,
			     Gal_DispatchFnSignatureKeyEntry *out_key_array,
			     int allow_other_out_keys)
{
  if (name) {
    sig->name = _gal_strdup(name);
  } else {
    sig->name = (char *) NULL;
  }
  sig->in_key_array = Gal_CopyDispatchFnKeyArray(in_key_array);
  sig->allow_other_in_keys = allow_other_in_keys;  
  sig->reply_provided = reply_provided;
  sig->out_key_array = Gal_CopyDispatchFnKeyArray(out_key_array);
  sig->allow_other_out_keys = allow_other_out_keys;
}

static void __Gal_SetMapCell(Gal_DispatchFnPkg *table,
			     int idx, const char *name,
			     Gal_FrameDataFnPtr fn_with_data,
			     Gal_DispatchFnSignatureKeyEntry *in_key_array,
			     int allow_other_in_keys, 
			     int reply_provided,
			     Gal_DispatchFnSignatureKeyEntry *out_key_array,
			     int allow_other_out_keys,
			     void *client_data)
{
  
  if (name) {
    table->fn_map[idx].name = _gal_strdup(name);
  } else {
    table->fn_map[idx].name = (char *) NULL;
  }
  __Gal_SetSigCell(&(table->fn_map[idx].sig), name,
		   in_key_array, allow_other_in_keys,
		   reply_provided, out_key_array,
		   allow_other_out_keys);
  table->fn_map[idx].fn_with_data = fn_with_data;
  table->fn_map[idx].client_data = client_data;
}

/* This function adds a single entry to the table. We start off with a new
   dispatch fn pkg, which we will extend if necessary. */

#define SIGNATURE_INCREMENT 10

static int
__gal_default_dispatch_function_selector(Gal_DispatchFnInvocation *i);

static Gal_Frame
__gal_default_dispatch_function_invoker(Gal_DispatchFnInvocation *i);

static Gal_DispatchFnSignature *
__gal_default_dispatch_fn_signature_lister(Gal_DispatchFnPkg *pkg);

static Gal_DispatchFnPkg *__Gal_NewDispatchFnPkg()
{
  Gal_DispatchFnPkg *new_pkg = (Gal_DispatchFnPkg *) malloc(sizeof(Gal_DispatchFnPkg));

  new_pkg->fn_map = (GAL_SERVER_FUNCTION_MAP *) calloc(SIGNATURE_INCREMENT, sizeof(GAL_SERVER_FUNCTION_MAP));
  new_pkg->num_entries = 0;
  new_pkg->allocated = SIGNATURE_INCREMENT;
  new_pkg->encoded_signature = (Gal_Object) NULL;
  new_pkg->selector = __gal_default_dispatch_function_selector;
  new_pkg->invoker = __gal_default_dispatch_function_invoker;
  new_pkg->lister = __gal_default_dispatch_fn_signature_lister;
  new_pkg->client_data = (void *) NULL;
  return new_pkg;
}

/* We always try to extend by one. To ensure backward compatibility with the
   previous use of GAL_SERVER_FUNCTION_MAP, we should always ensure that there
   is room for two more: the new entry and a NULL element at the end. */

static void __Gal_ExtendDispatchFnPkg(Gal_DispatchFnPkg *new_pkg)
{
  if ((new_pkg->num_entries + 2) > new_pkg->allocated) {
    new_pkg->fn_map = (GAL_SERVER_FUNCTION_MAP *) realloc(new_pkg->fn_map, (new_pkg->allocated + SIGNATURE_INCREMENT) * sizeof(GAL_SERVER_FUNCTION_MAP));
    new_pkg->allocated += SIGNATURE_INCREMENT;
  }
}
  
void _Gal_FreeDispatchFnPkg(Gal_DispatchFnPkg *pkg)
{
  int i;
  
  if (pkg) {
    for (i = 0; i < pkg->num_entries; i++) {
      if (pkg->fn_map) {
	if (pkg->fn_map[i].name)
	  free(pkg->fn_map[i].name);
	if (pkg->fn_map[i].sig.name)
	  free(pkg->fn_map[i].sig.name);
	if (pkg->fn_map[i].sig.in_key_array)
	  Gal_FreeDispatchFnKeyArray(pkg->fn_map[i].sig.in_key_array);
	if (pkg->fn_map[i].sig.out_key_array)
	  Gal_FreeDispatchFnKeyArray(pkg->fn_map[i].sig.out_key_array);
      }
    }
    if (pkg->encoded_signature) {
      Gal_FreeObject(pkg->encoded_signature);
    }
    if (pkg->fn_map)
      free(pkg->fn_map);
    free(pkg);
  }
}

Gal_DispatchFnPkg *Gal_AddDispatchFunctionEntry(Gal_DispatchFnPkg *table,
						const char *name,
						Gal_FrameDataFnPtr fn_with_data,
						Gal_DispatchFnSignatureKeyEntry *in_key_array,
						int allow_other_in_keys, 
						int reply_provided,
						Gal_DispatchFnSignatureKeyEntry *out_key_array,
						int allow_other_out_keys)
{
  if (table == (Gal_DispatchFnPkg *) NULL) {
    table = __Gal_NewDispatchFnPkg();
  }
  
  /* Make sure there is enough room. */
  __Gal_ExtendDispatchFnPkg(table);
  
  /* Now, we need to place the new data in the fn map and signature, and NULL
     out the following entry. */

  __Gal_SetMapCell(table, table->num_entries, name, fn_with_data,
		   in_key_array, allow_other_in_keys,
		   reply_provided, out_key_array, allow_other_out_keys,
		   (void *) NULL);

  __Gal_SetMapCell(table, table->num_entries + 1,
		   (char *) NULL, (Gal_FrameDataFnPtr) NULL,
		   (Gal_DispatchFnSignatureKeyEntry *) NULL, 0, 0,
		   (Gal_DispatchFnSignatureKeyEntry *) NULL, 0,
		   (void *) NULL);
  table->num_entries++;
  return table;
}

/* And this is for standalone dispatch function signatures. */

Gal_DispatchFnSignature *
Gal_CreateDispatchFnSignature(const char *name,
			      Gal_DispatchFnSignatureKeyEntry *in_key_array,
			      int allow_other_in_keys, 
			      int reply_provided,
			      Gal_DispatchFnSignatureKeyEntry *out_key_array,
			      int allow_other_out_keys)
{
  Gal_DispatchFnSignature *sig = (Gal_DispatchFnSignature *) calloc(1, sizeof(Gal_DispatchFnSignature));

  __Gal_SetSigCell(sig, name, in_key_array, allow_other_in_keys,
		   reply_provided, out_key_array, allow_other_out_keys);
  return sig;
}  

/* SAM 6/8/00: I realized that client_data is the only way to
   record the callbacks when embedding in a scripting language, and
   I also realized that I hadn't provided that functionality.
   Really, all the dispatch function signatures should be redone
   to handle that, but I'm not going to do that right now - just
   give people the facility to set and retrieve it. There are
   enough places where the name is recorded; I'm just going to
   piggyback off the function map. No error is generated if
   there's no place to put it.
*/

void Gal_DispatchFnPkgSetClientData(Gal_DispatchFnPkg *pkg,
				    const char *op_name,
				    void *data)
{
  int i;

  GAL_SERVER_FUNCTION_MAP *fn_map;

  if (!pkg) {
    return;
  }
  
  fn_map = pkg->fn_map;

  if (!fn_map) {
    return;
  }
  for(i=0;;i++) {
    if (!fn_map[i].name) break;
    
    if (!strcmp(fn_map[i].name, op_name)) {
      fn_map[i].client_data = data;
      break;
    }
  }
}

void *Gal_DispatchFnPkgGetClientData(Gal_DispatchFnPkg *pkg,
				     const char *op_name)
{
  int i;

  GAL_SERVER_FUNCTION_MAP *fn_map;

  if (!pkg) {
    return (void *) NULL;
  }
  
  fn_map = pkg->fn_map;

  if (!fn_map) {
    return (void *) NULL;
  }
  for(i=0;;i++) {
    if (!fn_map[i].name) break;
    
    if (!strcmp(fn_map[i].name, op_name)) {
      return fn_map[i].client_data;
    }
  }
  return (void *) NULL;
}

/* This function supports searching through the table in order to
   find a function to execute. */

/* SAM 6/8/00: In the process of writing embedding code, I
   discovered that under some bizarre circumstances (if, for
   instance, you create a server which hasn't had any
   dispatch functions declared), pkg may be NULL. I'd better
   check for that case. */

/* SAM 8/31/01: Finally decided it would be a good thing
   to provide some control over the dispatch function
   selection process. The bindings could register their
   dispatch functions differently if they chose, or either
   the bindings or C could override the selection (if, say,
   the server wanted to capture everything, for logging
   or perhaps providing a GUI stub to give the user complete
   interactive control over the dispatch function behavior.
   The normal programmer ain't gonna be using this feature,
   obviously... */

static int
__gal_default_dispatch_function_selector(Gal_DispatchFnInvocation *inv)
{
  int i;
  char *op_name = inv->bare_op_name;

  GAL_SERVER_FUNCTION_MAP *fn_map;
  
  fn_map = inv->pkg->fn_map;

  if (!fn_map) {
    return 0;
  }
  for(i=0;;i++) {
    if (!fn_map[i].name) break;

    if (!strcmp(fn_map[i].name, op_name)) {
      inv->call_client_data = (void *) &fn_map[i];
      inv->sig = &(fn_map[i].sig);
      return 1;
    }
  }
  return 0;
}

int
Gal_FindDispatchFunctionEntry(Gal_DispatchFnInvocation *invocation)
{
  if (!invocation) {
    return 0;
  }

  if (!invocation->pkg) {
    return 0;
  }
  
  if (invocation->pkg->selector) {
    return (*invocation->pkg->selector)(invocation);
  } else {
    return 0;
  }
}

/* We need to pair invocation with selection. So they
   both need to be configurable. */

static Gal_Frame
__gal_default_dispatch_function_invoker(Gal_DispatchFnInvocation *i)
{
  GAL_SERVER_FUNCTION_MAP *fn_entry = (GAL_SERVER_FUNCTION_MAP *) i->call_client_data;
  
  if (fn_entry->fn_with_data) {
    GalUtil_PInfo1("Invoking dispatch function: %s\n", i->bare_op_name);
    return (*fn_entry->fn_with_data)(i->frame, i->env);
  } else {
    return (Gal_Frame) NULL;
  }
}

Gal_Frame _Gal_InvokeDispatchFn(Gal_DispatchFnInvocation *i)
{
  return (*i->pkg->invoker)(i);
}

static Gal_DispatchFnSignature *
__gal_default_dispatch_fn_signature_lister(Gal_DispatchFnPkg *pkg)
{
  int i, j;
  Gal_DispatchFnSignature *sigs;
  GAL_SERVER_FUNCTION_MAP *fn_map;
  
  if (!pkg)
    return (Gal_DispatchFnSignature *) NULL;

  fn_map = pkg->fn_map;
  for (i = 0; fn_map[i].name; i++);
  if (i == 0)
    return (Gal_DispatchFnSignature *) NULL;
  sigs = (Gal_DispatchFnSignature *) calloc(i + 1,
					    sizeof(Gal_DispatchFnSignature));
  for (j = 0; j < i; j++) {
    sigs[j] = fn_map[j].sig;
  }
  return sigs;  
}

/* This will always be freshly malloc'ed. */

Gal_DispatchFnSignature *
_Gal_ListDispatchFnSignatures(Gal_DispatchFnPkg *pkg)
{
  return (*pkg->lister)(pkg);
}  

Gal_DispatchFnPkg *_Gal_DispatchFnPkgSetAccess(Gal_DispatchFnPkg *pkg,
					       Gal_DispatchFunctionSelector s,
					       Gal_DispatchFnSignatureLister l,
					       Gal_DispatchFunctionInvoker i,
					       void *invocation_client_data)
{
  if (pkg == (Gal_DispatchFnPkg *) NULL) {
    pkg = __Gal_NewDispatchFnPkg();
  }
  
  pkg->selector = s;
  pkg->invoker = i;
  pkg->lister = l;
  pkg->client_data = invocation_client_data;

  return pkg;
}

/* These functions support encoding and decoding of the signatures.
   The encoding will be as compact as possible; it will consist of
   nested list objects. */

static Gal_Object __Gal_EncodeDispatchFnKeyArray(Gal_DispatchFnSignatureKeyEntry *array, Gal_Object **tmp_array_ptr, int *tmp_array_size)
{
  int i, j;
  Gal_Object *tmp_array;
  
  if (!array)
    return Gal_ListObject(NULL, 0);
  /* Count them up. */
  for (i = 0; array[i].key; i++);
  if (i == 0)
    return Gal_ListObject(NULL, 0);
  /* Make sure our temp buffer is big enough. */
  if (i > *tmp_array_size) {
    *tmp_array_size = i + SIGNATURE_INCREMENT;
    if (*tmp_array_ptr) {
      *tmp_array_ptr = (Gal_Object *) realloc(*tmp_array_ptr, (i + SIGNATURE_INCREMENT) * sizeof(Gal_Object));
    } else {
      *tmp_array_ptr = (Gal_Object *) malloc((i + SIGNATURE_INCREMENT) * sizeof(Gal_Object));
    }
  }
  tmp_array = *tmp_array_ptr;
  for (j = 0; j < i; j++) {
    tmp_array[j] = Gal_ListObjectFromElements(3, Gal_StringObject(array[j].key),
					      Gal_IntObject(array[j].value_type),
					      Gal_IntObject(array[j].obligatory));
  }
  return Gal_ListObject(tmp_array, i);
}  

/* A list of 6-tuples, (name, in_keys, other_keys_allowed, response_provided,
   out_keys, out_keys_allowed), where in_keys and out_keys are lists of 3-tuples
   (key, value_type, obligatory). */

Gal_Object Gal_EncodeDispatchFnSignatures(Gal_DispatchFnPkg *pkg)
{
  int i, j;
  Gal_Object *tmp_key_array = (Gal_Object *) NULL;
  int tmp_key_size = 0;
  Gal_Object *tmp_array = (Gal_Object *) NULL;
  Gal_Object res;
  Gal_DispatchFnSignature *sigs;
  
  if (!pkg)
    return Gal_ListObject(NULL, 0);
  /* This will always be freshly malloc'ed. */
  sigs = _Gal_ListDispatchFnSignatures(pkg);
  if (!sigs)
    return Gal_ListObject(NULL, 0);
  /* Count the length of the list. */
  for (i = 0; sigs[i].name; i++);
  if (i == 0)
    return Gal_ListObject(NULL, 0);
  /* Build the requisite list objects for each map. */
  tmp_array = (Gal_Object *) malloc(i * sizeof(Gal_Object));
  for (j = 0; j < i; j++) {
    tmp_array[j] = Gal_ListObjectFromElements(6, Gal_StringObject(sigs[j].name),
					      __Gal_EncodeDispatchFnKeyArray(sigs[j].in_key_array, &tmp_key_array, &tmp_key_size),
					      Gal_IntObject(sigs[j].allow_other_in_keys),
					      Gal_IntObject(sigs[j].reply_provided),
					      __Gal_EncodeDispatchFnKeyArray(sigs[j].out_key_array, &tmp_key_array, &tmp_key_size),
					      Gal_IntObject(sigs[j].allow_other_out_keys));
  }
  res = Gal_ListObject(tmp_array, j);
  if (tmp_array)
    free(tmp_array);
  if (tmp_key_array)
    free(tmp_key_array);
  if (sigs)
    free(sigs);
  return res;
}

/* Remember, remove this object from the frame before you free it
   if you ship the encoding! */

/* SAM 6/8/00: In the process of writing embedding code, I
   discovered that under some bizarre circumstances (if, for
   instance, you create a server which hasn't had any
   dispatch functions declared), pkg may be NULL. I'd better
   check for that case. */

Gal_Object Gal_EncodeDispatchFnPkgSigs(Gal_DispatchFnPkg *pkg)
{
  if (!pkg) {
    return Gal_EncodeDispatchFnSignatures(pkg);
  } else if (pkg->encoded_signature) {
    return pkg->encoded_signature;
  } else {
    Gal_Object new_obj = Gal_EncodeDispatchFnSignatures(pkg);
    pkg->encoded_signature = new_obj;
    return new_obj;
  }
}

static int __Gal_DecodeDispatchFnSignatureKeyArray(Gal_Object maybe_list, Gal_DispatchFnSignatureKeyEntry **res_ptr)
{
  int num_key_elements, num_keys = 0, i;
  Gal_Object key_object;
  Gal_Object *key_elements;
  Gal_Object *elements;
  Gal_DispatchFnSignatureKeyEntry *new_array = (Gal_DispatchFnSignatureKeyEntry *) NULL;
  
  if (!Gal_Listp(maybe_list)) {
    *res_ptr = (Gal_DispatchFnSignatureKeyEntry *) NULL;
    return 0;
  }
  elements = Gal_ListValue(maybe_list, &num_keys);
  new_array = (Gal_DispatchFnSignatureKeyEntry *) calloc(num_keys + 1, sizeof(Gal_DispatchFnSignatureKeyEntry));
  for (i = 0; i < num_keys; i++) {
    key_object = elements[i];
    if (!Gal_Listp(key_object)) {
      Gal_FreeDispatchFnKeyArray(new_array);
      *res_ptr = (Gal_DispatchFnSignatureKeyEntry *) NULL;
      return 0;
    }
    key_elements = Gal_ListValue(key_object, &num_key_elements);
    if (num_key_elements != 3) {
      Gal_FreeDispatchFnKeyArray(new_array);
      *res_ptr = (Gal_DispatchFnSignatureKeyEntry *) NULL;
      return 0;
    }
    /* Tuple is (name, type, obligatory). */
    if ((!Gal_Stringp(key_elements[0])) ||
	(!Gal_Intp(key_elements[1])) ||
	(!Gal_Intp(key_elements[2]))) {
      Gal_FreeDispatchFnKeyArray(new_array);
      *res_ptr = (Gal_DispatchFnSignatureKeyEntry *) NULL;
      return 0;
    } 
    new_array[i].key = _gal_strdup(Gal_StringValue(key_elements[0]));
    new_array[i].value_type = Gal_IntValue(key_elements[1]);
    new_array[i].obligatory = Gal_IntValue(key_elements[2]);
  }
  *res_ptr = new_array;
  return 1;
}
			      
Gal_DispatchFnSignature *Gal_DecodeDispatchFnSignatures(Gal_Object maybe_list)
{
  int num_signatures = 0, i;
  Gal_Object *elements;
  Gal_DispatchFnSignature *new_sig = (Gal_DispatchFnSignature *) NULL;
  Gal_Object sig_object;
  Gal_Object *sig_elements;
  int num_sig_elements;
  
  if (!Gal_Listp(maybe_list))
    return (Gal_DispatchFnSignature *) NULL;
  /* Each entry in the list is a signature. */
  elements = Gal_ListValue(maybe_list, &num_signatures);
  new_sig = (Gal_DispatchFnSignature *) calloc(num_signatures + 1, sizeof(Gal_DispatchFnSignature));
  for (i = 0; i < num_signatures; i++) {
    sig_object = elements[i];
    if (!Gal_Listp(maybe_list)) {
      Gal_FreeDispatchFnSignature(new_sig);
      return (Gal_DispatchFnSignature *) NULL;
    }
    sig_elements = Gal_ListValue(sig_object, &num_sig_elements);
    if (num_sig_elements != 6) {
      Gal_FreeDispatchFnSignature(new_sig);
      return (Gal_DispatchFnSignature *) NULL;
    }
    /* Next, we try to decode the keys (the 1th and 4th elements of the list). If
       the decode fails for either, then we fail the whole thing. */
    if (!__Gal_DecodeDispatchFnSignatureKeyArray(sig_elements[1], &(new_sig[i].in_key_array))) {
      Gal_FreeDispatchFnSignature(new_sig);
      return (Gal_DispatchFnSignature *) NULL;
    }
    if (!__Gal_DecodeDispatchFnSignatureKeyArray(sig_elements[4], &(new_sig[i].out_key_array))) {
      Gal_FreeDispatchFnSignature(new_sig);
      return (Gal_DispatchFnSignature *) NULL;
    }
    /* Now, we do the rest of the work. */
    if ((!Gal_Stringp(sig_elements[0])) ||
	(!Gal_Intp(sig_elements[2])) ||
	(!Gal_Intp(sig_elements[3])) ||
	(!Gal_Intp(sig_elements[5]))) {
      Gal_FreeDispatchFnSignature(new_sig);
      return (Gal_DispatchFnSignature *) NULL;
    } 
    new_sig[i].name = _gal_strdup(Gal_StringValue(sig_elements[0]));
    new_sig[i].allow_other_in_keys = Gal_IntValue(sig_elements[2]);
    new_sig[i].reply_provided = Gal_IntValue(sig_elements[3]);
    new_sig[i].allow_other_out_keys = Gal_IntValue(sig_elements[5]);
  }
  return new_sig;
}
    
/* These functions validate a frame against a signature. */

static int __find_string_in_list(char *entry, char **list, int num_elts)
{
  /* If num_elts < 0, I'll search for a NULL. */
  int i = 0;

  if (!list) return -1;
  while (1) {
    if (((num_elts < 0) && !list[i]) ||
	((num_elts >= 0) && (num_elts == i)))
      return -1;
    if (!strcmp(entry, list[i]))
      return i;
    i++;
  }
}

/* This internal function loops through the key entries, keeping
   track of the keys which were checked. I'm not going to work
   too hard at making this efficient, since it's a debugging tool.

   We start by getting all the properties (don't forget to free this
   when we're done). We construct a parallel array of props which
   were checked. */

static void __Gal_ValidateFrame(const char *op_name, Gal_Frame frame,
				Gal_DispatchFnSignatureKeyEntry *array,
				int allow_other_keys,
				char *direction, char **exclusions)
{
  int num_props;
  char **props;
  int *checked;
  Gal_Object temp;
  int i = 0, j;
  
  props = Gal_GetProperties(frame, &num_props);
  checked = (int *) calloc(num_props, sizeof(int));

  /* So first, we check the array to enforce the properties. */
  if (array) {
    while (1) {
      /* Stop when there's an empty element. */
      if (!array[i].key) break;
      /* Otherwise, try to find the element. We might as well
	 check in the list first, since we will need to mark it anyway. */
      j = __find_string_in_list(array[i].key, props, num_props);
      if ((j < 0) && (array[i].obligatory == GAL_KEY_ALWAYS)) {
	  GalUtil_Warn("For %s: obligatory %s key %s not found",
		       op_name, direction, array[i].key);
      }
      if (j >= 0) {
	checked[j] = 1;
	temp = Gal_GetObject(frame, array[i].key);
	if ((array[i].value_type != GAL_FREE) &&
	    (Gal_GetObjectType(temp) != array[i].value_type)) {
	    GalUtil_Warn("For %s: %s key %s should be a %s, but is a %s\n",
			 op_name, direction, array[i].key,
			 Gal_ObjectTypeString(array[i].value_type),
			 Gal_ObjectTypeString(Gal_GetObjectType(temp)));
	}
      }
      i++;
    }
  }
  /* Next, we look at the unchecked properties and if other keys is not
     permitted, we report the ones which aren't excluded. */
  if (allow_other_keys == GAL_OTHER_KEYS_NEVER) {
    for (j = 0; j < num_props; j++) {
      if ((!checked[j]) &&
	  (__find_string_in_list(props[j], exclusions, -1) == -1)) {
	  GalUtil_Warn("For %s: %s key %s not permitted\n",
		       op_name, direction, props[j]);
      }
    }
  }
  if (props) free(props);
  if (checked) free(checked);
}

void Gal_ValidateDispatchFnInput(const char *op_name, Gal_Frame frame,
				 Gal_DispatchFnSignature *sig, char **exclusions)
{
  __Gal_ValidateFrame(op_name, frame, sig->in_key_array, sig->allow_other_in_keys,
		      "input", exclusions);
}

void Gal_ValidateDispatchFnOutput(const char *op_name, Gal_Frame frame,
				  Gal_DispatchFnSignature *sig, char **exclusions)
{
  switch (sig->reply_provided) {
  case GAL_REPLY_PROVIDED:
    if (!frame) {
      GalUtil_Warn("For  %s: expected frame reply but found NULL\n", op_name);
    } else {
      __Gal_ValidateFrame(op_name, frame, sig->out_key_array, sig->allow_other_out_keys, "reply", exclusions);
    }
    break;
  case GAL_REPLY_NONE:
    if (frame) {
      GalUtil_Warn("For %s: expected NULL reply but found frame\n", op_name);
    }
    break;
  case GAL_REPLY_UNKNOWN:
    __Gal_ValidateFrame(op_name, frame, sig->out_key_array, sig->allow_other_out_keys,
			"reply", exclusions);
    break;
  default:
    break;
  }  
}

/* For use in the Hub, where there are no dispatch function tables. */

Gal_DispatchFnSignature *Gal_FindNamedSignature(Gal_DispatchFnSignature *sigs,
						const char *name)
{
  /* the sigs list is always null terminated. */
  int i;
  char *copied_name;
  
  if (!sigs)
    return (Gal_DispatchFnSignature *) NULL;

  /* SAM 2/8/00: For consistency, Gal_SplitOperationName always
     returns new memory. */
  copied_name = Gal_SplitOperationName(name, NULL);
  
  for (i = 0; sigs[i].name; i++) {
    if (Gal_StringEq(sigs[i].name, copied_name)) {
      free(copied_name);
      return &sigs[i];
    }
  }
  free(copied_name);
  return (Gal_DispatchFnSignature *) NULL;
}
