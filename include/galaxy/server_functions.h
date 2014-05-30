/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _SERVER_FUNCTIONS_H
#define _SERVER_FUNCTIONS_H

#include "galaxy/generic-server.h"

Gal_DispatchFnPkg *_GalSS_InitializeSignatures(GalIO_ServerStruct *gcomm, Gal_DispatchFnPkg *s);
void _GalSS_InitializeDefaults(GalIO_ServerStruct *s);
extern int _GalIO_ServerIsInitializationPrototype(GalIO_ServerStruct *scomm);

#ifdef SERVER_FUNCTIONS_INCLUDE

/* define the server name, default port, and function prototypes */
#define _ ,
#define GAL_SERVER_NAME(__name__) static char *ServerName = #__name__;
#define GAL_SERVER_HOST(__host__)
#define GAL_SERVER_PORT(__port__) static unsigned short DefaultPort = __port__;
#define GAL_SERVER_DISPATCH_PORT(__port__) static unsigned short DefaultPort = __port__;
#define GAL_SERVER_OP(__op__) Gal_Frame __op__(Gal_Frame, void *);
#define GAL_SERVER_OP_SIGNATURE(__op__, __in_array__, __other_in_keys__, __reply__, __out_array__, __other_out_keys__) Gal_Frame __op__(Gal_Frame, void *);
#define GAL_SERVER_SW_PACKAGE(__name__) extern void _GalSS_configure_##__name__(GalIO_ServerStruct *scomm);
#define GAL_SERVER_MAX_CONNECTIONS(__conns__)
#include SERVER_FUNCTIONS_INCLUDE
#undef GAL_SERVER_NAME
#undef GAL_SERVER_HOST
#undef GAL_SERVER_PORT
#undef GAL_SERVER_DISPATCH_PORT
#undef GAL_SERVER_OP
#undef GAL_SERVER_OP_SIGNATURE
#undef GAL_SERVER_OP_KEYS
#undef GAL_SERVER_SW_PACKAGE
#undef GAL_SERVER_MAX_CONNECTIONS

/* define the s/w packages to load and the function map. */

#define GAL_SERVER_NAME(__name__)
#define GAL_SERVER_HOST(__host__)
#define GAL_SERVER_PORT(__port__)
#define GAL_SERVER_DISPATCH_PORT(__port__)
#define GAL_SERVER_SW_PACKAGE(__name__) _GalSS_configure_##__name__(scomm);
#define GAL_SERVER_MAX_CONNECTIONS(__conns__)
#define GAL_SERVER_OP_KEYS(__keys__) __keys__
#define GAL_SERVER_OP(__op__) { s = Gal_AddDispatchFunctionEntry(s, #__op__, __op__, NULL, GAL_OTHER_KEYS_MAYBE, GAL_REPLY_UNKNOWN, NULL, GAL_OTHER_KEYS_MAYBE); }
#define GAL_SERVER_OP_SIGNATURE(__op__, __in_array__, __other_in_keys__, __reply__, __out_array__, __other_out_keys__) { Gal_DispatchFnSignatureKeyEntry *in_keys = Gal_CreateDispatchFnKeyArray(0, __in_array__, NULL); Gal_DispatchFnSignatureKeyEntry *out_keys = Gal_CreateDispatchFnKeyArray(0, __out_array__, NULL); s = Gal_AddDispatchFunctionEntry(s, #__op__, __op__, in_keys, __other_in_keys__, __reply__, out_keys, __other_out_keys__); Gal_FreeDispatchFnKeyArray(in_keys); Gal_FreeDispatchFnKeyArray(out_keys); }
Gal_DispatchFnPkg *_GalSS_InitializeSignatures(GalIO_ServerStruct *scomm, Gal_DispatchFnPkg *s)
{
  if ((!scomm) || (!_GalIO_ServerIsInitializationPrototype(scomm))) {
#include SERVER_FUNCTIONS_INCLUDE
    return s;
  } else {
    return (Gal_DispatchFnPkg *) NULL;
  }
}
#undef GAL_SERVER_NAME
#undef GAL_SERVER_HOST
#undef GAL_SERVER_PORT
#undef GAL_SERVER_DISPATCH_PORT
#undef GAL_SERVER_OP
#undef GAL_SERVER_OP_SIGNATURE
#undef GAL_SERVER_OP_KEYS
#undef GAL_SERVER_SW_PACKAGE
#undef GAL_SERVER_MAX_CONNECTIONS
#undef _

/* SAM 10/3/99: I want to be able to use this in a context where there are no servers. */

#ifndef GAL_SERVER_OPS_ONLY

#define _ ,
#define GAL_SERVER_NAME(__name__)
#define GAL_SERVER_HOST(__host__)
#define GAL_SERVER_PORT(__port__)
#define GAL_SERVER_DISPATCH_PORT(__port__)
#define GAL_SERVER_OP(__op__) 
#define GAL_SERVER_OP_SIGNATURE(__op__, __in_array__, __other_in_keys__, __reply__, __out_array__, __other_out_keys__)
#define GAL_SERVER_SW_PACKAGE(__name__)
#define GAL_SERVER_MAX_CONNECTIONS(__conns__) GalIO_SetServerMaxConnections(s, __conns__);

/* SAM 7/2/02: I really need to grab the default information
   before the server is defined, in order to retrieve the
   server name to possibly look up entries in a server
   locations file (all this should ideally happen before
   the server is ever defined). I could add another function,
   but then if you don't use the headers, you'd have to
   define another function, and I'd need to add another
   file to the library so that I wouldn't get linker clashes
   (because if you import one function from a file, you
   import them all, and I might want one without the other).
   So what can I do which will work both in the headers
   and in the headerless case? Well, I can allocate a "fake"
   server struct which will serve to retrieve only
   the information requested. This means that ANYTHING IN
   THE SCOPE OF THIS FUNCTION MUST CHECK FOR WHETHER THIS
   "FAKE" SERVER STRUCT IS PRESENT. */
   
void _GalSS_InitializeDefaults(GalIO_ServerStruct *s)
{
  GalSS_InitializeServerDefaults(s, ServerName, DefaultPort);
  if (s && !_GalIO_ServerIsInitializationPrototype(s)) {
#include SERVER_FUNCTIONS_INCLUDE
    GalIO_SetServerDispatchFnPkg(s, _GalSS_InitializeSignatures(s, GalIO_GetServerDispatchFnPkg(s)));
  }
}

#undef GAL_SERVER_NAME
#undef GAL_SERVER_HOST
#undef GAL_SERVER_PORT
#undef GAL_SERVER_DISPATCH_PORT
#undef GAL_SERVER_OP
#undef GAL_SERVER_OP_SIGNATURE
#undef GAL_SERVER_OP_KEYS
#undef GAL_SERVER_SW_PACKAGE
#undef GAL_SERVER_MAX_CONNECTIONS
#undef _

#endif /* GAL_SERVER_OPS_ONLY */

#else /* #ifdef SERVER_FUNCTIONS_INCLUDE */

void _GalSS_InitializeDefaults(GalIO_ServerStruct *s)
{
  GalUtil_Fatal("Unable to initialize server defaults!");
}

#endif /* #ifdef SERVER_FUNCTIONS_INCLUDE */

#endif /* #ifndef _SERVER_FUNCTIONS_H */
