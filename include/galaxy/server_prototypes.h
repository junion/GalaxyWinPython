/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _SERVER_PROTOTYPES_H
#define _SERVER_PROTOTYPES_H

#ifdef SERVER_FUNCTIONS_INCLUDE

/* define the server function prototypes */
#define _ ,
#define GAL_SERVER_NAME(__name__) static char *ServerName = #__name__;
#define GAL_SERVER_HOST(__host__)
#define GAL_SERVER_PORT(__port__) static int DefaultPort = __port__;
#define GAL_SERVER_DISPATCH_PORT(__port__) static int DefaultPort = __port__;
#define GAL_SERVER_OP(__op__) Gal_Frame __op__(Gal_Frame, void *);
#define GAL_SERVER_OP_SIGNATURE(__op__, __in_array__, __other_in_keys__, __reply__, __out_array__, __other_out_keys__) Gal_Frame __op__(Gal_Frame, void *);
#define GAL_SERVER_SW_PACKAGE(__name__) extern void _GalSS_configure_##__name__();
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
#undef _

#endif /* #ifdef SERVER_FUNCTIONS_INCLUDE */

#endif /* #ifndef _SERVER_PROTOTYPES_H */
