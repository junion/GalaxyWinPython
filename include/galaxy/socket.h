/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef I_GAL_LIB_SOCKET
#define I_GAL_LIB_SOCKET

#ifdef WIN32
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#endif

/* SAM 8/24/99: Copied into open source archive and modified to
   remove unused header file "stio.h". Changed names to "move symbols out
   of the way" so MIT libsls_util and libGalaxy can coexist. */

/*---------------------------------------------------------------------------
  UNIX vs WIN32: sockets
  ---------------------------------------------------------------------------*/
#ifndef WIN32
#include <stdlib.h>
#include <errno.h>
#include <sys/param.h>
#define GAL_SOCKET	        int
#define GAL_INVALID_SOCKET	-1
#define	GAL_SOCKET_ERROR	-1
#define	GAL_SOCKET_ERRNO	errno
#define	GAL_SOCKET_IOCTL	ioctl
#define GAL_SOCKET_CLOSE	close
#define GAL_EWOULDBLOCK         EWOULDBLOCK
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN	64
#endif
#else
#define GAL_INVALID_SOCKET	INVALID_SOCKET
#define GAL_SOCKET			SOCKET
#define GAL_SOCKET_ERROR	SOCKET_ERROR
#define GAL_SOCKET_ERRNO	WSAGetLastError()
#define GAL_EWOULDBLOCK     WSAEWOULDBLOCK
#define GAL_SOCKET_IOCTL	ioctlsocket
#define GAL_SOCKET_CLOSE	closesocket
#define MAXHOSTNAMELEN	64
#endif /* WIN32 */


/* SAM 8/24/99: SELECT seems not to be used anywhere. SELECT_CAST
   seems only to be used here. */

/* I should be able to make many of these a const, but all the
   OS casts would also have to be right. Not bothering. */
int GalUtil_IPFromHostname(char *hostname);
/* Basic socket routines */
GAL_SOCKET GalUtil_SockCreateServer(unsigned short port, int n_listen);
GAL_SOCKET GalUtil_SockCreateClient(char *host, unsigned short port);
GAL_SOCKET GalUtil_SockServerAcceptConnection(GAL_SOCKET sock, char *host);
int GalUtil_SockServerCheckForConnection(GAL_SOCKET sock, char *host, int *client_sock);
int GalUtil_SockServerBlockingCheckForConnection(GAL_SOCKET sock, char *host, int *client_sock);
void GalUtil_SockServerInaddrToString(unsigned long inaddr, char *s);
int GalUtil_SockServerHostnameToInaddr(char *hostname, unsigned long *inaddr);
int GalUtil_IP4Address(unsigned long *inaddr);
int GalUtil_SockClose(GAL_SOCKET sock);
int GalUtil_SockBlockOn(GAL_SOCKET sock);
int GalUtil_SockBlockOff(GAL_SOCKET sock);
int GalUtil_SockCanWrite(GAL_SOCKET sock);
int GalUtil_SockCanRead(GAL_SOCKET sock);
long GalUtil_SockBytesWaiting (GAL_SOCKET sock);

int GalUtil_SockSelect(int maxfd, fd_set *read_fdset,
		       fd_set *write_fdset, fd_set *err_fdset,
		       struct timeval *tp, int *interrupted);

#endif /* I_GAL_LIB_SOCKET */


/* 
  for Emacs...
  Local Variables:
  mode: c
  fill-column: 110
  comment-column: 80
  c-indent-level: 2
  c-continued-statement-offset: 2
  c-brace-offset: -2
  c-argdecl-indent: 2
  c-label-offset: -2
  End:
*/
