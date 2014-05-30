/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"

#include <math.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>

#include "galaxy/util.h"
#include "galaxy/galaxy.h"

#ifdef WIN32
#include <time.h>
#include <winsock2.h>
#else
#include <sys/time.h>
#include <net/if.h>
#endif

#ifdef NEED_SYS_FILIO
#include <sys/filio.h>
#endif

#ifdef NEED_SYS_SOCKIO
#include <sys/sockio.h>
#endif

#ifdef NEED_SYS_IOCTL
#include <sys/ioctl.h>
#endif

#ifdef WIN32
static int winsock_initialized = 0;

int winsock_init(void)
{
  WORD wVersionRequested;
  WSADATA wsaData;
  int err;

  if (winsock_initialized) return 0;

  wVersionRequested = MAKEWORD( 2, 0 ); 
  err = WSAStartup( wVersionRequested, &wsaData );
  if ( err != 0 ) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't find the Winsock DLL\n");
    return -1;
  }
	
  /* Confirm that the WinSock DLL supports 2.0.*/
  /* Note that if the DLL supports versions greater    */
  /* than 2.0 in addition to 2.0, it will still return */
  /* 2.0 in wVersion since that is the version we      */
  /* requested.                                        */ 
  if ( LOBYTE( wsaData.wVersion ) != 2 ||
       HIBYTE( wsaData.wVersion ) != 0 ) {
    /* Tell the user that we could not find a usable */
    /* WinSock DLL.                                  */

    GalUtil_WarnWithLocation(__FUNCTION__, "Winsock DLL is too old, need v2.0 at least\n");

    WSACleanup( );
    return -1;
  }
	
  /* The WinSock DLL is acceptable */

  winsock_initialized = 1;

  return 0;
}
#endif

/* Ray Lau says: this is IPv4 compliant only. This is because we don't
   have reentrant version of inet_ntoa. An interaction with Tcl was
   trouncing the internal buffer for Tcl and Tcl was misbehaving. */

/* SAM 9/13/99: This is used in more than one place, and not used
   in some places here where it probably should. So I've generalized and
   exported it. Note that no bounds checking is done on the buffer. */

void GalUtil_SockServerInaddrToString(unsigned long in_addr, char *s)
{
  unsigned long a = ntohl(in_addr);
  sprintf(s,"%d.%d.%d.%d",
	  (int)((a & 0xff000000) >> 24),	  
	  (int)((a & 0xff0000) >> 16),	  
	  (int)((a & 0xff00) >> 8),
	  (int)(a & 0xff));
}

/* SAM 9/13/99: Moved a lot of the behavior from GalIO_IPAddress() to
   here in order to localize the network access and manipulation. */

/* SAM 9/27/99: added checks for gethostbyname_r() to configure
   script, but SLS version won't have access. So I'll just reconstruct
   the best guesses from the previous release if HAVE_CONFIG_H is
   not defined. For Linux, I'm not sure what to say; some versions
   have the 6-arg version, some the 5-arg version. Ugh. */

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

#define MAXADDRLENGTH 64

static int __GalUtil_GetHostByName(char *hostname, char *inbuf)
{
#ifdef HAVE_GETHOSTBYNAME_R
  struct hostent hinfo[1];
#if defined(HAVE_GETHOSTBYNAME_R_3_ARG) || defined(HAVE_GETHOSTBYNAME_R_6_ARG)
  int status;
#ifdef HAVE_GETHOSTBYNAME_R_3_ARG
  struct hostent_data hdata[1];
#endif /* HAVE_GETHOSTBYNAME_R_3_ARG */
#ifdef HAVE_GETHOSTBYNAME_R_6_ARG
  struct hostent *hdata;
#endif /* HAVE_GETHOSTBYNAME_R_6_ARG */
#endif /* defined(HAVE_GETHOSTBYNAME_R_3_ARG) || defined(HAVE_GETHOSTBYNAME_R_6_ARG) */
#if defined(HAVE_GETHOSTBYNAME_R_5_ARG) || defined(HAVE_GETHOSTBYNAME_R_6_ARG)
  int error = 0;
  char buf[MAXHOSTNAMELEN*10];
#endif /* defined(HAVE_GETHOSTBYNAME_R_5_ARG) || defined(HAVE_GETHOSTBYNAME_R_6_ARG) */
#endif /* HAVE_GETHOSTBYNAME_R */
  struct hostent *hinfo_ptr = NULL;

#ifdef HAVE_GETHOSTBYNAME_R_5_ARG
  hinfo_ptr = gethostbyname_r(hostname, hinfo, buf,
			      MAXHOSTNAMELEN*10, &error);
#endif /* HAVE_GETHOSTBYNAME_R_5_ARG */
#ifdef HAVE_GETHOSTBYNAME_R_3_ARG
  memset(hdata, 0, sizeof(struct hostent_data));
  status = gethostbyname_r(hostname, hinfo, hdata);
  if (status == 0)
    hinfo_ptr = hinfo;
#endif /* HAVE_GETHOSTBYNAME_R_3_ARG */
#ifdef HAVE_GETHOSTBYNAME_R_6_ARG
  status = gethostbyname_r(hostname, hinfo, buf, MAXHOSTNAMELEN*10,
			   &hdata, &error);
  if (status == 0)
    hinfo_ptr = hinfo;
#endif /* HAVE_GETHOSTBYNAME_R_6_ARG */
#ifdef HAVE_GETHOSTBYNAME
  hinfo_ptr = gethostbyname(hostname);
#endif

  if (hinfo_ptr) {
    if (hinfo_ptr->h_addr_list[0]) {
      if (hinfo_ptr->h_length <= MAXADDRLENGTH) {
	memset(inbuf, 0, MAXADDRLENGTH);
	memcpy(inbuf, hinfo_ptr->h_addr_list[0], hinfo_ptr->h_length);
	return hinfo_ptr->h_length;
      }
    }
  }
  return -1;
}

int GalUtil_SockServerHostnameToInaddr(char *hostname, unsigned long *inaddr)
{
  char buf[MAXADDRLENGTH];
  int h_length = __GalUtil_GetHostByName(hostname, buf);
  struct in_addr in;

  if ((h_length != -1) && (sizeof(in.s_addr) < MAXADDRLENGTH)) {
    (void) memcpy(&in.s_addr, buf, sizeof (in.s_addr));
    *inaddr = in.s_addr;
    return 1;
  }
  return 0;
}

int GalUtil_IPFromHostname(char *hostname)
{
  int ip;
  unsigned long inaddr;
  char buf[MAXHOSTNAMELEN*2];

  ip = 0;
  if (GalUtil_SockServerHostnameToInaddr(hostname, &inaddr)) {
    GalUtil_SockServerInaddrToString(inaddr, buf);
    ip = inet_addr(buf);
  } else GalUtil_WarnWithLocation(__FUNCTION__, "gethostbyname error on host %s\n", hostname);
  
  return(ip);
}

/*
 * int GalUtil_IP4Address(unsigned long *inaddr)
 * 
 * Searches the interfaces for an IP address. If only a
 * loopback address exists, we use it, but otherwise we
 * use the first non-loopback address.
 SAM 12/3/01: Got it working for BSDs.
 */

int GalUtil_IP4Address(unsigned long *inaddr)
{
  int result = 0;
#ifndef WIN32
  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  int len = 0;
  int lastlen = len;
  char *buf = NULL;
  char *ptr = NULL;
  struct ifconf ifc;
  struct in_addr loopaddr;
  loopaddr.s_addr = htonl(INADDR_LOOPBACK);

  while (1) {
    /* Use trial and error to find a large enough buffer.
       We need to keep checking until the difference in
       size is greater than the size of an ifreq, because that
       implies that we didn't use all the space. We're not
       guaranteed that the ioctl will fail if there's not
       enough room for all the interfaces. It fails on Solaris 2.7,
       but not on Linux or MacOS X. */
    if (len > 0)
      buf = (char *) malloc(len);
    else
      buf = (char *) NULL;

    ifc.ifc_len = len;
    ifc.ifc_buf = buf;
    if (ioctl(sockfd, SIOCGIFCONF, &ifc) < 0) {
      if (errno != EINVAL || lastlen != 0) {
	free(buf);
	close(sockfd);
	return 0;
      }
    } else {
      /* Better cast sizeof() to an int, since if it's
	 unsigned, the comparison will be unsigned,
	 and if len - ifc.ifc_len is negative, well,
	 we're in trouble. */
      if ((len - ifc.ifc_len) >= (int) sizeof(struct ifreq))
	break;
      lastlen = ifc.ifc_len;
    }
    len += 10*sizeof(struct ifreq);
    if (buf) free(buf);
  }
  
  /* Search for the first interface that isn't loopback.
     In order to do this, we must examine both the family
     (we don't want a non-AF_INET interface) and see whether
     the interface is up. We need to do a separate ioctl
     to get the flags which will tell us the interface is up,
     and that ioctl has to be done on a copy of the ifreq
     structure, because the flags memory and the address
     memory are in a C union. Grrr.
  */
  ptr = (char *) ifc.ifc_req;
  while (ptr < (buf + ifc.ifc_len)) {
    struct ifreq *addr_ifr = (struct ifreq *) ptr;
    struct ifreq flag_ifr_mem;
    struct ifreq *flag_ifr = &flag_ifr_mem;
    struct in_addr addr;
    
    /* I can't look both for the flags and the address in the
       same structure, becuase it's probably a union. So
       we copy the structure and do the ioctl on the copy. */
    
    memcpy(flag_ifr, addr_ifr, sizeof(struct ifreq));
    if (ioctl(sockfd, SIOCGIFFLAGS, (char *) flag_ifr) < 0) {
      GalUtil_WarnWithLocation(__FUNCTION__, "failed in SIOCGIFFLAGS\n");
      close(sockfd);
      return(0);
    }
    
    if (!(flag_ifr->ifr_flags & IFF_UP)) {
      /* Interface isn't up. */
      continue;
    } else if (!(addr_ifr->ifr_addr.sa_family == AF_INET)) {
      /* Interface isn't AF_INET. */
    } else {
      addr = ((struct sockaddr_in *) &addr_ifr->ifr_addr)->sin_addr;
      if (addr.s_addr == loopaddr.s_addr) {
	if (!result){
	  /* Loopback found, but no other address has been found yet,
	   * so tentatively use loopback, but keep looking
	   */
	  *inaddr = addr.s_addr;
	  result = 1;	
	}
      } else {
	*inaddr = addr.s_addr;
	result = 1;
	break;
      }
    }
    /* The size of the structure is constant on non-BSD,
       variable on BSD. */
#ifndef HAVE_SA_LEN
    ptr += sizeof(addr_ifr->ifr_name) + sizeof(struct sockaddr);
#else
    ptr += sizeof(addr_ifr->ifr_name) + \
      (addr_ifr->ifr_addr.sa_len > sizeof(struct sockaddr)
       ? addr_ifr->ifr_addr.sa_len
       : sizeof(struct sockaddr));
#endif
  }

  close(sockfd);
  free(buf);
#endif
  return result;
}


GAL_SOCKET
GalUtil_SockCreateClient(char *host, unsigned short port)
{
  struct sockaddr_in server, local;
  GAL_SOCKET sock;
  int res, len, err;
  unsigned long addr;

#ifdef WIN32
  if (winsock_init() < 0)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "WinSock initialization failed");
    return (GAL_INVALID_SOCKET);
  }
#endif

#ifdef __sunos__
  bzero((char *)&server, sizeof(server));
#else
  memset(&server, 0, sizeof(server));
#endif

  server.sin_family = AF_INET;
  server.sin_port = htons(port);

  /* Create the new socket */
  sock = socket(AF_INET, SOCK_STREAM, 0);
  if (sock == GAL_INVALID_SOCKET) {
#ifdef WIN32
    err = WSAGetLastError();
#else
    err = errno;
#endif
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to create socket, error: %d", err);
    return (GAL_INVALID_SOCKET);
  }

  /* Fail on connection to ourself (possible at least in Linux). */
  len = sizeof(local);
  res = getsockname(sock, (struct sockaddr *) &local, &len);
  if (res == 0
      && len == sizeof(local)
      && len == sizeof(server)
      && memcmp(&local, &server, len) == 0) {
    GAL_SOCKET_CLOSE(sock);
    GalUtil_WarnWithLocation(__FUNCTION__, "Socket connected with itself");
    return(GAL_INVALID_SOCKET);
  }

  /* Get the full specs of the target host */
  if((int)(addr = inet_addr(host)) == -1) {
    int h_length;
    char buf[MAXADDRLENGTH];

    h_length = __GalUtil_GetHostByName(host, buf);
    
    if (h_length == -1) {
      GAL_SOCKET_CLOSE(sock);
      return (GAL_INVALID_SOCKET);
    }
    _gal_bcopy((char*) buf, (char*) &(server.sin_addr), h_length);
  } else {
    /* 981015 raylau: do not use gethostbyaddr as that attempts reverse DNS! */
    /* struct in_addr in_val; */
    unsigned long in_val_ul;

    in_val_ul = inet_addr(host);	/* actually, this is deprecated usage, but solaris 2.5 doesn't yet do inet_aton */
    _gal_bcopy((char*) &in_val_ul, (char*) &(server.sin_addr), sizeof(in_val_ul));

    /*inet_aton(host,&in_val);*/
    /*_gal_bcopy((char*) &in_val_ul, (char*) &(server.sin_addr), sizeof(struct in_addr));*/
  }

	/* Now connect this socket to target host/port */
  res = connect(sock, (struct sockaddr *)&server, sizeof(server));
  if (res == GAL_SOCKET_ERROR) {
    GAL_SOCKET_CLOSE(sock);
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to connect client socket");
    return(GAL_INVALID_SOCKET);
  }

  return(sock);
}


GAL_SOCKET
GalUtil_SockCreateServer(unsigned short port, int n_listen)
{
  struct sockaddr_in server;
  int res, err;
  GAL_SOCKET sock;
  Gal_Boolean notzero = GAL_TRUE;

#ifdef WIN32
  if (winsock_init() < 0)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "WinSock initialization failed");
    return (GAL_INVALID_SOCKET);
  }
#endif

#ifdef __sunos__
  bzero((char *)&server, sizeof(server));
#else
  memset(&server, 0, sizeof(server));
#endif

  /* Create a new socket */
  sock = socket(AF_INET, SOCK_STREAM, 0);
  if (sock == GAL_INVALID_SOCKET) {
#ifdef WIN32
    err = WSAGetLastError();
#else
    err = errno;
#endif
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to create socket, error: %d", err);
    return (GAL_INVALID_SOCKET);
  }
#ifndef WIN32
  /* this should solve the locking sockets problem! phs */
  /* SAM 8/6/02: It appears that NT doesn't hold onto the 
     socket, and furthermore, REUSEADDR has a bug where 
	 ports can be reused ACROSS PROCESSES. This is bad. 
	 So we just don't use this at all on Windows. */
  setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *) &notzero, sizeof(int));
#endif
	/* Bind it to the port */
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port = htons(port);
  res = bind(sock, (struct sockaddr *)&server, sizeof(server));

  if (res == GAL_SOCKET_ERROR) {
    GalUtil_SockClose(sock);
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to bind server socket");
    return(GAL_INVALID_SOCKET);
  }

  /* Specify that we will accept incoming connections */
  listen(sock, n_listen);
  if (res == GAL_SOCKET_ERROR) {
    GalUtil_SockClose(sock);
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to start listening on server socket");
    return(GAL_INVALID_SOCKET);
  }

  return(sock);
}

/* this sets blocking off */
int GalUtil_SockBlockOff(GAL_SOCKET sock)
{
#ifndef WIN32
  int flags;

  flags = fcntl(sock, F_GETFL, 0);

  if (flags < 0)
    return 1;

  flags |= O_NDELAY;

  return fcntl(sock, F_SETFL, flags) < 0;
#else
  u_long arg = 1;
  int err;

  err = ioctlsocket(sock, FIONBIO, &arg);
  if (err) {
    err = WSAGetLastError();
    GalUtil_WarnWithLocation(__FUNCTION__, "Socket error %d", err);
    return 1;
  }
  return 0;

  /* return GAL_SOCKET_IOCTL(sock, FIONBIO, &arg) != SOCKET_ERROR; */
#endif
}

/* this sets blocking on */
int GalUtil_SockBlockOn(GAL_SOCKET sock)
{
#ifndef WIN32
  int flags;

  flags = fcntl(sock, F_GETFL, 0);
  if (flags < 0) {
      GalUtil_WarnWithLocation(__FUNCTION__, "Socket error %d", errno);
      return 1;
  }

  flags &= ~O_NDELAY;

  return fcntl(sock, F_SETFL, flags) < 0;
#else
  u_long arg = 0;
  int err;

  err = ioctlsocket(sock, FIONBIO, &arg);
  if (err) {
    err = WSAGetLastError();
    GalUtil_WarnWithLocation(__FUNCTION__, "Socket error %d", err);
    return 1;
  }
  return 0;

  /* return GAL_SOCKET_IOCTL(sock, FIONBIO, &arg) != SOCKET_ERROR; */
#endif
}

int GalUtil_SockClose(GAL_SOCKET sock)
{
  if (GAL_SOCKET_CLOSE(sock) == GAL_SOCKET_ERROR)
    return -1;
  else
    return 0;
}

/* SAM 7/11/00: Trying to handle cross platform EINTR capture. */

int GalUtil_SockSelect(int maxfd, fd_set *read_fdset,
		       fd_set *write_fdset, fd_set *err_fdset,
		       struct timeval *tp, int *interrupted)
{
  int sock_status = select(maxfd, read_fdset, write_fdset, err_fdset, tp);
  
  if (sock_status == GAL_SOCKET_ERROR) {
#ifdef WIN32
    if (WSAGetLastError() == WSAEINTR)
#else
    if (errno == EINTR)
#endif
    {
      if (interrupted) {
	*interrupted = 1;
      }
    }
  }
  return sock_status;
}


#ifndef WIN32
/*------------------------------------------------------------------------------------------------------------
  Do low-level read(), but temporarily set blocking on/off according to the blocking parameter.
  Be careful not to disturb any other flags or mess up errno.
  ------------------------------------------------------------------------------------------------------------*/
int __GalUtil_SockReadBytes(GAL_SOCKET sock, char *buf, int n_bytes, int blocking)
{
  int sock_flags, change_flags, result, save_errno, count;

  /* EH -- init result so compiler won't complain */
  result = 0;

  sock_flags = fcntl(sock, F_GETFL, 0);

  if (blocking) {

    change_flags = sock_flags >= 0 && (sock_flags & O_NDELAY) != 0;
    if (change_flags) {
      fcntl(sock, F_SETFL, sock_flags & ~O_NDELAY);
    }
    count = 0;
    while (n_bytes > 0) {
      result = _gal_read(sock, buf, n_bytes);

      if (result < 0) {
	/* try to continue if it was a signal that stopped us ... */
	if (errno == EINTR)
	  continue;

	break;
      }

      /* MM -- I added this line (and below) (10/16/95) -- on sun-sunos's, */
      /* read _seems_ to return 0 only when there is an error */

      if (result == 0)
	break;

      n_bytes -= result;
      buf += result;
      count += result;
    }
    result = count;

  } else {

    change_flags = sock_flags >= 0 && (sock_flags & O_NDELAY) == 0;
    if (change_flags) {
      fcntl(sock, F_SETFL, sock_flags | O_NDELAY);
    }
    count = 0;
    while (n_bytes > 0) {
      result = _gal_read(sock, buf, n_bytes);

      if (result <= 0)
	break;

      if (result == 0)
	break;

      n_bytes -= result;
      buf += result;
      count += result;
    }
    if (count > 0 && (result == 0 || (result < 0 && errno == GAL_EWOULDBLOCK))) {
      result = count;
      errno = 0;
    }
    
  }

  if (change_flags) {
    save_errno = errno;
    fcntl(sock, F_SETFL, sock_flags);
    errno = save_errno;
  }

  return(result);
}
#else
/*-----------------------------------------------------------------
  Set blocking and do recv(): no F_GETFL/F_SETFL or errno in WIN32.
  -----------------------------------------------------------------*/
int __GalUtil_SockReadBytes(GAL_SOCKET sock, char *buf, int n_bytes, int blocking)
{
  int result = 0;
  int count = 0;

  if (blocking) {

    GalUtil_SockBlockOn(sock);

    while (n_bytes > 0) {
      result = recv(sock, buf, n_bytes, 0);

      if (result == GAL_SOCKET_ERROR)
	break;

      n_bytes -= result;
      buf += result;
      count += result;
    }
    result = count;

  } else {

    GalUtil_SockBlockOff(sock);

    while (n_bytes > 0) {
      result = recv(sock, buf, n_bytes, 0);

      if (result == GAL_SOCKET_ERROR)
	break;

      if (result == 0)
	break;

      n_bytes -= result;
      buf += result;
      count += result;
    }
    if (result != GAL_SOCKET_ERROR || GAL_SOCKET_ERRNO == GAL_EWOULDBLOCK) {
      result = count;
    }
  }
  return(result);
}
#endif

GAL_SOCKET GalUtil_SockServerAcceptConnection(GAL_SOCKET sock, char *host)
{
  GAL_SOCKET client_sock;
  int sz;
  struct sockaddr_in server;
  /* struct hostent *hp; */

  sz = sizeof(server);
  client_sock = accept(sock, (struct sockaddr *)&server, &sz);

  if (client_sock == GAL_INVALID_SOCKET)
    return(GAL_INVALID_SOCKET);

/*
  GalUtil_Print(-1,"  (accept specified size %d bytes for the address)\n", sz);
*/

  /* 19981103 RL -- just return the IP address to avoid reverse DNS */
  /* for some stupid reason (reentrancy due to TCL callback!?)
     inet_ntoa zaps us. */
  GalUtil_SockServerInaddrToString(server.sin_addr.s_addr, host);

/*
  hp = gethostbyaddr((char *)&(server.sin_addr.s_addr), sizeof(long), AF_INET);
  if (hp == NULL) {
    GalUtil_WarnWithLocation(__FUNCTION__, "  (get_host_by_addr failed)\n");
    return (-1);
  }

  strcpy(host, hp->h_name);
*/

  return(client_sock);
}

/* 
 *   Checks if a client is waiting to connect to sock.
 *   Non-blocking, to be called by a polling routine.
 *   Returns 1 for connection, 0 if waiting for connection, 
 *   -1 for error. Sets host and client_sock on success.
 */

/* SAM 9/28/99: Added blocking version for threads.
   9/8/00: changed signature to deal with interruptions. */

static int __GalUtil_SockServerConnectionCheck(GAL_SOCKET sock, char *host, int *client_sock, int blocking, int *interrupted);

int GalUtil_SockServerBlockingCheckForConnection(GAL_SOCKET sock, char *host, int *client_sock)
{
  int interrupted = 0;
  int status = -1;
  
  while ((status = __GalUtil_SockServerConnectionCheck(sock, host, client_sock, 1, &interrupted)) == -1) {
    if (interrupted) {
      /* If this is an interrupted call, reset interrupted
	 and try again. Otherwise, break out of the loop. */
      interrupted = 0;
    } else break;
  }
  return status;
}

int GalUtil_SockServerCheckForConnection(GAL_SOCKET sock, char *host, int *client_sock)
{
  return __GalUtil_SockServerConnectionCheck(sock, host, client_sock, 0, (int *) NULL);
}

static int __GalUtil_SockServerConnectionCheck(GAL_SOCKET sock, char *host, int *client_sock, int blocking, int *interrupted)
{
  GAL_SOCKET poll_sock;
  /* struct hostent *hp;*/
  struct sockaddr_in server;
  int sz = sizeof(server);

  if (blocking) {
    GalUtil_SockBlockOn(sock);
  } else {
    GalUtil_SockBlockOff(sock);
  }

  poll_sock = accept(sock, (struct sockaddr *)&server, &sz);

  if (poll_sock == GAL_INVALID_SOCKET) {
#ifdef WIN32
    int err = WSAGetLastError();
    if ((!blocking) && (err == WSAEWOULDBLOCK)) {
      return 0;
    } else {
      if ((err == WSAEINTR) && interrupted) {
	*interrupted = 1;
      } else {
	GalUtil_WarnWithLocation(__FUNCTION__, "Socket error %d", err);
      }
      return -1;
    }
#else
    if ((!blocking) && (GAL_SOCKET_ERRNO == EWOULDBLOCK)) {
      return(0);
    } else {
      if ((GAL_SOCKET_ERRNO == EINTR) && interrupted) {
	*interrupted = 1;
      } else {
	GalUtil_WarnWithLocation(__FUNCTION__, "Socket error %d", errno);
      }
      return -1;
    }
#endif
  }

  *client_sock = poll_sock;
  
  /* 19981103 RL -- just return the IP address to avoid reverse DNS */
  /* for some stupid reason (reentrancy due to TCL callback!?)
     inet_ntoa zaps us. */
  
  GalUtil_SockServerInaddrToString(server.sin_addr.s_addr, host);
  return(1);
}

int GalUtil_SockCanRead(GAL_SOCKET sock)
{
  fd_set will_read;
  struct timeval to;

  FD_ZERO(&will_read);
  FD_SET(sock, &will_read);
  to.tv_sec = to.tv_usec = 0;
  if (select(sock+1, &will_read, NULL, NULL, &to) == -1)
    GalUtil_WarnWithLocation(__FUNCTION__, "Socket select failed\n");
  if (FD_ISSET(sock, &will_read))
    return(1);
  else
    return(0);
}

int GalUtil_SockCanWrite(GAL_SOCKET sock)
{
  fd_set will_write;
  struct timeval to;


  FD_ZERO(&will_write);
  FD_SET(sock, &will_write);
  to.tv_sec = to.tv_usec = 0;
  if (select(sock+1, NULL, &will_write, NULL, &to) == -1)
      GalUtil_WarnWithLocation(__FUNCTION__, "Socket select failed\n");
  if (FD_ISSET(sock, &will_write))
    return(1);
  else
    return(0);
}

long GalUtil_SockBytesWaiting (GAL_SOCKET sock)
{
    long bytes_available;

    if (GAL_SOCKET_IOCTL(sock, FIONREAD, &bytes_available) < 0)
      return -1;

    return(bytes_available);
}

/* 
  for Emacs...
  Local Variables:
  mode: C
  comment-column: 50
  fill-column: 110
  c-indent-level: 2
  c-continued-statement-offset: 2
  c-brace-offset: -2
  c-argdecl-indent: 2
  c-label-offset: -2
  End:
*/

