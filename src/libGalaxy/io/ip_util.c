/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
/*
#include <sys/types.h>
#ifndef WIN32
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include <netdb.h>
#else
#include <winsock2.h>
#endif*/
#include "galaxy/galaxy_io.h"

/* SAM 9/10/99: Just wrap a mutex around the whole thing. We're not
   sure what the status of gethostbyname() on some of these platforms
   is, and we're also not sure about the reading properties of
   ip_address. */

static GalUtil_LocalMutex ip_address_lock;

void _Gal_init_ip_util(void)
{
  GalUtil_InitLocalMutex(&ip_address_lock);
}

/* SAM 4/18/02: because GalUtil_IP4Address() calls ioctl(),
   it is not thread-cancellation-safe. So this mutex needs
   to be protected with push/pop. */

extern void _gal_unlock_mutex(void *mutex);

/* return result should NOT be freed */
char *GalIO_IPAddress(void)
{
  static char ip_address[128];
  static int ip_address_retrieved = 0;
  unsigned long inaddr;
  char hbuf[MAXHOSTNAMELEN+1];

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &ip_address_lock);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &ip_address_lock);
#endif    
  GalUtil_LockLocalMutex(&ip_address_lock);
  
  if (!ip_address_retrieved && (
#ifndef WIN32
       GalUtil_IP4Address(&inaddr) ||
#endif
       (_gal_gethostname(hbuf, MAXHOSTNAMELEN) != -1 &&
        GalUtil_SockServerHostnameToInaddr(hbuf, &inaddr)))) {
    GalUtil_SockServerInaddrToString(inaddr, ip_address);
    ip_address_retrieved = 1;
  }
  
  GalUtil_UnlockLocalMutex(&ip_address_lock);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif

  if (ip_address_retrieved)
    return ip_address;
  else
    return (char *) NULL;
}

#ifdef COMPILE_MAIN

char *default_DNS_domain(void)
{
  static char *domain_name = NULL;
  FILE *fp;
  char buf[1024];
  char *cp, *lp = 0;

  if (domain_name)
    return(domain_name);

  fp = fopen("/etc/resolv.conf", "r");
  if (fp == NULL)
  {
    GalUtil_Warn("Failed to open /etc/resolv.conf\n");
    return(NULL);
  }

  while(fgets(buf, 1024, fp))
  {
    cp = _gal_strtok_r(buf, " \t\n", &lp);
    if (strcmp(cp, "domain") == 0)
    {
      cp = _gal_strtok_r(lp, " \t\n", &lp);
      domain_name = _gal_strdup(cp);
      fclose(fp);
      return(domain_name);
    }
  }
    
  return(NULL);
}

char *canonical_hostname(void)
{
  static char *hostname = NULL;
  char hbuf[MAXHOSTNAMELEN+1]; 
  char *dns_domain;
  
  if (hostname)
    return(hostname);

  if (_gal_gethostname(hbuf, MAXHOSTNAMELEN) == -1)
    return NULL;

  if (strchr(hbuf, '.') == NULL)
  {
    dns_domain = default_DNS_domain();
    if (dns_domain)
    {
      strcat(hbuf,".");
      strcat(hbuf,dns_domain);
    }
  }

  hostname = _gal_strdup(hbuf);
  return(hostname);
}

int main(void)
{
  char *name, *ip;
  char *dns_domain;
  char buf[MAXHOSTNAMELEN*10];
  struct hostent hinfo[1];
  struct in_addr in;
  char **p;
  int error = 0;
  
  if ((name = canonical_hostname()))
    GalUtil_Print(-1, "Canonical hostname is %s\n", name);

  if ((ip = GalIO_IPAddress()))
    GalUtil_Print(-1, "IP address is %s\n", ip);

#if 0
  if (gethostbyname_r(name, hinfo, buf, MAXHOSTNAMELEN*10, &error))
  {
    GalUtil_Print(-1,"Canonical hostname is %s\n", hinfo->h_name);

    for (p = hinfo->h_addr_list; *p != 0; p++) {
      struct in_addr in;
      char **q;

      (void) memcpy(&in.s_addr, *p, sizeof (in.s_addr));
      GalUtil_Print(-1,"%s\t%s", inet_ntoa(in), hinfo->h_name);
      for (q = hinfo->h_aliases; *q != 0; q++)
	GalUtil_Print(-1," %s", *q);
      GalUtil_Print(-1,"\n");
    }
  }

  if (gethostbyname_r(ip, hinfo, buf, MAXHOSTNAMELEN*10, &error))
  {
    GalUtil_Print(-1,"Canonical hostname is %s\n", hinfo->h_name);

    for (p = hinfo->h_addr_list; *p != 0; p++) {
      struct in_addr in;
      char **q;

      (void) memcpy(&in.s_addr, *p, sizeof (in.s_addr));
      GalUtil_Print(-1,"%s\t%s", inet_ntoa(in), hinfo->h_name);
      for (q = hinfo->h_aliases; *q != 0; q++)
	GalUtil_Print(-1," %s", *q);
      GalUtil_Print(-1,"\n");
    }
  }
#endif

  exit(0);
}
#endif
