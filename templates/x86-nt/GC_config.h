/* x86-nt/GC_config.h.  Mirrors audomatically generated configure header.  */

/* Define this if you have config.h */
#define HAVE_CONFIG_H 1

/* Define this if you have strerror_r */
/* #undef HAVE_STRERROR_R */

/* Define this if all you have is gethostbyname(). */
#define HAVE_GETHOSTBYNAME 1

/* Define this if you have some version of gethostbyname_r() */
/* #undef HAVE_GETHOSTBYNAME_R */

/* Define this if you have the 3-arg version of gethostbyname_r() */
/* #undef HAVE_GETHOSTBYNAME_R_3_ARG */

/* Define this if you have the 5-arg version of gethostbyname_r() */
/* #undef HAVE_GETHOSTBYNAME_R_5_ARG */

/* Define this if you have the 6-arg version of gethostbyname_r() */
/* #undef HAVE_GETHOSTBYNAME_R_6_ARG */

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
#undef WORDS_BIGENDIAN

#define SIZEOF_CHAR 1
#define SIZEOF_SHORT 2
#define SIZEOF_INT 4

/* Not worrying about 64-bit Windows yet. */

#define SIZEOF_LONG 4
#define SIZEOF_LONG_LONG 0
#define SIZEOF_FLOAT 4
#define SIZEOF_DOUBLE 8

/* Define this if you need sys/ioctl.h to get FIONREAD or SIOCGIFCONF */
#undef NEED_SYS_IOCTL

/* Define this if you need sys/filio.h to get FIONREAD */
#undef NEED_SYS_FILIO

/* Define this if you need sys/sockio.h to get SIOCGIFCONF */
#undef NEED_SYS_SOCKIO

#define HAVE_IEEEFP 1

/* Define if you have strtok_r */
#undef HAVE_STRTOK_R

/* Define if you have xdr_longlong_t. MacOS X 10.1 doesn't. */
#define HAVE_XDR_LONGLONG_T 1

/* Define if you have the nanosleep header. Not used on Win32. */
#undef HAVE_NANOSLEEP_HEADER

/* Define if you have sa_len in sockaddr_in. */
#undef HAVE_SA_LEN
