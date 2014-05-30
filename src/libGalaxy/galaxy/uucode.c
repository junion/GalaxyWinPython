/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 	PURPOSE:  uuencode and uudecode for binary data
 *
 *  	The algorithm takes three octets as input and writes four
 *  	characters of output by splitting the input at six-bit
 *  	intervals into four octets, containing data in the lower
 *  	six bits only.  These octets are converted printable non-
 *  	space characters by adding a value of 0x30 to each octet,
 *  	so that each octet is in the range 0x30-0x6f, within the
 *  	printable ASCII character set.
 *
 * 	original sources provided by MITRE
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *_gal_uuencode(char *source, int len)
{
  int enc_len;
  char *enc_buf;
  char source0, source1, source2;
  int i, j = 0;

  if (source && len)
  {
    /* add one for rounding error, one for null termination */
    enc_len = (((len / 3) + 1) * 4) + 1;
    enc_buf = (char *) malloc(enc_len * sizeof(char));

    if (enc_buf)
    {
      for (i = 0; i < len; i += 3) {
	source0 = source[i];
	if (i + 1 < len)
	  source1 = source[i + 1];
	else source1 = (char) 0;
	if (i + 2 < len)
	  source2 = source[i + 2];
	else source2 = (char) 0;

	enc_buf[j++] = ((source0 >> 2) & 0x3F) + 0x20;
	enc_buf[j++] = (((source0 << 4) | ((source1 >> 4) & 0xF)) & 0x3F) + 0x20;
	enc_buf[j++] = (((source1 << 2) | ((source2 >> 6) & 0x3)) & 0x3F) + 0x20;
	enc_buf[j++] = (source2 & 0x3F) + 0x20;
      }
      enc_buf[j] = '\0';
      return enc_buf;
    }
  }
  return(NULL);
}
  
char *_gal_uudecode(char *source, int len)
{
  char *dec_buf;
  char sa, sb, sc, sd;
  int enc_len;
  int i, j = 0;

  if (source && len)
  {
    /* malloc something of the original length */
    dec_buf = (char *) malloc(len * sizeof(char));
    enc_len = strlen(source);

    if (dec_buf)
    {
      for (i = 0; i < enc_len; i += 4) {
	/* Decode like this, being sure that we're always less than len. */
	/* Things may have been padded. */
	sa = source[i] - 0x20;
	sb = source[i+1] - 0x20;
	sc = source[i+2] - 0x20;
	sd = source[i+3] - 0x20;
	dec_buf[j++] = ((sa << 2) | ((sb >> 4) & 0x3)) & 0xFF;
	if (j < len) 
	  dec_buf[j++] = ((sb << 4) | ((sc >> 2) & 0xF)) & 0xFF;
	else break;
	if (j < len)
	  dec_buf[j++] = ((sc << 6) | (sd & 0x3F)) & 0xFF;
	else break;
      }
      return dec_buf;
    }
  }
  return(NULL);
}
