/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include <ctype.h>

#include "galaxy/util.h"
#include "gal_internal.h"

/********************************************************************************
 * Read a token from a stream.
 ********************************************************************************/

char *Gal_ReadToken(Gal_InputStream gs, char *tok, int toksize,
		    const char *stop_chars, int do_rewind)
{  
  int next_char;
  int i = 0, i_max = toksize - 1;

  if (tok && gs && gs->fn_pkg->next_char_fn)
  {
    tok[0] = '\0';

    /* skip over white space */
    while (isspace((next_char = (*gs->fn_pkg->next_char_fn)(gs))));
    if (next_char <= 0) return(NULL);

    /* read until the next space or stop character */
    while ((next_char > 0) &&
	   (!isspace(next_char)) &&
	   ((i == 0) || (stop_chars == NULL) || (strchr(stop_chars, next_char) == NULL)))
    {
      if (i < i_max)
	tok[i++] = next_char;
      else
	i++;
      next_char = (*gs->fn_pkg->next_char_fn)(gs);
    }

    /* unread the last character unless end of stream or error */
    if (do_rewind && (next_char > 0))
      (*gs->fn_pkg->rewind_fn)(gs, 1);

    if (i <= i_max)
      tok[i] = 0;
    else
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Truncated token %s while reading", tok);
      tok[i_max] = 0;
    }
    return(tok);
  }
  return(NULL);
}

/********************************************************************************
 * Read next whitespace delineated token from a string.
 * Return pointer to next char after token.
 * If no token found, return NULL.
 ********************************************************************************/

char *Gal_NextToken(const char *sent, char *tok, int toksize)
{
  char *cp = NULL;

  if (tok && sent)
  {
    Gal_InputStream gs = Gal_MakeStringInputStream(sent);

    if (gs)
    {
      char *res = Gal_ReadToken(gs, tok, toksize, NULL, 1);
      if (res)
	cp = Gal_StringInputStreamString(gs);
      free(gs);
    }
  }
  return(cp);
}

/********************************************************************************
 * String comparison functions (replace symbol comparison using ==)
 ********************************************************************************/

int Gal_StringEq(const char *str1, const char *str2)
{
  if (str1 && str2)
  {
    if (strcmp(str1, str2) == 0)
      return(GAL_TRUE);
  }
  return(GAL_FALSE);
}

int Gal_StringCaseEq(const char *str1, const char *str2)
{
  if (str1 && str2)
  {
    if (_gal_strcasecmp(str1, str2) == 0)
      return(GAL_TRUE);
  }
  return(GAL_FALSE);
}

/********************************************************************************
 * Trim characters from the beginning and/or end of a string
 ********************************************************************************/

void Gal_StringRightTrim(char *str, const char *trim)
{
  int i;

  if (trim == NULL)
    trim = " \t\r\n\v\f";

  if (str)
  {
    i = strlen(str) - 1;
    while ((i >= 0) && strchr(trim, str[i])) i--;
    str[i+1] = '\0';
  }
}

void Gal_StringLeftTrim(char *str, const char *trim)
{
  char *sp, *cp;

  if (trim == NULL)
    trim = " \t\r\n\v\f";

  if (str)
  {
    sp = str;
    while (*sp && strchr(trim, *sp)) sp++;
    cp = str;
    while (*sp) *cp++ = *sp++;
    *cp = '\0';
  }
}

void Gal_StringTrim(char *str, const char *trim)
{
  Gal_StringRightTrim(str, trim);
  Gal_StringLeftTrim(str, trim);
}

/********************************************************************************
 * Test if a string is a valid int or float
 ********************************************************************************/

int Gal_DigitStringp(const char *word)
{
  int wlen, i, dcount = 0;
  int tc;

  if (word == NULL) return(0);
  if ((wlen = strlen(word)) == 0) return(0);

  for (i=0; i<wlen; i++)
  {
    tc = word[i];
    if (tc == '-' || tc == '+')
    {
      if (i == 0)
      continue;
      else
      return(0);
    }
    else if (isdigit(tc))
    {
      dcount++;
    }
    else
      return(0);
  }
  if (dcount > 0)
    return(1);
  return(0);
}

int Gal_FloatStringp(const char *word)
{
  int wlen, i, pcount=0, dcount=0, got_e=0, got_pm=0;
  int tc;

  if (word == NULL) return(0);
  if ((wlen = strlen(word)) == 0) return(0);

  for(i=0; i<wlen; i++)
  {
    tc = word[i];
    if (tc == 'e' || tc == 'E')
    {
      if (!dcount || got_e)
	return(0);
      else
	got_e = i;
    }
    else if (tc == '.')
    {
      if (pcount || got_e)
	return(0);
      else
	pcount = 1;
    }
    else if (tc == '-' || tc == '+')
    {
      if (got_pm)
	return(0);
      if (i == 0)
	continue;
      else if (got_e && (i == (got_e + 1)))
	got_pm = 1;
      else
	return(0);
    }
    else if (isdigit(tc))
    {
      dcount++;
    }
    else
      return(0);
  }
  if (dcount > 0)
    return(1);
  return(0);
}
