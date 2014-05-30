/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"

static int num_alarms = 0;
static char **alarm_names = 0;

char *GalHUB_AlarmGetDisableKey()
{
  return "disable";
}

char *GalHUB_AlarmGetEnableKey() {
  return "enable";
}

void GalHUB_AlarmAdd(char *alarm)
{
  int i;
  for(i=0;i<num_alarms;i++)
    if(!strcmp(alarm_names[i],alarm))	/* already there */
      return;

  if(!alarm_names) {
    alarm_names = (char **)calloc(1,sizeof(char *));
    alarm_names[0] = _gal_strdup(alarm);
    num_alarms++;
    return;
  } else {
    alarm_names = (char **)realloc(alarm_names,(num_alarms+1) * sizeof(char *));
    alarm_names[num_alarms++] = _gal_strdup(alarm);
  }
}


char *GalHUB_AlarmIndexToName(int i)
{
  if(i < 0 || i >= num_alarms)
    return 0;
  return alarm_names[i];
}

int GalHUB_AlarmNameToIndex(char *name)
{
  int i;
  for(i=0;i<num_alarms;i++)
    if(!strcmp(alarm_names[i],name))
      return i;
  return -1;
}

int GalHUB_AlarmGetNum(void)
{
  return num_alarms;
}

