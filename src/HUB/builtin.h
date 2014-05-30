/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy.h"

char *scratch_that(Gal_Frame frame);
char *clear_history(Gal_Frame frame);
char *erase_all_history(Gal_Frame frame);
char *close_off(Gal_Frame frame);
char *greetings(Gal_Frame frame);
char *praise(Gal_Frame frame);
char *vacuous(Gal_Frame frame);
char *domain_switch(Gal_Frame frame);
char *unknown_word(Gal_Frame frame);
char *no_parse(Gal_Frame frame);
char *call_me(Gal_Frame frame);

static struct GalHUB_FUNCTION_MAP { char *name; char *(*fn)(); }
hub_function_map[] = { 
	{"scratch_that", scratch_that },
	{"clear_history", clear_history },
	{"erase_all_history", erase_all_history },
	{"close_off", close_off },
	{"greetings", greetings},
	{"praise", praise},
	{"vacuous", vacuous},
	{"domain_switch", domain_switch },
	{"unknown_word", unknown_word },
	{"no_parse", no_parse },
	{"call_me", call_me },
	{ NULL, NULL}
	};
