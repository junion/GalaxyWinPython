/*
  This file (c) Copyright 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* This file contains the tags which have elements which
   have already been mentioned in program_tags.h. The idea is
   that tag_enum.h includes program_tags.h and only that, to
   get all the elements defined, and all the mappings are
   defined here. */

GAL_TAG("&&", GAL_TEST_AND)
GAL_TAG("||", GAL_TEST_OR)
GAL_TAG("==", GAL_TEST_EQ)
GAL_TAG("^", GAL_TEST_STRSTR)
GAL_TAG("%", GAL_TEST_STRSTR)
