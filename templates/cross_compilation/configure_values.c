#include <stdio.h>

int main()
{
  /* Are we little or big endian?  From Harbison&Steele.
     Taken from macro expansion of bigendian test in GNU configure. */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  
  printf("ac_cc_sizeof_char=%d\n", sizeof(char));
  printf("ac_cc_sizeof_short=%d\n", sizeof(short));
  printf("ac_cc_sizeof_int=%d\n", sizeof(int));
  printf("ac_cc_sizeof_long=%d\n", sizeof(long));
  printf("ac_cc_sizeof_long_long=%d\n", sizeof(long long));
  printf("ac_cc_sizeof_float=%d\n", sizeof(float));
  printf("ac_cc_sizeof_double=%d\n", sizeof(double));

  /* The executable in configure succeeded for small-endian,
     failed for big-endian. Non-zero exit status is failure.
     This test was the exit status of the configure executable.
     So if this test succeeds (true or 1), the overall test
     fails (since it's nonzero), which means it's
     bigendian. */
  
  if (u.c[sizeof (long) - 1] == 1) {
    /* big endian. */
    printf("ac_cc_c_bigendian=yes\n");
  } else {
    printf("ac_cc_c_bigendian=no\n");
  }
  exit(0);
}
