# This is a /bin/sh file which sets variable values.
# It is sourced in two places.

# It must always have a value for host. This value should
# be a value for ARCH-OS.

host=armv4l-linux

# We need to set the following tools: CC, CXX, AR.

CC=/usr/local/arm-linux/bin/arm-linux-gcc
CXX=/usr/local/arm-linux/bin/arm-linux-g++
AR="/usr/local/arm-linux/bin/arm-linux-ar crsv"

# These variables are special cross-compilation variables which could not
# be derived without executing code.

ac_cc_sizeof_char=1
ac_cc_sizeof_short=2
ac_cc_sizeof_int=4
ac_cc_sizeof_long=4
ac_cc_sizeof_long_long=8
ac_cc_sizeof_float=4
ac_cc_sizeof_double=8
ac_cc_c_bigendian=no
