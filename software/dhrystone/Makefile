FIVEALIVE_ROOT ?= $(realpath ../../)
 
# Application-specific options
CFILES = dhry_1.c dhry_2.c stdlib.c
CFLAGS =

# Common flow
include $(FIVEALIVE_ROOT)/software/common/generic.mk

dhry_1.o dhry_2.o: \
  CFLAGS += -Wno-implicit-int -Wno-implicit-function-declaration
