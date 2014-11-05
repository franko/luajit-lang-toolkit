
# Makefile
#
# Copyright (C) 2014 Francesco Abbate
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

ifneq (,$(findstring Windows,$(OS)))
  HOST_SYS= Windows
else
  HOST_SYS:= $(shell uname -s)
  ifneq (,$(findstring CYGWIN,$(TARGET_SYS)))
    HOST_SYS= Windows
  endif
endif

AR= ar rcu
RANLIB= ranlib

ifeq ($(HOST_SYS),Windows)
  LUAJIT_CFLAGS := -IC:/fra/local/include
else
  LUAJIT_CFLAGS := $(shell pkg-config luajit --cflags)
endif

CFLAGS += $(LUAJIT_CFLAGS)

COMPILE = $(CC) $(CFLAGS) $(DEFS) $(INCLUDES)

LANG_SRC_FILES = language-gs.c lua-language-gs.c
LANG_OBJ_FILES := $(LANG_SRC_FILES:%.c=%.o)
DEP_FILES := $(LANG_SRC_FILES:%.c=.deps/%.P)

DEPS_MAGIC := $(shell mkdir .deps > /dev/null 2>&1 || :)

TARGETS = liblang.a

all: $(TARGETS)

liblang.a: $(LANG_OBJ_FILES)
	@echo Archive $@
	@$(AR) $@ $?
	@$(RANLIB) $@

%.o: %.c
	@echo Compiling $<
	@$(COMPILE) -Wp,-MMD,.deps/$(*F).pp -c $<
	@-cp .deps/$(*F).pp .deps/$(*F).P; \
	tr ' ' '\012' < .deps/$(*F).pp \
          | sed -e 's/^\\$$//' -e '/^$$/ d' -e '/:$$/ d' -e 's/$$/ :/' \
            >> .deps/$(*F).P; \
	rm .deps/$(*F).pp

.PHONY: clean all

clean:
	rm -fr *.o *.a $(TARGETS)

-include $(DEP_FILES)
