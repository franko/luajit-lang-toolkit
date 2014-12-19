#ifndef LANGUAGE_H
#define LANGUAGE_H

#include "lua.h"

extern int language_init(lua_State *L);
extern int language_report(lua_State *L, int status);
extern int language_loadbuffer(lua_State *L, const char *buff, size_t sz, const char *name);
extern int language_loadfile(lua_State *L, const char *filename);

/* Indicate an internal error of the "language" implementation.
 * LUA_ERRERR is supposed to be the last acceptable value from lua.h. */
#define LUA_LANGERR (LUA_ERRERR + 1)

#endif
