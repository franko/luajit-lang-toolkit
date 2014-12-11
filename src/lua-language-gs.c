#include <string.h>

#include "lua.h"
#include "lauxlib.h"
#include "language.h"
#include "lua-language-gs.h"

static int
language_lua_loadfile(lua_State* L)
{
    const char *filename;
    if (lua_isnoneornil(L, 1)) {
        filename = NULL;
    } else {
        filename = luaL_checkstring(L, 1);
    }

    int status = language_loadfile(L, filename);
    if (status != 0) {
        return lua_error(L);
    }
    return 1;
}

static int
language_lua_dofile(lua_State* L)
{
    language_lua_loadfile(L);
    int n = lua_gettop(L) - 1;
    lua_call(L, 0, LUA_MULTRET);
    return lua_gettop(L) - n;
}

static int
language_lua_loadstring(lua_State* L)
{
    const char *s = luaL_checkstring(L, 1);
    const char *name;
    if (lua_isnoneornil(L, 2)) {
        name = NULL;
    } else {
        name = luaL_checkstring(L, 2);
    }
    int status = language_loadbuffer(L, s, strlen(s), name);
    if (status != 0) {
        return lua_error(L);
    }
    return 1;
}

static const luaL_Reg language_lib[] = {
  { "loadstring",  language_lua_loadstring },
  { "loadfile",    language_lua_loadfile },
  { "dofile",      language_lua_dofile },
  { NULL, NULL }
};

int luaopen_langloaders(lua_State *L)
{
    lua_newtable(L);
    luaL_register(L, NULL, language_lib);
    return 1;
}
