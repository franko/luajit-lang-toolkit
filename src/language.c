#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "lua.h"
#include "luaconf.h"
#include "lauxlib.h"
#include "lualib.h"
#include "language.h"
#include "language_bcloader.h"

#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

/* The position in the Lua stack of the loadstring and loadfile functions. */
#define MY_LOADSTRING_INDEX 1
#define MY_LOADFILE_INDEX 2

lua_State *parser_L;

/* Pop a string from "parser_L" top of the stack and push it in "L" stack. */
static void error_xtransfer(lua_State *L)
{
    const char *msg = lua_tostring(parser_L, -1);
    lua_pushstring(L, msg);
    lua_pop(parser_L, 1);
}

static int loadbuffer_xtransfer(lua_State *L, const char *filename)
{
    size_t code_len;
    const char *code = lua_tolstring(parser_L, -1, &code_len);
    int status = luaL_loadbuffer(L, code, code_len, filename);
    lua_pop(parser_L, 1);
    return status;
}

int
language_init(lua_State *L) {
    parser_L = luaL_newstate();
      if (unlikely(parser_L == NULL)) {
        lua_pushstring(L, "cannot create state: not enough memory");
        return LUA_ERRRUN;
    }
    luaL_openlibs(parser_L);
    language_bc_preload(parser_L);

    lua_getglobal(parser_L, "require");
    lua_pushstring(parser_L, "lang.compile");
    int load_status = lua_pcall(parser_L, 1, 1, 0);
    if (unlikely(load_status != 0)) {
        error_xtransfer(L);
        return LUA_ERRRUN;
    }
    if (lua_istable(parser_L, -1)) {
        lua_pushstring(parser_L, "string");
        lua_rawget(parser_L, -2);
        lua_pushstring(parser_L, "file");
        lua_rawget(parser_L, -3);
        lua_remove(parser_L, -3);
    } else {
        lua_pop(parser_L, 1);
        lua_pushstring(L, "module \"lang.compile\" does not load properly");
        return LUA_ERRRUN;
    }

    if (!lua_isfunction(parser_L, MY_LOADSTRING_INDEX) ||
        !lua_isfunction(parser_L, MY_LOADFILE_INDEX)) {
        lua_pop(parser_L, 2);
        lua_pushstring(L, "invalid compile functions");
        load_status = LUA_ERRRUN;
    }
    return load_status;
}

static int
language_check_error(lua_State *L, const char *filename, int parse_status)
{
    if (parse_status != 0) {
        error_xtransfer(L);
        return LUA_LANGERR;
    }
    int compile_success = lua_toboolean(parser_L, -2);
    if (!compile_success) {
        error_xtransfer(L);
        lua_pop(parser_L, 1); /* Pop the boolean value. */
        return LUA_ERRSYNTAX;
    }
    int load_status = loadbuffer_xtransfer(L, filename);
    lua_pop(parser_L, 1); /* Pop the boolean value. */
    return load_status;
}

int
language_loadbuffer(lua_State *L, const char *buff, size_t sz, const char *name)
{
    /* Check if the string begin with the bytecode header. */
    if (sz >= 4 && buff[0] == 0x1b && buff[1] == 0x4c && buff[2] == 0x4a) {
        return luaL_loadbuffer(L, buff, sz, name);
    }
    lua_pushvalue(parser_L, MY_LOADSTRING_INDEX);
    lua_pushlstring(parser_L, buff, sz);
    lua_pushstring(parser_L, name);
    int parse_status = lua_pcall(parser_L, 2, 2, 0);
    return language_check_error(L, name, parse_status);
}

static void l_message(const char *pname, const char *msg)
{
    if (pname) fprintf(stderr, "%s: ", pname);
    fprintf(stderr, "%s\n", msg);
    fflush(stderr);
}

int language_report(lua_State *_L, int status)
{
    lua_State *L = parser_L;
    if (status && !lua_isnil(L, -1)) {
        const char *msg = lua_tostring(L, -1);
        if (msg == NULL) msg = "(error object is not a string)";
        l_message("<luajit-lang-toolkit parser>", msg);
        lua_pop(L, 1);
    }
    return status;
}

int
language_loadfile(lua_State *L, const char *filename)
{
    lua_pushvalue(parser_L, MY_LOADFILE_INDEX);
    lua_pushstring(parser_L, filename);
    int parse_status = lua_pcall(parser_L, 1, 2, 0);
    return language_check_error(L, filename, parse_status);
}
