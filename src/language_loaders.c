#include <string.h>

#include "lua.h"
#include "lauxlib.h"
#include "language.h"
#include "language_loaders.h"

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

static int readable(const char *filename)
{
  FILE *f = fopen(filename, "r");  /* try to open file */
  if (f == NULL) return 0;  /* open failed */
  fclose(f);
  return 1;
}

static const char *pushnexttemplate(lua_State *L, const char *path)
{
  const char *l;
  while (*path == *LUA_PATHSEP) path++;  /* skip separators */
  if (*path == '\0') return NULL;  /* no more templates */
  l = strchr(path, *LUA_PATHSEP);  /* find next separator */
  if (l == NULL) l = path + strlen(path);
  lua_pushlstring(L, path, (size_t)(l - path));  /* template */
  return l;
}

static const char *searchpath (lua_State *L, const char *name,
                   const char *path, const char *sep,
                   const char *dirsep)
{
  luaL_Buffer msg;  /* to build error message */
  luaL_buffinit(L, &msg);
  if (*sep != '\0')  /* non-empty separator? */
    name = luaL_gsub(L, name, sep, dirsep);  /* replace it by 'dirsep' */
  while ((path = pushnexttemplate(L, path)) != NULL) {
    const char *filename = luaL_gsub(L, lua_tostring(L, -1),
                     LUA_PATH_MARK, name);
    lua_remove(L, -2);  /* remove path template */
    if (readable(filename))  /* does file exist and is readable? */
      return filename;  /* return that file name */
    lua_pushfstring(L, "\n\tno file " LUA_QS, filename);
    lua_remove(L, -2);  /* remove file name */
    luaL_addvalue(&msg);  /* concatenate error msg. entry */
  }
  luaL_pushresult(&msg);  /* create error message */
  return NULL;  /* not found */
}

static void push_package_field(lua_State *L, const char *pname)
{
  lua_getfield(L, LUA_GLOBALSINDEX, "package");
  lua_getfield(L, -1, pname);
  lua_remove(L, -2);
}

static const char *findfile(lua_State *L, const char *name,
                const char *pname)
{
  const char *path;
  push_package_field(L, pname);
  path = lua_tostring(L, -1);
  if (path == NULL)
    luaL_error(L, LUA_QL("package.%s") " must be a string", pname);
  return searchpath(L, name, path, ".", LUA_DIRSEP);
}

static void loaderror(lua_State *L, const char *filename)
{
  luaL_error(L, "error loading module " LUA_QS " from file " LUA_QS ":\n\t%s",
         lua_tostring(L, 1), filename, lua_tostring(L, -1));
}

static int ltk_package_loader_lua(lua_State *L)
{
  const char *filename;
  const char *name = luaL_checkstring(L, 1);
  filename = findfile(L, name, "path");
  if (filename == NULL) return 1;  /* library not found in this path */
  if (language_loadfile(L, filename) != 0)
    loaderror(L, filename);
  return 1;  /* library loaded successfully */
}

static const luaL_Reg language_lib[] = {
  { "loadstring",  language_lua_loadstring },
  { "loadfile",    language_lua_loadfile },
  { "dofile",      language_lua_dofile },
  { "loader",      ltk_package_loader_lua },
  { NULL, NULL }
};

int luaopen_langloaders(lua_State *L)
{
    lua_newtable(L);
    luaL_register(L, NULL, language_lib);
    return 1;
}
