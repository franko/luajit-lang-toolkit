#include <string.h>
#include <lua.h>
#include <lauxlib.h>

#include "language_bcloader.h"

#include "lang/ast-boolean-const-eval.h"
#include "lang/ast-const-eval.h"
#include "lang/bcread.h"
#include "lang/bcsave.h"
#include "lang/bytecode.h"
#include "lang/compile.h"
#include "lang/generator.h"
#include "lang/lexer.h"
#include "lang/lua-ast.h"
#include "lang/operator.h"
#include "lang/parser.h"
#include "lang/reader.h"

struct bcpair {
    const char *name;
    const char *bc;
    size_t size;
};

static struct bcpair bcmodule[] = {
    {"ast-boolean-const-eval", luaJIT_BC_ast_boolean_const_eval, luaJIT_BC_ast_boolean_const_eval_SIZE},
    {"ast-const-eval", luaJIT_BC_ast_const_eval, luaJIT_BC_ast_const_eval_SIZE},
    {"bcread", luaJIT_BC_bcread, luaJIT_BC_bcread_SIZE},
    {"bcsave", luaJIT_BC_bcsave, luaJIT_BC_bcsave_SIZE},
    {"bytecode", luaJIT_BC_bytecode, luaJIT_BC_bytecode_SIZE},
    {"compile", luaJIT_BC_compile, luaJIT_BC_compile_SIZE},
    {"generator", luaJIT_BC_generator, luaJIT_BC_generator_SIZE},
    {"lexer", luaJIT_BC_lexer, luaJIT_BC_lexer_SIZE},
    {"lua-ast", luaJIT_BC_lua_ast, luaJIT_BC_lua_ast_SIZE},
    {"operator", luaJIT_BC_operator, luaJIT_BC_operator_SIZE},
    {"parser", luaJIT_BC_parser, luaJIT_BC_parser_SIZE},
    {"reader", luaJIT_BC_reader, luaJIT_BC_reader_SIZE},
    {0, 0, 0}
};

/* Load into package.preload lang.* modules using embedded bytecode. */
void language_bc_preload(lua_State *L)
{
    struct bcpair *i;
    lua_getfield(L, LUA_GLOBALSINDEX, "package");
    lua_getfield(L, -1, "preload");
    for (i = bcmodule; i->name; i++) {
        lua_pushstring(L, "lang.");
        lua_pushstring(L, i->name);
        lua_concat(L, 2);
        luaL_loadbuffer(L, i->bc, i->size, lua_tostring(L, -1));
        lua_rawset(L, -3);
    }
    lua_pop(L, 2);
}
