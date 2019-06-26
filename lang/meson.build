lang_sources = ['ast-boolean-const-eval.lua', 'ast-const-eval.lua', 'bcread.lua', 'bcsave.lua', 'bytecode.lua', 'compile.lua', 'generator.lua', 'lexer.lua', 'id-generator.lua', 'lua-ast.lua', 'operator.lua', 'parser.lua', 'reader.lua']

luajit = find_program('luajit')

lang_bc_headers = []
foreach lua_source : lang_sources
    lang_bc_headers += custom_target(lua_source + '.h',
        input: lua_source,
        output: '@BASENAME@.h',
        command: [luajit, '-b', '@INPUT@', '@OUTPUT@']
    )
endforeach

lua_module_install_dir = 'share/lua/5.1'

install_data(lang_sources, install_dir: lua_module_install_dir + '/lang')
