lang_sources = [
    'ast_boolean_const_eval.lua',
    'ast_const_eval.lua',
    'bcread.lua',
    'bcsave.lua',
    'bytecode.lua',
    'compile.lua',
    'generator.lua',
    'lexer.lua',
    'id_generator.lua',
    'lua_ast.lua',
    'operator.lua',
    'parser.lua',
    'reader.lua',
]

luajit = find_program('luajit')

if bytecode_preload
    lang_bc_sources = []
    foreach lua_source : lang_sources
        source_basename = lua_source.split('.')[0]
        module_name = 'lang_' + source_basename.underscorify()
        message(module_name + ' ' + source_basename + '.c')
        lang_bc_sources += custom_target(source_basename + '.c',
            input: lua_source,
            output: '@BASENAME@.c',
            command: [luajit, '-b', '-n', module_name, '@INPUT@', '@OUTPUT@']
        )
    endforeach
    lang_bc_lib = static_library('langbc', lang_bc_sources)
else
    lua_module_install_dir = 'share/lua/5.1'
    install_data(lang_sources, install_dir: lua_module_install_dir + '/lang')
endif
