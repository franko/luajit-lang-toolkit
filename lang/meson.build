lang_sources = ['ast-boolean-const-eval.lua', 'ast-const-eval.lua', 'bcread.lua', 'bcsave.lua', 'bytecode.lua', 'compile.lua', 'generator.lua', 'lexer.lua', 'id-generator.lua', 'lua-ast.lua', 'operator.lua', 'parser.lua', 'reader.lua']

luajit = find_program('luajit')

bc_generator = generator(luajit,
    arguments: ['-b', '@INPUT@', '@OUTPUT@'],
    output  : '@BASENAME@.h',
)

lang_bc_headers = bc_generator.process(lang_sources)
