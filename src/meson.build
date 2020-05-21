lang_cflags = []

if host_machine.system() == 'darwin'
    lang_cflags += ['-pagezero_size', '10000', '-image_base', '100000000']
endif

lang_sources = ['language.c', 'language_loaders.c']

if bytecode_preload
    lang_whole_libs = [lang_bc_lib]
    lang_export_dynamic = true
else
    lang_whole_libs = []
    lang_export_dynamic = false
endif

luajit_dep = dependency('luajit')

liblang = static_library('ljlangtk',
    lang_sources,
    c_args: lang_cflags,
    dependencies: luajit_dep,
    install: true,
)

luajit_x = executable('luajit-x',
    'luajit-x.c',
    dependencies: luajit_dep,
    c_args: lang_cflags,
    link_with: liblang,
    link_whole: lang_whole_libs,
    export_dynamic: lang_export_dynamic,
    install: true,
)

liblang_dep = declare_dependency(
    include_directories: include_directories('.'),
    link_with: liblang,
    dependencies: luajit_dep,
)

install_headers('language.h', 'language_loaders.h')

pkg.generate(liblang,
    filebase : 'ljlangtk',
    name : 'LuaJIT Lang Toolkit',
    description : 'The LuaJIT Language Toolkit',
    url : 'https://github.com/franko/luajit-lang-toolkit',
)
