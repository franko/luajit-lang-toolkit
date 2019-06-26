lang_cflags = []

if host_machine.system() == 'darwin'
    lang_cflags += ['-pagezero_size', '10000', '-image_base', '100000000']
endif

lang_sources = ['language.c', 'language_loaders.c']

if get_option('preload')
    lang_sources += 'language_bcloader.c'
    lang_cflags += '-DBC_PRELOAD'
endif

luajit_dep = dependency('luajit')

liblang = static_library('liblang',
    lang_sources + lang_bc_headers,
    include_directories: lang_headers_dir,
    c_args: lang_cflags,
    dependencies: luajit_dep,
    install: true,
)

luajit_x = executable('luajit-x',
    ['luajit-x.c'] + lang_bc_headers,
    include_directories: lang_headers_dir,
    dependencies: luajit_dep,
    c_args: lang_cflags,
    link_with: liblang,
    install: true,
)
