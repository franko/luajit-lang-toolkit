lang_cargs = []

if host_machine.system() == 'darwin'
    lang_cargs += ['-pagezero_size', '10000', '-image_base', '100000000']
endif

lang_sources = ['language.c', 'language_loaders.c']

if get_option('preload')
  lang_sources += 'language_bcloader.c'
  lang_cargs += '-DBC_PRELOAD'
endif

luajit_dep = dependency('luajit')

liblang = static_library('liblang',
    lang_sources,
    include_directories: lang_headers_dir,
    c_args: lang_cargs,
    dependencies: luajit_dep,
)

luajit_x = executable('luajit-x',
    'luajit-x.c',
    include_directories: lang_headers_dir,
    dependencies: luajit_dep,
    c_args: lang_cargs,
    link_with: liblang
)
