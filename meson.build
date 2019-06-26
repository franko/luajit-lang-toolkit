project('luajit-lang-toolkit', 'c', version : '1.0', default_options : 'c_std=c99')

pkg = import('pkgconfig')

bytecode_preload = get_option('preload')

lang_headers_dir = include_directories('.')

subdir('lang')
subdir('src')
