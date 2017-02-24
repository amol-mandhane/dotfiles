def FlagsForFile(filename, **kwargs):
    flags = [
        '-Wall',
        '-Wextra',
        '-Werror'
        '-pedantic',
        '-I',
    ]

    filetype = filename.split(".")[-1]

    if filetype == 'c':
        flags += ['-xc']
    elif filetype in ['cpp', 'cc']:
        flags += ['-xc++']
        flags += ['-std=c++11']
    elif filetype == 'objc':
        flags += ['-ObjC']
    else:
        flags = []

    return {
        'flags':    flags,
        'do_cache': True
    }
