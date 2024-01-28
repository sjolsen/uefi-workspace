#!/usr/bin/python3

import os.path
import subprocess
import sys

import common

USAGE = 'Usage: uncrustify.py FILES...'


def main(argv: list[str]):
    env = common.activate()
    files = argv[1:]

    plugin_dir = os.path.join(
        env.workspace,
        'edk2/.pytool/Plugin/UncrustifyCheck')
    uncrustify = os.path.join(
        plugin_dir,
        'mu-uncrustify-release_extdep/Linux-x86/uncrustify')
    config = os.path.join(plugin_dir, 'uncrustify.cfg')

    args = [uncrustify]
    args.extend(['-c', config])
    args.extend(['--replace', '--no-backup'])
    args.extend(files)
    subprocess.run(args, env=env.environ, check=True)

if __name__ == '__main__':
    main(sys.argv)
