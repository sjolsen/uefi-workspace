#!/usr/bin/python3

import dataclasses
import os
import os.path
import shutil
import subprocess
import sys
import tempfile


def copy_file(src: str, dst: str):
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    shutil.copy(src, dst)


@dataclasses.dataclass
class Env:
    workspace: str
    basetools: str


def getenv() -> Env:

    def get(key: str) -> str:
        try:
            return os.environ[key]
        except KeyError:
            raise RuntimeError(f'{key} is not set (did you run ". activate"?)')

    return Env(
        workspace=get('WORKSPACE'),
        basetools=get('EDK_TOOLS_PATH'))


def basetools(env: Env):
    make_args = ['make']
    make_args.extend(['-j', str(len(os.sched_getaffinity(0)))])
    make_args.extend(['-C', env.basetools])
    subprocess.run(make_args)


def run(env: Env):
    builddir = os.path.join(env.workspace, 'Build')
    os.makedirs(builddir, exist_ok=True)

    with tempfile.TemporaryDirectory(dir=builddir, prefix='_emulate') as tmpdir:
        bios = os.path.join(tmpdir, 'OVMF.fd')
        hda = os.path.join(tmpdir, 'hda')

        copy_file(os.path.join(builddir, 'OvmfX64/DEBUG_GCC5/FV/OVMF.fd'), bios)
        copy_file(
            os.path.join(builddir, 'Refinery/DEBUG_GCC5/X64/Refinery.efi'),
            os.path.join(hda, 'EFI/BOOT/BOOTx64.efi'))

        qemu_args = ['qemu-system-x86_64']
        qemu_args.extend(['-net', 'none'])
        qemu_args.extend(['-device', 'VGA,xres=800,yres=600'])
        qemu_args.extend(['-drive', f'if=pflash,unit=0,format=raw,file={bios}'])
        qemu_args.extend(['-drive', f'format=raw,file=fat:rw:{hda}'])
        subprocess.run(qemu_args)


def main(argv: list[str]) -> int:
    USAGE = 'Usage: emulate.py [basetools|ovmf|build|run]'
    env = getenv()

    match argv:
        case [_, 'basetools']:
            basetools(env)
        case [_, 'ovmf']:
            subprocess.run(['build', '-p', 'OvmfPkg/OvmfPkgX64.dsc'])
        case [_, 'build']:
            subprocess.run(['build', '-p', 'RefineryPkg/RefineryPkg.dsc'])
        case [_, 'run']:
            run(env)
        case _:
            print(USAGE, file=sys.stderr)
            return 1

    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
