#!/usr/bin/python3

import ast
import dataclasses
import os
import os.path
import shutil
import subprocess
import sys
import tempfile

USAGE = 'Usage: emulate.py [basetools|ovmf|build|buildall|run]'


def copy_file(src: str, dst: str):
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    shutil.copy(src, dst)


@dataclasses.dataclass
class Env:
    environ: dict[str, str]
    workspace: str
    basetools: str


def activate() -> Env:
    parent = os.path.dirname(os.path.abspath(__file__))
    script = os.path.join(parent, 'activate')

    print_env = 'python3 -c "import os; print(dict(os.environ))"'
    command = f'. {script} >/dev/null; {print_env}'
    cp = subprocess.run(
        ['sh', '-c', command],
        cwd=parent,
        capture_output=True,
        text=True,
        check=True)
    environ = ast.literal_eval(cp.stdout)

    return Env(
        environ=environ,
        workspace=environ['WORKSPACE'],
        basetools=environ['EDK_TOOLS_PATH'])


def basetools(env: Env):
    make_args = ['make']
    make_args.extend(['-j', str(len(os.sched_getaffinity(0)))])
    make_args.extend(['-C', env.basetools])
    subprocess.run(make_args, env=env.environ, check=True)


def ovmf(env: Env):
    args = ['build', '-p', 'OvmfPkg/OvmfPkgX64.dsc']
    subprocess.run(args, env=env.environ, check=True)


def build(env: Env):
    args = ['build', '-p', 'RefineryPkg/RefineryPkg.dsc']
    subprocess.run(args, env=env.environ, check=True)


def buildall(env: Env):
    basetools(env)
    ovmf(env)
    build(env)


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
        subprocess.run(qemu_args, env=env.environ, check=True)


def main(argv: list[str]) -> int:
    env = activate()
    match argv:
        case [_, 'basetools']:
            basetools(env)
        case [_, 'ovmf']:
            ovmf(env)
        case [_, 'build']:
            build(env)
        case [_, 'buildall']:
            buildall(env)
        case [_, 'run']:
            run(env)
        case _:
            print(USAGE, file=sys.stderr)
            return 1
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
