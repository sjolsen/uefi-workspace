#!/usr/bin/python3

import os
import os.path
import shutil
import subprocess
import sys
import tempfile

import common

USAGE = 'Usage: emulate.py [basetools|ovmf|build|buildall|run|debug|test]'


def copy_file(src: str, dst: str):
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    shutil.copy(src, dst)


def basetools(env: common.Env):
    make_args = ['make']
    make_args.extend(['-j', str(len(os.sched_getaffinity(0)))])
    make_args.extend(['-C', env.basetools])
    env.run(make_args)


def ovmf(env: common.Env):
    env.run(['build', '-p', 'OvmfPkg/OvmfPkgX64.dsc'])


def build(env: common.Env):
    env.run(['build', '-p', 'RefineryPkg/RefineryPkg.dsc'])


def buildall(env: common.Env):
    basetools(env)
    ovmf(env)
    build(env)


def run(env: common.Env, debug: bool = False):
    builddir = os.path.join(env.workspace, 'Build')
    os.makedirs(builddir, exist_ok=True)

    with tempfile.TemporaryDirectory(dir=builddir, prefix='_emulate') as tmpdir:
        bios = os.path.join(tmpdir, 'OVMF.fd')
        hda = os.path.join(tmpdir, 'hda')

        copy_file(os.path.join(builddir, 'OvmfX64/DEBUG_GCC/FV/OVMF.fd'), bios)
        copy_file(
            os.path.join(builddir, 'Refinery/DEBUG_GCC/X64/Refinery.efi'),
            os.path.join(hda, 'EFI/BOOT/BOOTx64.efi'))
        copy_file(
            os.path.join(builddir, 'Refinery/DEBUG_GCC/X64/UsbMouseDxe.efi'),
            os.path.join(hda, 'EFI/Drivers/USBMouseDxe.efi'))

        qemu_args = ['qemu-system-x86_64']
        qemu_args.extend(['-net', 'none'])
        qemu_args.extend(['-device', 'VGA,xres=640,yres=480'])
        qemu_args.extend(['-display', 'gtk,gl=es'])
        qemu_args.extend(['-usb'])
        qemu_args.extend(['-device', 'usb-mouse'])
        qemu_args.extend(['-drive', f'if=pflash,unit=0,format=raw,file={bios}'])
        qemu_args.extend(['-drive', f'format=raw,file=fat:rw:{hda}'])

        if debug:
            debug_file = os.path.join(env.workspace, 'debug.log')
            qemu_args.extend(['-debugcon', f'file:{debug_file}'])
            qemu_args.extend(['-global', 'isa-debugcon.iobase=0x402'])

        env.run(qemu_args)


def test(env: common.Env, arch: str = 'X64'):
    env.run(['build', '-p', 'BoraxPkg/BoraxPkg.dsc', '-a', arch])
    builddir = os.path.join(env.workspace, 'Build')
    test_base = os.path.join(builddir, f'Borax/DEBUG_GCC/{arch}')
    test_file = os.path.join(
        env.workspace,
        f'refinery/BoraxPkg/Test/BoraxVirtualMachineTest/TestFile{arch}.bxo')
    test_bin = os.path.join(test_base, 'BoraxVirtualMachineTest')
    env.run(['valgrind', '--error-exitcode=1', test_bin, test_file])


def main(argv: list[str]) -> int:
    env = common.activate()
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
        case [_, 'debug']:
            run(env, debug=True)
        case [_, 'test']:
            test(env, arch='IA32')
            test(env, arch='X64')
        case _:
            print(USAGE, file=sys.stderr)
            return 1
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
