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


def main(argv: list[str]) -> int:
    env = common.activate()
    match argv:
        case [_, 'run']:
            run(env)
        case [_, 'debug']:
            run(env, debug=True)
        case _:
            print(USAGE, file=sys.stderr)
            return 1
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
