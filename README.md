uefi-workspace
==============

This is a development environment for building and testing
[Refinery](https://github.com/sjolsen/refinery), a UEFI application targeting
TianoCore EDK II. Source control is handled through Git submodules, and common
development tasks are implemented as command-line verbs via the
increasingly-inaccurately named `emulate.py`. It _should_ be possible to
bootstrap at least the QEMU simulation using just `git clone` and `emulate.py`
on a modern Linux system.

Source-code formatting is handled through `uncrustify.py`. This requires some
hinky .NET nonsense that is *technically* documented in the EDK II developer's
guide but which I haven't written down and don't remember. Sorry :)

The most interesting command for a casual observer will be `emulate.py buildall`
followed by `emulate.py run`. This will build the Refinery application and OVMF
firmware for QEMU, set up a boot disk and BIOS ROM, and boot QEMU. The resulting
application is not that interesting at the moment, but it does demonstrate a few
things:

- Rendering character-based graphics to the UEFI text console
- Keyboard input (arrow keys change the color scheme)
- Mouse input (you can move the cursor but not much else). This involves
  building a standalone UEFI driver (`usb-mouse`) and loading it dynamically
  from the EFI System Partition. I could have just enabled the driver statically
  in OVMF but this is more fun >:3
