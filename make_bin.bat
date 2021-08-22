@echo off
cls

dlditool.exe -s0x02000000 "dstt.dldi" "arm9.bin"

del moonshl2.nds
ndstool.exe -c moonshl2.nds -r7 0x037f8000 -e7 0x037f8000 -r9 0x02000000 -e9 0x02000000 -7 arm7.bin -9 arm9.bin -b icon.bmp "MoonShell;Version 2.00 beta.5;RVCT3.1 [Build 569]"

dir moonshl2.nds
pause
make_bin.bat
