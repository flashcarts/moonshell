@echo off
call setenv_devkitPror17.bat

:loop
cls
goto skipclean
rd /q /s arm9\build\

:skipclean

del arm9\arm9.bin
del arm9\arm9.so
del arm9\arm9.map
del arm9\specs\ds_arm9_mshlplg_crt0.o

del arm9\arm9_sort.map
del arm9\arm9_objdump_*.txt

make
if exist arm9\arm9.so goto run
pause
goto loop

:run

copy arm9\arm9.bin spc.msp

pause
goto loop

