call set_bccenv.bat
@echo off
cls
del *.obj
rem bcc32.exe -c -O2 -5 gbalzss16palenc_static.c
rem copy gbalzss16palenc_static.obj ..
bcc32.exe -c -O2 -5 gbalzss16enc_static.c
copy gbalzss16enc_static.obj ..
pause
make.bat
