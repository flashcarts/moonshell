@echo off

echo �N���[���R���p�C���̂��߂Ɉꎞ�t�@�C�����폜���܂��Bmake�̎������s�͂��܂���B
echo ���~����ꍇ��(Windows)����{�^�����������A(DOS)Ctrl+C�������ĉ������B
pause

rd /q /s arm9\build\

del arm9\arm9.bin
del arm9\arm9.so
del arm9\arm9.map
del arm9\specs\ds_arm9_mshlplg_crt0.o

del arm9\arm9_sort.map
del arm9\arm9_objdump_*.txt

