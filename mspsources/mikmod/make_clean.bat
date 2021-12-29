@echo off

echo クリーンコンパイルのために一時ファイルを削除します。makeの自動実行はしません。
echo 中止する場合は(Windows)閉じるボタンを押すか、(DOS)Ctrl+Cを押して下さい。
pause

rd /q /s arm9\build\

del arm9\arm9.bin
del arm9\arm9.so
del arm9\arm9.map
del arm9\specs\ds_arm9_mshlplg_crt0.o

del arm9\arm9_sort.map
del arm9\arm9_objdump_*.txt

