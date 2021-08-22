sort.exe arm9.map > arm9_sort.map
arm9_objdump.exe -a arm9.so > arm9_objdump_a.txt
arm9_objdump.exe -f arm9.so > arm9_objdump_f.txt
arm9_objdump.exe -p arm9.so > arm9_objdump_p.txt
arm9_objdump.exe -h arm9.so > arm9_objdump_h.txt
arm9_objdump.exe -x arm9.so > arm9_objdump_x.txt
arm9_objdump.exe -s arm9.so > arm9_objdump_s.txt
arm9_objdump.exe -S arm9.so > arm9_objdump_src.txt
