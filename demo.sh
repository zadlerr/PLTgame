./venture.native < test_files/demo.vtr > demo.ll
llc demo.ll > demo.s
cc -o demo.exe demo.s stdlib.o
./demo.exe
