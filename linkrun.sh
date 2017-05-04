./venture.native < test_files/test1.vtr > test1.ll
llc test1.ll > test1.s
cc -o test1.exe test1.s stdlib.o
./test1.exe

./venture.native < test_files/test2.vtr > test2.ll
llc test2.ll > test2.s
cc -o test2.exe test2.s stdlib.o
./test2.exe

./venture.native < test_files/test3.vtr > test3.ll
llc test3.ll > test3.s
cc -o test3.exe test3.s stdlib.o
./test3.exe

./venture.native < test_files/test4.vtr > test4.ll
llc test4.ll > test4.s
cc -o test4.exe test4.s stdlib.o
./test4.exe

./venture.native < test_files/test5.vtr > test5.ll
llc test5.ll > test5.s
cc -o test5.exe test5.s stdlib.o 
./test5.exe
