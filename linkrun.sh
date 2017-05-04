./venture.native < test_files/test.vtr > test.ll
llc test.ll > test.s
cc -o test.exe test.s input.o scompare.o
./test.exe

./venture.native < test_files/test2.vtr > test2.ll
llc test2.ll > test2.s
cc -o test2.exe test2.s input.o scompare.o
./test2.exe

./venture.native < test_files/test3.vtr > test3.ll
llc test3.ll > test3.s
cc -o test3.exe test3.s input.o scompare.o
./test3.exe

./venture.native < test_files/test5.vtr > test5.ll
llc test5.ll > test5.s
cc -o test5.exe test5.s input.o scompare.o
./test5.exe
