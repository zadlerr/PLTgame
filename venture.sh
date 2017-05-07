./venture.native < $1 > code.ll
llc code.ll > code.s
cc -o code.exe code.s stdlib.o
./code.exe
