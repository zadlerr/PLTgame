make clean && make

(./venture.native -c < test_files/test.vtr) > llvm2run.lli
(./venture.native -c < test_files/test2.vtr) > llvm2run2.lli
(./venture.native -c < test_files/test3.vtr) > llvm2run3.lli
(./venture.native -c < test_files/test4.vtr) > llvm2run4.lli

lli llvm2run.lli
lli llvm2run2.lli
lli llvm2run3.lli
lli llvm2run4.lli
