make clean && make

(./venture.native -c < test_files/test.vtr) > llvm2run.lli

lli llvm2run.lli
