VENTURE = "./venture.sh"

globallog=testall.log
rm -f $globallog

error=0
keep=0
globalerror=0

usage() {

	echo "Usage: run_tests.sh [options] [.vtr files]" 
	echo "-k    Keep intermediate files"
   	echo "-h    Print this help"
    	exit 1

}

SignalError() {

    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"

}

Compare() {

    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {

	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2

    }

}

Run() {
 
    echo $* 1>&2
    eval $* || {

	SignalError "$1 failed on $*"
	return 1

    }

}

RunFail() {

    echo $* 1>&2
    eval $* && {

	SignalError "failed: $* did not report an error"
	return 1

    }

    return 0

}

while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="test_files/test-*.vtr"
fi

for file in $files
do
 case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
	*fail-*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror
