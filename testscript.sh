#!/bin/bash

# Usage: bash testscript.sh [-a]
# -a : do not stop the testscript if it encounters an error, if this flag is not included then the script will terminated on the first error it encounters

# The script will run the compiler on all the files found in tests/in/ directory and compare the output with the contents of the files found in tests/out/ with the same name. If it finds a difference then it will re-run the compiler with the debug flag set and place the input in tests/failed_out/

TEST_DIR="tests"

IN_TEST_DIR="$TEST_DIR/in"
OUT_TEST_DIR="$TEST_DIR/out"
FAILED_TEST_DIR="$TEST_DIR/failed_out"

# get the command-line arguments
files_to_run="*"
flag=""

if [ "$1" != "" ]
then
  if [[ "$1" == -* ]]
  then
    flag="$1"
  else
    files_to_run="$1"
  fi
fi

if [ "$2" != "" ]
then
  files_to_run="$2"
fi

make raco-make
rm -f tests/failed_out/*

echo ""
echo "Using input files located in: \"$IN_TEST_DIR/\""
echo "Using output files located in \"$OUT_TEST_DIR/\""
echo "Running tests on: $IN_TEST_DIR/$files_to_run"
echo ""

all_tests_passed=true

for in_file in $IN_TEST_DIR/$files_to_run
do
  dir_array=(`echo $in_file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  in_file_name=${dir_array[$in_file_pos]}
  out_file=$OUT_TEST_DIR/$in_file_name
  
  echo "Running compiler on: $in_file"

  test_output=`racket compiler.rkt $in_file`
  expected_output=`cat $out_file`

  if [ "$test_output" == "$expected_output" ]
  then
    echo "Test Passed!"
  else
    all_tests_passed=false

    echo "Test Failed! Compiler returned: $test_output, Expected: $expected_output"
    echo "Re-running test with debug flag and putting output in file: $FAILED_TEST_DIR/$in_file_name.out"
    
    in_file_contents=`cat $in_file`
    debug_output=`racket compiler.rkt -d $in_file` # > $FAILED_TEST_DIR/$in_file_name.out

    echo "$in_file_contents\n$debug_output" > $FAILED_TEST_DIR/$in_file_name.out

    if [ "$flag" != "-a" ]
    then
      exit
    fi
  fi

  echo ""
done

if [ $all_tests_passed == true ]
then
  echo "ALL TESTS PASSED!"
else
  echo "A TEST FAILED!"
fi
