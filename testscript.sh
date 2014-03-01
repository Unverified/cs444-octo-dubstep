#!/bin/bash

# Usage: bash testscript.sh [-a] [regex]
# -a : do not stop the testscript if it encounters an error, if this flag is not included then the script will terminated on the first error it encounters
# regex : a regular expression, so you can specify what files to run the test script on in tests/in (eg "bash testscript a*" will run all files in tests/in/ beginning with a). If no regex is supplied then all files in tests/in/ are run.

# The script will run the compiler on all the files found in tests/in/ directory and compare the output with the contents of the files found in tests/out/ with the same name. If it finds a difference then it will re-run the compiler with the debug flag set and place the input in tests/failed_out/

TEST_DIR="tests"

STDLIB=`find stdlib -iname "*.java"`
IN_TEST_DIR="$TEST_DIR/in"
OUT_TEST_DIR="$TEST_DIR/out"
FAILED_TEST_DIR="$TEST_DIR/failed_out"

# get the command-line arguments
files_to_run="$IN_TEST_DIR/*"
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

make

echo ""
echo "Using input files located in: \"$IN_TEST_DIR/\""
echo "Using output files located in \"$OUT_TEST_DIR/\""
echo "Running tests on: $files_to_run"
echo ""

for test in $files_to_run
do
  if [ -d "$test" ]				
  then
    test_files=`find $test -iname "*.java"`
  elif [[ "$test" == *.java ]]
  then
    test_files=$test
  else
    continue
  fi

  dir_array=(`echo $test | tr '/' ' '`)
  let test_name_pos=${#dir_array[@]}-1
  test_name=${dir_array[$test_name_pos]}
  out_file=$OUT_TEST_DIR/$test_name

  echo "Running compiler with files:"
  echo "$test_files"
  echo "$STDLIB"
    
  racket compiler.rkt $test_files $STDLIB # > $FAILED_TEST_DIR/$test_name.out
  test_output=$?
  expected_output=`cat $out_file`

  if [ "$test_output" == "$expected_output" ]
  then
    rm $FAILED_TEST_DIR/$test_name.out
    echo "Test Passed!"
  else
    echo "Test Failed! Compiler returned: $test_output, Expected: $expected_output"
    echo "Output in file: $FAILED_TEST_DIR/$in_file_name.out"

    if [ "$flag" != "-a" ]
    then
      exit
    fi
  fi

  echo ""
done

echo "DONE TESTING!"
