#!/bin/bash

# Usage: bash testscript.sh [-s] [a# [files in a#] ]

# -s stops the compiler when a test fails
# a# tells the script what subfolder to look in for tests (ie a1, a2, a3, a4, a5), defaults to a3
# "files in a#" tells the script what files in a# to run 

# example: bash testscript.sh a2 "J1*" -> will run all the tests in tests/in/a1/ starting with J1

TEST_DIR="tests"

STDLIB=`find stdlib -iname "*.java"`
IN_TEST_DIR="$TEST_DIR/in"
OUT_TEST_DIR="$TEST_DIR/out"
FAILED_TEST_DIR="$TEST_DIR/failed_out"

A="a3"
files_to_run="*"
stop_on_fail=false

if [ "$1" != "" ]
then
  if [[ "$1" == -s ]]
  then
    stop_on_fail=true
    if [ "$2" != "" ]
    then
      A="$2"
    fi

    if [ "$3" != "" ]
    then
      files_to_run="$3"
    fi
  else
    A="$1"

    if [ "$2" != "" ]
    then
      files_to_run="$2"
    fi
  fi
fi


make
mkdir -p $FAILED_TEST_DIR/$A/

echo ""
echo "Using input files located in: \"$IN_TEST_DIR/$A/\""
echo "Using output files located in \"$OUT_TEST_DIR/$A/\""
echo "Running tests on: $IN_TEST_DIR/$A/$files_to_run"
echo ""

for test in $IN_TEST_DIR/$A/$files_to_run
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
  out_file=$OUT_TEST_DIR/$A/$test_name

  echo "Running compiler with files:"
  echo "$test_files"
  echo "$STDLIB"
    
  racket compiler.rkt $test_files  > $FAILED_TEST_DIR/temp.out
  test_output=$?
  expected_output=`cat $out_file`

  if [ "$test_output" == "$expected_output" ]
  then
    rm $FAILED_TEST_DIR/temp.out
    rm -f $FAILED_TEST_DIR/$A/$test_name
    echo "Test Passed!"
  else
    mv $FAILED_TEST_DIR/temp.out $FAILED_TEST_DIR/$A/$test_name
    echo "Test Failed! Compiler returned: $test_output, Expected: $expected_output"
    echo "Output in file: $FAILED_TEST_DIR/$A/$test_name"

    if [ "$stop_on_fail" == "true" ]
    then
      exit
    fi
  fi

  echo ""
done

echo "DONE TESTING!"
