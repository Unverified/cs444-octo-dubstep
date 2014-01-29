#!/bin/bash
TEST_DIR="tests"
IN_TEST_DIR="$TEST_DIR/in"
OUT_TEST_DIR="$TEST_DIR/out"

make raco-make

echo ""
echo "Using input files located in: \"$IN_TEST_DIR/\""
echo "Using output files located in \"$OUT_TEST_DIR/\""
echo ""

for in_file in $IN_TEST_DIR/*
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
    echo "Test Failed! Compiler returned: $test_output, Expected: $expected_output"
    echo "Re-running test to debug flag and putting output in file: $TEST_DIR/$in_file_name.out"
    racket compiler.rkt -d $in_file > $TEST_DIR/$in_file_name.out
    exit
  fi

  echo ""
done

echo "ALL TESTS PASSED!"
