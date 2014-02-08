#!/bin/bash

for file in tests/tests/J1*
do

  file_contents=`cat $file`

  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "Compiled" > "out/$file_name"
done

for file in tests/tests/Je*
do

  file_contents=`cat $file`

  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "Error" > "out/$file_name"
done
