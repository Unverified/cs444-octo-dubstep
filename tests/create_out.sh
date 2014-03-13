#!/bin/bash

for file in in/a3/J1*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "0" > "out/a3/$file_name"
done

for file in in/a3/J2*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "0" > "out/a3/$file_name"
done

for file in in/a3/Je*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "42" > "out/a3/$file_name"
done
