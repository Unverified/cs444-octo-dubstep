#!/bin/bash

A=a5

for file in in/$A/J1*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "123" > "out/$A/$file_name"
done

for file in in/$A/J2*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "123" > "out/$A/$file_name"
done

for file in in/$A/Je*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "-1" > "out/$A/$file_name"
done
