#!/bin/bash

A=a1

for file in in/$A/J1*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "0" > "out/$A/$file_name"
done

for file in in/$A/J2*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "0" > "out/$A/$file_name"
done

for file in in/$A/Je*
do
  dir_array=(`echo $file | tr '/' ' '`)
  let in_file_pos=${#dir_array[@]}-1
  file_name=${dir_array[$in_file_pos]}

  echo "42" > "out/$A/$file_name"
done
