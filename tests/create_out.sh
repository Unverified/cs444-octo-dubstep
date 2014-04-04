#!/bin/bash

A=a1

for i in {1..5}
do
  for file in in/a$i/J1*
  do
    dir_array=(`echo $file | tr '/' ' '`)
    let in_file_pos=${#dir_array[@]}-1
    file_name=${dir_array[$in_file_pos]}

    echo "123" > "out/a$i/$file_name"
  done

  for file in in/a$i/J2*
  do
    dir_array=(`echo $file | tr '/' ' '`)
    let in_file_pos=${#dir_array[@]}-1
    file_name=${dir_array[$in_file_pos]}

    echo "123" > "out/a$i/$file_name"
  done

#  for file in in/a$i/Je*
#  do
#    dir_array=(`echo $file | tr '/' ' '`)
#    let in_file_pos=${#dir_array[@]}-1
#    file_name=${dir_array[$in_file_pos]}

#    echo "13" > "out/a$i/$file_name"
#  done

#  for file in in/a$i/J1e*
#  do
#    dir_array=(`echo $file | tr '/' ' '`)
#    let in_file_pos=${#dir_array[@]}-1
#    file_name=${dir_array[$in_file_pos]}

#    echo "13" > "out/a$i/$file_name"
#  done

done
