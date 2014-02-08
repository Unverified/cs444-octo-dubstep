#!/bin/bash

# This script downloads all the example joos1w programs from the Joos Languages
# spec located at the url assigned below. It then strips out the html tags and
# places it in the tests/in/ directory so the testscript can test it

BASE_URL="https://www.student.cs.uwaterloo.ca/~cs444/features"
CS444_FILES_TO_GET=`cat cs444_files_to_get.txt`

for file in `echo $CS444_FILES_TO_GET | tr '\n' ' '`
do
  wget -q $BASE_URL/$file.html

  echo "Getting: $BASE_URL/$file.html"

  file_contents=`cat $file.html`

  new_file_name=$file"CS"

  file_contents=${file_contents/"<pre>"/""}
  file_contents=${file_contents/"</pre>"/""}
  file_contents=${file_contents//"<font color=blue>"/""}
  file_contents=${file_contents//"<font color=\"blue\">"/""}
  file_contents=${file_contents//"</font>"/""}
  file_contents=${file_contents//"class A"/"class $new_file_name"}
  file_contents=${file_contents//"interface A"/"interface $new_file_name"}
  file_contents=${file_contents//"public A("/"public $new_file_name("}
  file_contents=${file_contents//"&gt;"/">"}
  file_contents=${file_contents//"&lt;"/"<"}
  file_contents=${file_contents//"&amp;"/"&"}

  rm $file.html

  echo $file_contents > "in/$new_file_name"
  echo "Compiled" > "out/$new_file_name"
done
