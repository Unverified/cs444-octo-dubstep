#! /bin/bash

<<<<<<< HEAD
for a in output/*.o; do
	rm ${a}
done
=======
rm -f output/*.o
>>>>>>> 939c92cd12d7988163713f6e07bcaf9670f84739
cp stdlib/runtime.s output/

for a in output/*.s; do
	/u/cs444/bin/nasm -O1 -f elf -g -F dwarf ${a}	
done

ld -melf_i386 -o main output/*.o

