#! /bin/bash

for a in output/*.s; do
	rm ${a}
done
cp stdlib/runtime.s output/

for a in output/*.s; do
	/u/cs444/bin/nasm -O1 -f elf -g -F dwarf ${a}	
done

ld -melf_i386 -o main output/*.o

