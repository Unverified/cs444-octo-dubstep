rm -f grammar			# remove the old grammar file
racket grammar.rkt		# generate new grammar file
./lalr1 grammar > lrtable	# run table generator
mv lrtable ../
