all: raco-exe
raco-exe:
	raco exe --gui compiler.rkt
raco-make:
	raco make compiler.rkt

clean:
	rm -rf compiled compiler
