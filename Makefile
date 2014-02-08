all:
	raco make compiler.rkt
        
	echo 'racket compiler.rkt $$1' > joosc

clean:
	rm -rf compiled compiler
