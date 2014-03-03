all:
	raco make compiler.rkt
       
	echo '#!/bin/bash\nracket compiler.rkt "$$@"' > joosc

clean:
	rm -rf compiled compiler
