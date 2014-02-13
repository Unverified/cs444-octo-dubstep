all:
	raco make compiler.rkt
       
	echo '#!/bin/bash\nracket compiler.rkt $$1' > joosc

clean:
	rm -rf compiled compiler
