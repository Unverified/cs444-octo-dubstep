CLASS_PATH = bin/

all: lr lalr
lr:
	javac -d $(CLASS_PATH) -cp $(CLASS_PATH) Jlr1.java
	jar cvfm lr1 lr_manifest.txt $(CLASS_PATH)
	chmod 755 lr1
lalr:
	javac -d $(CLASS_PATH) -cp $(CLASS_PATH) Jlalr1.java
	jar cvfm lalr1 lalr_manifest.txt $(CLASS_PATH)
	chmod 755 lalr1
