
all: Main

FILES = Enigma4.ml Decrypt.ml Main.ml

Engima4:
    corebuild Engima4.native

Decrypt:
    corebuild Decrypt.native
    
Main: $(FILES)
	corebuild Main.native
check:
 	chmod u+x ../check_width
	../check_width Enigma4.ml
 	../check_width Decrypt.ml
 	../check_width Main.ml	
 clean:
 	rm -rf _build *.native
