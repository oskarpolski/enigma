Tim 

all: Enigma4 Decrypt Main
    corebuild Engima4.native
    corebuild Decrypt.native
    corebuild Main.native
    
Engima4:
    corebuild Engima4.native
 
Decrypt:
    corebuild Decrypt.native
    
Main:
    corebuild Main.native
    
check:
	chmod u+x ../check_width
	../check_width Engima4.ml
	../check_width Decrypt.ml
	../check_width Main.ml
	
clean:
	rm -rf _build *.native
