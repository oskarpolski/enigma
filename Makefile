<<<<<<< HEAD
TIM,

MAKE A MAKEFILE. MAKE SURE EVERYTHING COMPILES. BELOW IS WHAT I FOUND IN THE MAKEFILE OF moogle
WE NEED SOMETHING SIMILAR

PLEASE, DO IT BY TONIGHT!


all: 

# These must be in the right order--no forward refs
FILES = order.ml dict.ml myset.ml graph.ml nodescore.ml util.ml \
	query.ml pagerank.ml crawl.ml moogle.ml

moogle: $(FILES)
	corebuild -lib str moogle.native
=======
Tim 
>>>>>>> e2521313f3406535a871f5c64f8cc73f4943d31b

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
