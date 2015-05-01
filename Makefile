<<<<<<< HEAD
all: Main
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
TIM,

MAKE A MAKEFILE. MAKE SURE EVERYTHING COMPILES. BELOW IS WHAT I FOUND IN THE MAKEFILE OF moogle
WE NEED SOMETHING SIMILAR

PLEASE, DO IT BY TONIGHT!


all: 
>>>>>>> 508986f927c26a1a44d7b6e15d52a711e914a5fe

# These must be in the right order--no forward refs
FILES = Enigma4.ml Decrypt.ml Main.ml

<<<<<<< HEAD
Main: $(FILES)
	corebuild -lib str Main.native

=======
moogle: $(FILES)
	corebuild -lib str moogle.native
=======
Tim 
>>>>>>> e2521313f3406535a871f5c64f8cc73f4943d31b
>>>>>>> 3e85815675b905742ca741009adc12b288f59f00

all: Main

FILES = Enigma4.ml Decrypt.ml Main.ml

Engima4:
	corebuild Enigma4.native
Decrypt:
	corebuild Decrypt.native
Main: $(FILES)
	corebuild Main.native
check:
	chmod u+x ../check_width
	../check_width Enigma4.ml
	../check_width Decrypt.ml
	../check_width Main.ml
>>>>>>> 508986f927c26a1a44d7b6e15d52a711e914a5fe
clean:
	rm -rf _build Main.native

