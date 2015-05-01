all: Main

# These must be in the right order--no forward refs
FILES = Enigma4.ml Decrypt.ml Main.ml

Main: $(FILES)
	corebuild -lib str Main.native

clean:
	rm -rf _build Main.native

