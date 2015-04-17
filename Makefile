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

clean:
	rm -rf _build moogle.native
