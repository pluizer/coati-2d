OBJECTS = batcher.o input.o misc.o node.o shader.o sprite-batcher.o sprite.o texture.o tilemap.o trans.o window.o
CFLAGS = -Wno-cpp

all: coati.so

batcher.o: batcher.scm misc.scm
	csc -c batcher.scm -C $(CFLAGS)

coati.so: coati.scm $(OBJECTS)
	csc -s $(OBJECTS) coati.scm -o coati.so -C $(CFLAGS)

input.o: input.scm
	csc -c input.scm -C $(CFLAGS)

misc.o: misc.scm
	csc -c misc.scm -C $(CFLAGS)

node.o: node.scm trans.scm
	csc -c node.scm -C $(CFLAGS)

shader.o: shader.scm
	csc -c shader.scm -C $(CFLAGS)

sprite.o: sprite.scm misc.scm
	csc -c sprite.scm -C $(CFLAGS)

sprite-batcher.o: sprite-batcher.scm shader.scm sprite.scm trans.scm
	csc -c sprite-batcher.scm -C $(CFLAGS)

texture.o: texture.scm misc.scm
	csc -c texture.scm -C $(CFLAGS)

tilemap.o: tilemap.scm
	csc -c tilemap.scm -C $(CFLAGS)

trans.o: trans.scm 
	csc -c trans.scm -C $(CFLAGS)

window.o: window.scm
	csc -c window.scm -C $(CFLAGS)

clean:
	rm -f *.o
	rm -f coati.so
