OBJECTS = animators.o batcher.o blend.o camera.o chunk.o drawing.o events.o font.o input.o misc.o node.o node-pers.o pixmap.o  particles.o polygon-batcher.o primitives.o shader.o sound.o sprite-batcher.o sprite.o texture.o tilemap.o trans.o window.o triangulate.o
CFLAGS = -Wno-cpp -OS -C -fPIC

all: coati.so

animators.o: animators.scm
	csc -c animators.scm -C $(CFLAGS)

batcher.o: batcher.scm chunk.scm shader.scm misc.scm
	csc -c batcher.scm -C $(CFLAGS)

blend.o: blend.scm
	csc -c blend.scm -C $(CFLAGS)

camera.o: camera.scm primitives.scm window.scm
	csc -c camera.scm -C $(CFLAGS)

chunk.o: chunk.scm
	csc -c chunk.scm -C $(CFLAGS)

coati.so: coati.scm $(OBJECTS)
	csc -c++ -s -J $(OBJECTS) coati.scm -o coati.so -C $(CFLAGS) -C -Wno-write-strings
	csc -s coati.import.scm
	mv coati.so ..
	mv coati.import.so ..

drawing.o: drawing.scm
	csc -c drawing.scm -C $(CFLAGS)

events.o: events.scm
	csc -c events.scm -C $(CFLAGS)

font.o: font.scm misc.scm
	csc -c font.scm -C $(CFLAGS)

input.o: input.scm primitives.scm misc.scm window.scm
	csc -c input.scm -C $(CFLAGS)

misc.o: misc.scm
	csc -c misc.scm -C $(CFLAGS)

node.o: node.scm misc.scm primitives.scm trans.scm sprite-batcher.scm
	csc -c node.scm -C $(CFLAGS)

node-pers.o: node-pers.scm node.scm
	csc -c node-pers.scm -C $(CFLAGS)

pixmap.o: pixmap.scm primitives.scm texture.scm
	csc -c pixmap.scm -C $(CFLAGS)

particles.o: particles.scm primitives.scm
	csc -c particles.scm -C $(CFLAGS)

polygon-batcher.o: polygon-batcher.scm batcher.scm blend.scm camera.scm misc.scm primitives.scm shader.scm
	csc -c polygon-batcher.scm -C $(CFLAGS)

triangulate.o: triangulate.cc triangulate.h
	g++ -c triangulate.cc -fPIC

primitives.o: primitives.scm triangulate.cc
	csc -c++ -c primitives.scm -C -std=c++11 -C -Wno-write-strings -C $(CFLAGS)

shader.o: shader.scm
	csc -c shader.scm -C $(CFLAGS)

sound.o: sound.scm
	csc -c sound.scm -C $(CFLAGS)

sprite.o: sprite.scm primitives.scm misc.scm
	csc -c sprite.scm -C $(CFLAGS)

sprite-batcher.o: sprite-batcher.scm batcher.scm blend.scm camera.scm misc.scm primitives.scm shader.scm sprite.scm
	csc -c sprite-batcher.scm -C $(CFLAGS)

texture.o: texture.scm sprite.scm sprite-batcher.scm misc.scm primitives.scm 
	csc -c texture.scm -C $(CFLAGS)

tilemap.o: tilemap.scm camera.scm primitives.scm helpers.scm
	csc -c tilemap.scm -C $(CFLAGS)

trans.o: trans.scm primitives.scm sprite.scm
	csc -c trans.scm -C $(CFLAGS)

window.o: window.scm primitives.scm
	csc -c window.scm -C $(CFLAGS)

clean:
	rm -f *.o
	rm -f coati.so
