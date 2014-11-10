OBJECTS = animators.o batcher.o chunk.o drawing.o events.o font.o input.o misc.o node.o resources.o particles.o primitives.o scene.o scene-batcher.o shader.o sound.o sprite-batcher.o sprite.o texture.o tilemap.o trans.o window.o
CFLAGS = -Wno-cpp

all: coati.so

animators.o: animators.scm
	csc -c animators.scm -C $(CFLAGS)

batcher.o: batcher.scm chunk.scm shader.scm misc.scm
	csc -c batcher.scm -C $(CFLAGS)

chunk.o: chunk.scm
	csc -c chunk.scm -C $(CFLAGS)

coati.so: coati.scm $(OBJECTS)
	csc -s $(OBJECTS) coati.scm -o coati.so -C $(CFLAGS)

drawing.o: drawing.scm
	csc -c drawing.scm -C $(CFLAGS)

events.o: events.scm
	csc -c events.scm -C $(CFLAGS)

font.o: font.scm
	csc -c font.scm -C $(CFLAGS)

input.o: input.scm
	csc -c input.scm -C $(CFLAGS)

misc.o: misc.scm
	csc -c misc.scm -C $(CFLAGS)

node.o: node.scm trans.scm
	csc -c node.scm -C $(CFLAGS)

resources.o: resources.scm trans.scm
	csc -c resources.scm -C $(CFLAGS)

particles.o: particles.scm primitives.scm scene-batcher.scm
	csc -c particles.scm -C $(CFLAGS)

primitives.o: primitives.scm
	csc -c primitives.scm -C $(CFLAGS)

scene.o: scene.scm node.scm
	csc -c scene.scm -C $(CFLAGS)

scene-batcher.o: scene-batcher.scm node.scm scene.scm sprite-batcher.scm
	csc -c scene-batcher.scm -C $(CFLAGS)

shader.o: shader.scm
	csc -c shader.scm -C $(CFLAGS)

sound.o: sound.scm
	csc -c sound.scm -C $(CFLAGS)

sprite.o: sprite.scm primitives.scm misc.scm
	csc -c sprite.scm -C $(CFLAGS)

sprite-batcher.o: sprite-batcher.scm primitives.scm shader.scm sprite.scm trans.scm
	csc -c sprite-batcher.scm -C $(CFLAGS)

texture.o: texture.scm sprite.scm sprite-batcher.scm resources.scm misc.scm primitives.scm 
	csc -c texture.scm -C $(CFLAGS)

tilemap.o: tilemap.scm primitives.scm
	csc -c tilemap.scm -C $(CFLAGS)

trans.o: trans.scm primitives.scm sprite.scm
	csc -c trans.scm -C $(CFLAGS)

window.o: window.scm
	csc -c window.scm -C $(CFLAGS)

clean:
	rm -f *.o
	rm -f coati.so
