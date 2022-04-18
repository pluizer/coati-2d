ALL: chipmunk.a

chipmunk.a:
	$(CC) -g -c -std=c99 *.c constraints/*.c -I./include -fPIC -DCP_USE_DOUBLES=0
	$(AR) rvs chipmunk.a *.o

clean:
	$(RM) -f *.o
	$(RM) -f chipmunk.a
