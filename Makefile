PREFIX  ?= /usr/local
DESTDIR ?=

.PHONY: all install clean

all install:
	CHICKEN_INSTALL_PREFIX="$(DESTDIR)$(PREFIX)" chicken-install

clean:
	find . -name "*.o" -delete
	find . -name "*.link" -delete
