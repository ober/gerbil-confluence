PROJECT := confluence
$(eval uid := $(shell id -u))
$(eval gid := $(shell id -g))

default: linux-static-docker

deps:
	$(GERBIL_HOME)/bin/gxpkg install github.com/ober/oberlib

build: deps
	$(GERBIL_HOME)/bin/gxpkg link $(PROJECT) /src || true
	$(GERBIL_HOME)/bin/gxpkg build $(PROJECT)

linux-static-docker:
	docker run -it \
	-e GERBIL_PATH=/tmp/.gerbil \
	-u "$(uid):$(gid)" \
	-v $(PWD):/src \
	jaimef/alpine-current:static \
	make -C /src linux-static

linux-static: build
	$(GERBIL_HOME)/bin/gxc -o $(PROJECT)-bin -static \
	-cc-options "-Bstatic" \
	-static \
	-ld-options "-static -lpthread -L/usr/lib64 -lssl -ldl -lyaml -lz" \
	-exe $(PROJECT)/$(PROJECT).ss

clean:
	rm -Rf $(PROJECT)-bin

install:
	mv $(PROJECT)-bin /usr/local/bin/$(PROJECT)

tests: test

test: test-search test-md2c

test-body:

test-config:

test-convert:

test-converter:

test-create:

test-info:

test-longtask:

test-md2c: $(eval tempfile := $(shell mktemp))
test-md2c:
	@echo -n "test-md2c: "
	@echo "### one\n> blockquote\n" > $(tempfile).md
	@echo "{toc}\nh3. one\n{quote} blockquote\n{quote}" > $(tempfile).chk
	@confluence md2c $(tempfile).md 2>&1 > /dev/null
	@if [ "$$(diff -q $(tempfile).cmd $(tempfile).chk; echo $$?)" = 0 ]; then\
		echo PASS;\
	else\
		echo FAIL;\
		diff -ru $(tempfile).chk $(tempfile).cmd; \
	fi
