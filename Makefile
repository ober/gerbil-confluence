PROJECT := confluence

ARCH := $(shell uname -m)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"
PWD := $(shell pwd)
UID := $(shell id -u)
GID := $(shell id -g)

default: static

check-root:
	@if [ "${UID}" -eq 0 ]; then \
	git config --global --add safe.directory /src; \
	fi

deps:
	git config --global --add safe.directory /src
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps check-root
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build -R $(PROJECT)

static: clean
	docker run -t \
	-e GERBIL_PATH=/src/.gerbil \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src build

clean:
	rm -rf .gerbil manifest.ss

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
