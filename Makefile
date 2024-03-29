PROJECT := confluence

ARCH := $(shell uname -m)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"

default: static

deps:
	git config --global --add safe.directory /src
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build --optimized $(PROJECT)

static: clean
	docker run -t \
	-e GERBIL_PATH=/src/.gerbil \
	-e UID=$(id -u) \
	-e GID=$(id -g) \
	-e USER=$(USER) \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src build

clean:
	rm -rf .gerbil

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
