PROJECT := confluence

ARCH := $(shell uname -m)
DOCKER_IMAGE := "gerbil/alpine:$(ARCH)"

default: linux-static-docker

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build $(PROJECT)

linux-static-docker: clean
	docker run -it \
	-e GERBIL_PATH=/src/.gerbil \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src build

linux-docker: clean
	docker run -it \
	-e GERBIL_PATH=/src/.gerbil \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src linux

linux: build
	./build.ss

clean:
	rm -rf .gerbil

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
