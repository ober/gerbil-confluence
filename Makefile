PROJECT := confluence

DOCKER_IMAGE := "gerbil/alpine:latest"

$(eval UID := $(shell id -u))
$(eval GID := $(shell id -g))

default: linux-static-docker

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib
	/opt/gerbil/bin/gxpkg install github.com/yanndegat/colorstring

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build $(PROJECT)

linux-static-docker: clean
	docker run -it \
	-e GERBIL_PATH=/src/.gerbil \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src linux-static

linux-static: build
	/opt/gerbil/bin/gxc -o $(PROJECT)-bin -static \
	-cc-options "-Bstatic" \
	-ld-options "-static -lpthread -L/usr/lib64 -lssl -ldl -lyaml -lz" \
	-exe $(PROJECT)/$(PROJECT).ss

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
	rm -rf $(PROJECT)-bin .gerbil

install:
	mv $(PROJECT)-bin /usr/local/bin/$(PROJECT)
