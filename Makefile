.PHONY: confluence


docker:
	docker build --rm=true -t confluence .
	docker tag confluence jaimef/confluence

push:
	docker push jaimef/confluence

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

test-remove-doc:

test-search:
	@echo -n "test-search: "
ifeq ('$(shell confluence search develop|grep develop)','')
	@echo FAIL
else
	@echo PASS
endif

test-update:

linux-static:
	docker run -e PATH=/usr/local/gambit/bin:/usr/local/gerbil/bin:/bin:/sbin:/usr/bin:/usr/sbin -e GERBIL_HOME=/root/gerbil -e GERBIL_PATH=/dd/.gerbil -v $(PWD):/dd -it jaimef/centos bash -c 'cd /dd && make linux-static-intern'

linux-static-intern:
	unset http_proxy https_proxy
	cd /dd && gxpkg link ober/confluence . || true
	gxpkg install github.com/ober/oberlib
#	gxpkg build ober/confluence
	gxc -o confluence-static -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -static -ld-options "-static -lpthread -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -exe confluence/confluence.ss
