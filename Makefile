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
