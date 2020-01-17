docker:
	docker build --rm=true -t confluence .
	docker tag confluence jaimef/confluence

push:
	docker push jaimef/confluence

tests: test-search

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
	@echo "h3. one\n{quote} blockquote\n{quote}" > $(tempfile).chk
	@confluence md2c $(tempfile).md 2>&1 > /dev/null
	@if [ "$$(diff -q $(tempfile).cmd $(tempfile).chk; echo $$?)" = 0 ]; then\
		echo PASS;\
	else\
		echo FAIL;\
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
