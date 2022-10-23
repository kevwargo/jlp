.PHONY: build
build:
	rm -rf target && mvn package

.PHONY: install
install: build
	mvn install
