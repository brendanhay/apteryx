SHELL := /usr/bin/env bash
FLAGS := --disable-documentation --disable-library-coverage
DEPS  := vendor/amazonka vendor/wai-route

.PHONY: build clean test lint

all: deps build link

link:
	$(foreach dir, $(wildcard s3apt-*),ln -fs dist/build/$(dir)/$(dir) $(dir:s3apt-%=%);)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox $(OUT)
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

deps: add-sources
	cabal install -j $(FLAGS) --only-dependencies

add-sources: cabal.sandbox.config $(DEPS)
	$(foreach dir,$(DEPS),cabal sandbox add-source $(dir);)

cabal.sandbox.config:
	cabal sandbox init

vendor/wai-route:
	git clone https://github.com/romanb/$*.git $@

vendor/%:
	git clone https://github.com/brendanhay/$*.git $@

