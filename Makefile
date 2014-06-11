SHELL        := /usr/bin/env bash
NAME         := apteryx
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
DEB          := $(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
FLAGS        := --disable-documentation --disable-library-coverage
DEPS         := vendor/amazonka vendor/bzlib-conduit
BIN          := dist/build/$(NAME)/$(NAME)

.PHONY: build clean test lint

all: deps build link

link:
	$(foreach dir, $(wildcard apteryx-*),ln -fs dist/build/$(dir)/$(dir) $(dir:apteryx-%=%);)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

dist: dist/$(DEB)

%.deb: deps build
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD_NUMBER) \
	 --architecture=amd64 \
	 --output-dir=dist

deps: add-sources
	cabal install -j $(FLAGS) --only-dependencies

add-sources: cabal.sandbox.config $(DEPS)
	$(foreach dir,$(DEPS),cabal sandbox add-source $(dir);)

cabal.sandbox.config:
	cabal sandbox init

vendor/bzlib-conduit:
	git clone https://github.com/snoyberg/bzlib-conduit.git $@

vendor/%:
	git clone https://github.com/brendanhay/$*.git $@
