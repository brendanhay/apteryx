SHELL        := /usr/bin/env bash
NAME         := s3apt
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
DEB          := $(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
FLAGS        := --disable-documentation --disable-library-coverage
DEPS         := vendor/amazonka vendor/wai-route
BIN          := dist/build/$(NAME)/$(NAME)
OUT          := dist/$(NAME)

.PHONY: $(BIN) clean test lint

all: deps build

build: $(BIN)

dist: deps dist/$(DEB)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox $(OUT)
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

$(BIN):
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

$(OUT): $(BIN)
	strip -o $(OUT) $< && upx $<

%.deb: $(OUT)
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

vendor/wai-route:
	git clone https://github.com/romanb/$*.git $@

vendor/%:
	git clone https://github.com/brendanhay/$*.git $@

