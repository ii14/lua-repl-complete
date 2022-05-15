LUA_VERSION ?= luajit
LUA         ?= $(shell which $(LUA_VERSION))

all: test

test:
	@busted --lua="$(LUA)"

.PHONY: test
