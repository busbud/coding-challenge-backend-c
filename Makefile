BASE_DIR = $(shell pwd)
APP_NAME = busbudcc
DKR_BIN = ./dkr

ERLANG_IMAGE = erlang:20.0.2

DOCKER_OPTS ?=
DOCKERIZE = @docker run --rm \
							 					--volume "$(BASE_DIR)":/app \
							 					--workdir "/app" \
							 					-it \
												$(DOCKER_OPTS) \
												$(ERLANG_IMAGE)

################################################################################
# Makefile API
################################################################################

.PHONY: deps
deps:
	$(DOCKERIZE) rebar3 as dev compile

.PHONY: tests
tests:
	$(DOCKERIZE) rebar3 ct

.PHONY: run
run: DOCKER_OPTS = --publish 9000:9000
run:
	$(DOCKERIZE) rebar3 as dev shell
