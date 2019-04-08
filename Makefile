BASE_DIR = $(shell pwd)
APP_NAME = busbudcc
DKR_BIN = ./dkr
CONFIG_PATH ?= "$(BASE_DIR)/config/sys.config"
TEST_CONFIG_PATH ?= "$(BASE_DIR)/config/test.config"
DOCKER_CONFIG_PATH = "/sys.config"

ERLANG_IMAGE = erlang:20.0.2

DOCKER_OPTS ?=
DOCKERIZE = docker run --rm \
							 				 --volume "$(BASE_DIR)":/app \
											 --workdir "/app" \
							 				 -it

POSTGRES_CONTAINER_NAME = busbudcc_postgresql_container

################################################################################
# Makefile API
################################################################################

.PHONY: deps
deps:
	$(DOCKERIZE) $(ERLANG_IMAGE) rebar3 as dev compile

.PHONY: tests
tests:
	$(DOCKERIZE) $(ERLANG_IMAGE) rebar3 ct

.PHONY: run
run: CONFIG_PATH = $(TEST_CONFIG_PATH)
run:
	$(eval DATABASE_HOST := $(call get_config, database_host))
	$(DOCKERIZE) --publish 9000:9000 \
							 --link "$(POSTGRES_CONTAINER_NAME)":"$(DATABASE_HOST)" \
							 $(ERLANG_IMAGE) \
							 rebar3 as dev shell

################################################################################
## Internal helpers
################################################################################

define get_config
	$(shell $(DOCKERIZE) --volume="$(CONFIG_PATH)":"$(DOCKER_CONFIG_PATH)" \
											 $(ERLANG_IMAGE) \
											 /app/scripts/get_config $(DOCKER_CONFIG_PATH) $(1))
endef

################################################################################
## Local DB API
################################################################################

define start_db_container
	@echo "\`$(POSTGRES_CONTAINER_NAME)\` not running. Starting container..."
	$(eval DATABASE_USER := $(call get_config, database_user))
	$(eval DATABASE_PASSWORD := $(call get_config, database_password))
	$(eval DATABASE_NAME := $(call get_config, database_name))
	@docker run --name $(POSTGRES_CONTAINER_NAME) \
							--detach \
	 						--volume "$(BASE_DIR)":/app \
	 						--workdir /app \
							--env POSTGRES_USER=$(DATABASE_USER) \
							--env POSTGRES_PASSWORD=$(DATABASE_PASSWORD) \
							--env POSTGRES_DB=$(DATABASE_NAME) \
							--rm \
	 						mdillon/postgis:11
	@docker exec $(POSTGRES_CONTAINER_NAME) sleep 40
endef

define stop_db_container
	@docker kill $(POSTGRES_CONTAINER_NAME) 1>/dev/null
	@echo "\`$(POSTGRES_CONTAINER_NAME)\` stopped."
endef

.PHONY: db-start
db-start:
	$(eval CONFIG_PATH := $(TEST_CONFIG_PATH))
	$(if $(shell docker ps --filter "name=$(POSTGRES_CONTAINER_NAME)" --quiet), \
	     @echo "\`$(POSTGRES_CONTAINER_NAME)\` already running.", \
			 $(call start_db_container))
	$(eval DATABASE_USER := $(call get_config, database_user))
	$(eval DATABASE_PASSWORD := $(call get_config, database_password))
	$(eval DATABASE_NAME := $(call get_config, database_name))
	@docker exec $(POSTGRES_CONTAINER_NAME) \
						   ./shmig -t postgresql \
											 -l $(DATABASE_USER) \
											 -p $(DATABASE_PASSWORD) \
											 -d $(DATABASE_NAME) \
											 up

.PHONY: db-stop
db-stop:
	$(if $(shell docker ps --filter "name=$(POSTGRES_CONTAINER_NAME)" --quiet), \
			 $(call stop_db_container), \
			 @echo "\`$(POSTGRES_CONTAINER_NAME)\` not running.")

.PHONY: db-migrate-up
db-migrate-up:
	$(eval DATABASE_HOST := $(call get_config, database_host))
	$(eval DATABASE_USER := $(call get_config, database_user))
	$(eval DATABASE_PASSWORD := $(call get_config, database_password))
	$(eval DATABASE_NAME := $(call get_config, database_name))
	@docker run --volume "$(BASE_DIR)":/app \
							--workdir /app \
							--rm \
	 						mdillon/postgis:9.6 \
						  /bin/bash -c "./shmig -t postgresql \
																		-H $(DATABASE_HOST) \
																		-l $(DATABASE_USER) \
																		-p $(DATABASE_PASSWORD) \
																		-d $(DATABASE_NAME) \
																		up"
	@echo "Migrated \`$(DATABASE_HOST)\` to last version."

.PHONY: db-create-migration
db-create-migration:
	$(if $(MIGRATION_NAME), \
			 @echo "Creating migration \`$(MIGRATION_NAME)\`", \
			 @echo "MIGRATION_NAME not provided. Aborting.." && exit 1)
	./shmig -t postgresql -d unnecessary_name create $(MIGRATION_NAME)

.PHONY: db-import-cities
db-import-cities: CONFIG_PATH = $(TEST_CONFIG_PATH)
db-import-cities:
	@awk -F'\t' '{printf "%s\t%s\t%s\t%s\n", $$2, $$5, $$6, $$9}' data/cities_canada-usa.tsv >data/cities_canada-usa.tsv.tmp
	$(eval DATABASE_USER := $(call get_config, database_user))
	$(eval DATABASE_PASSWORD := $(call get_config, database_password))
	$(eval DATABASE_NAME := $(call get_config, database_name))
	@docker exec -it \
							 -e PGPASSWORD="$(DATABASE_PASSWORD)" \
									$(POSTGRES_CONTAINER_NAME) \
						   psql -U $(DATABASE_USER) \
										-d $(DATABASE_NAME) \
										-f data/import.sql
	@rm data/cities_canada-usa.tsv.tmp
