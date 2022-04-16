# ==============================================================================
# Initialization ===============================================================

install: ## Installs the project
	docker-compose build
	$(MAKE) npm CMD='install'
	$(MAKE) start_dev

# ==============================================================================
# Project updates tasks ========================================================
npm:
	docker-compose run --rm --user $$(id -u):$$(id -g) --entrypoint sh web -c "npm $(CMD)"

build:
	$(MAKE) npm CMD="run build"

load_data:
	$(MAKE) npm CMD="run load_data"

create_db:
	$(MAKE) npm CMD="run create_db"

drop_db:
	$(MAKE) npm CMD="run drop_db"

generate_migration:
	$(MAKE) npm CMD="run generate_migration $(NAME)"

migration_migrate:
	$(MAKE) npm CMD="run migrate"

lint:
	$(MAKE) npm CMD="run lint"

run_test:
	$(MAKE) npm CMD="run pre:test"
#$(MAKE) npm CMD="run test"

# ==============================================================================
# Servers (and other long-running processes) management ========================
start_dev:
	docker-compose up -d db
	docker-compose run --rm --user $$(id -u):$$(id -g) --service-ports --entrypoint sh web -c 'npm run dev'

run_in_background:
	$(MAKE) stop || true
	$(MAKE) build
	docker-compose up -d

stop:
	@docker-compose stop