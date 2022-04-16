# ==============================================================================
# Initialization ===============================================================

install: ## Installs the project
	docker-compose build
	$(MAKE) npm CMD='install'
	$(MAKE) start_dev

# ==============================================================================
# Project updates tasks ========================================================
npm: ## Npm, ex usage make npm CMD='--version'
	docker-compose run --rm --user $$(id -u):$$(id -g) --entrypoint sh web -c "npm $(CMD)"

build: ## Builds js project
	$(MAKE) npm CMD="run build"

# ==============================================================================
# Servers (and other long-running processes) management ========================
start_dev: ## Starts the HTTP and Postgres servers
	docker-compose run --rm --user $$(id -u):$$(id -g) --service-ports --entrypoint sh web -c 'npm run dev'

run_in_background: # Runs project in background for production purpose
	$(MAKE) stop || true
	$(MAKE) build
	docker-compose up -d

stop: ## Stops all containers
	@docker-compose stop