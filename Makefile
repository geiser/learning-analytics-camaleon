version =  3.7
services_plumber_build_context =  ./docker/plumber
services_plumber_build_target =  prod
services_plumber_image =  geiser/learning-analytics-camaleon_plumber
services_plumber_container_name =  learning-analytics-camaleon_plumber
services_shiny_build_context =  ./docker/shiny
services_shiny_build_target =  prod
services_shiny_image =  geiser/learning-analytics-camaleon_shiny
services_shiny_container_name =  learning-analytics-camaleon_shiny

PREFIX = /Users/gcc/learning-analytics-camaleon
APP_NAME = learning-analytics-camaleon
VERSION = prod

DD = /usr/local/bin/docker
DC = /usr/local/bin/docker-compose

# HELP: This will output the help for each task
.PHONY: all

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.DEFAULT_GOAL := help

config.yml:
	@echo 'config.yml is necessary copying from config.default.yml'
	cp config.default.yml config.yml

all: pull build ## Pull and build the images to the project

pull: ## Pull images from docker.io to the project
	$(DC) -p $(APP_NAME) pull $(service)

build: ## Build imagens to the project
	$(DC) -p $(APP_NAME) build --force-rm $(service)

build-nc: ## Build imagens to the project without using cache
	$(DC) -p $(APP_NAME) build --force-rm --no-cache $(service)

clean: ## Remove containers and images related to the project
	@$(DC) -p $(APP_NAME) down --remove-orphans --rmi all 2>/dev/null \
	&& echo 'Image(s) for "$(APP_NAME)" removed.' \
	|| echo 'Image(s) for "$(APP_NAME)" already removed.'
	@rm docker-compose.service
	@rm Makefile

run: config.yml ## Run a project service in the terminal
ifndef service
	$(error service variable is not set. Use 'make run service={service}')
else
	$(DC) -p $(APP_NAME) run --rm --service-ports $(service)
endif

up: config.yml ## Spin up the project services as containers
	$(DC) -p $(APP_NAME) up --no-build -d $(service)

start: stop ## Start the project services as containers
	$(DC) -p $(APP_NAME) start $(service)

stop: ## Stop the project services running as container
	$(DC) -p $(APP_NAME) stop $(service)

down: stop ## Stop and remove the project services running as containers
	$(DC) -p $(APP_NAME) down -v

# Docker release - build, tag and push the container
release: build publish ## Make a release by building and publishing the `{version}` as `latest` tagged containers to the docker.io

release-nc: build-nc publish ## Make a release by building the container without caching the image, and publishing the `{version}` as `latest` tagged containers in the docker.io

# Docker publish
publish: publish-latest publish-version ## Publish the `{version}` as `latest` tagged containers to docker.io

publish-latest: tag-latest ## Publish the `latest` taged container to the docker.io
ifndef service
	$(error service variable is not set. Use 'make publish-latest service={service}')
else
	@echo 'publish latest to docker.io/$(services_$(service)_image):latest'
	$(DD) push $(services_$(service)_image):latest
endif

publish-version: tag-version ## Publish the `{version}` taged container to the docker.io
ifndef service
	$(error service variable is not set. Use 'make publish-version service={service}')
else
	@echo 'publish $(VERSION) to docker.io/$(services_$(service)_image):$(VERSION)'
	$(DD) push $(services_$(service)_image):$(VERSION)
endif

# Docker tagging
tag: tag-latest tag-version ## Generate container tags for the `{version}` ans `latest` tags

tag-latest: ## Tagging imagen of a service with the `latest` tag, use: make service={service} tag-latest
ifndef service
	$(error service variable is not set. Use 'make tag-latest service={service}')
else
	@echo 'create tag latest for $(services_$(service)_image)'
	$(DD) tag $(services_$(service)_image):$(VERSION) $(services_$(service)_image):latest
endif

tag-version: ## Tagging imagen of a service with the `{version}` tag, use: make service={service} tag-version
ifndef service
	$(error service variable is not set. Use 'make tag-version service={service}')
else
	@echo 'create tag version for $(services_$(service)_image)'
	$(DD) tag $(services_$(service)_image):$(VERSION) $(services_$(service)_image):$(VERSION)
endif

# Docker installing as systemd
install: all  ## Install a service of the project as systemd
ifndef service
	$(error service variable is not set. Use 'make install service={service}')
else
	@mkdir -p /etc/docker/compose/$(APP_NAME)
	@cp -v docker-compose.yml /etc/docker/compose/$(APP_NAME)
	@cp -v docker-compose.service /etc/systemd/system/$(APP_NAME)@.service
endif

# HELPERS
prune: ## clean all that is not actively used
	$(DD) system prune -af

version: ## output to version
	@echo $(VERSION)
