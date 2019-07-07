PWD ?= pwd_unknown
APP_NAME = $(notdir $(PWD))
VERSION = prod

ifndef service
service = plumber
endif

# import config.
cnf ?= config.env
include $(cnf)
export $(shell sed 's/=.*//' $(cnf))
TAG_NAME=$(DOCKER_REPO)/$(APP_NAME)_$(service)


# pass variables for Docker to pick up.
export APP_NAME
export VERSION
export TAG_NAME

# HELP: This will output the help for each task
.PHONY: help

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.DEFAULT_GOAL := help

pull: ## Pull the image from docker.io
	docker-compose -p $(APP_NAME) pull $(service)

# Build the container
build: ## Build the release and develoment container. The development
	docker-compose -p $(APP_NAME) build --force-rm $(service)


build-nc: ## Build the release and develoment container. The development
	docker-compose -p $(APP_NAME) build --force-rm --no-cache $(service)

clean: ## Remove created images related to the project
	@docker-compose -p $(APP_NAME) down --remove-orphans --rmi all 2>/dev/null \
	&& echo 'Image(s) for "$(APP_NAME)" removed.' \
	|| echo 'Image(s) for "$(APP_NAME)" already removed.'

# Build and run the container
run: ## Run the service in the terminal
	docker-compose -p $(APP_NAME) run --rm --service-ports $(service)

up: ## Spin up the service as container
	docker-compose -p $(APP_NAME) up --no-build -d $(service)

start: stop ## Start service as container
	docker-compose -p $(APP_NAME) start $(service)

stop: ## Stop running container
	docker-compose -p $(APP_NAME) stop $(service)

down: stop ## Stop and remove running containers
	docker-compose -p $(APP_NAME) down

# Docker release - build, tag and push the container
release: build publish ## Make a release by building and publishing the `{version}` as `latest` tagged containers to the docker.io

release-nc: build-nc publish ## Make a release by building the container without caching the image, and publishing the `{version}` as `latest` tagged containers in the docker.io

# Docker publish
publish: publish-latest publish-version ## Publish the `{version}` as `latest` tagged containers to docker.io

publish-latest: tag-latest ## Publish the `latest` taged container to the docker.io
	@echo 'publish latest to docker.io/$(TAG_NAME):latest'
	docker push $(TAG_NAME):latest

publish-version: tag-version ## Publish the `{version}` taged container to the docker.io
	@echo 'publish $(VERSION) to docker.io/$(TAG_NAME):$(VERSION)'
	docker push $(TAG_NAME):$(VERSION)

# Docker tagging
tag: tag-latest tag-version ## Generate container tags for the `{version}` ans `latest` tags

tag-latest: ## Generate container `{version}` tag
	@echo 'create tag latest'
	docker tag $(TAG_NAME):$(VERSION) $(TAG_NAME):latest

tag-version: ## Generate container `latest` tag
	@echo 'create tag for version: $(VERSION)'
	docker tag $(TAG_NAME):$(VERSION) $(TAG_NAME):$(VERSION)

# HELPERS
prune: ## clean all that is not actively used
	docker system prune -af

version: ## output to version
	@echo $(VERSION)

