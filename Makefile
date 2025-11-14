.PHONY: help test test-unit test-e2e test-docker build build-docker build-local clean clean-docker clean-all dev dev-docker

# Default target
.DEFAULT_GOAL := help

# Docker image names
TEST_IMAGE := die-trying-test
BUILD_IMAGE := die-trying

help: ## Show this help message
	@echo "Die Trying - Static Site Generator"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z0-9_-]+:.*## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*## "}; {printf "  %-20s %s\n", $$1, $$2}'

test: test-docker ## Run all tests in Docker (default)

test-unit: ## Run only unit tests (requires local SBCL)
	@echo "Running unit tests..."
	@sbcl --non-interactive \
		--load ~/quicklisp/setup.lisp \
		--eval "(push #P\"$(PWD)/\" asdf:*central-registry*)" \
		--eval "(asdf:load-asd (merge-pathnames \"die-trying.asd\" #P\"$(PWD)/\"))" \
		--eval "(ql:quickload :die-trying-tests :silent t)" \
		--eval "(fiveam:run! 'die-trying-tests::vector-to-list-test)" \
		--eval "(fiveam:run! 'die-trying-tests::vector-empty-p-test)" \
		--eval "(fiveam:run! 'die-trying-tests::topological-sort-test)" \
		--eval "(fiveam:run! 'die-trying-tests::categorize-file-test)" \
		--eval "(fiveam:run! 'die-trying-tests::get-out-path-test)" \
		--eval "(fiveam:run! 'die-trying-tests::build-fragment-objects-test)" \
		--eval "(fiveam:run! 'die-trying-tests::find-dependencies-test)"

test-e2e: ## Run only e2e tests (requires local SBCL)
	@echo "Running e2e tests..."
	@sbcl --non-interactive \
		--load ~/quicklisp/setup.lisp \
		--eval "(push #P\"$(PWD)/\" asdf:*central-registry*)" \
		--eval "(asdf:load-asd (merge-pathnames \"die-trying.asd\" #P\"$(PWD)/\"))" \
		--eval "(ql:quickload :die-trying-tests :silent t)" \
		--eval "(fiveam:run! 'die-trying-tests::e2e-simple-build-test)" \
		--eval "(fiveam:run! 'die-trying-tests::e2e-nested-fragments-test)" \
		--eval "(fiveam:run! 'die-trying-tests::e2e-multiple-templates-test)" \
		--eval "(fiveam:run! 'die-trying-tests::e2e-asset-copying-test)"

test-docker: ## Run all tests in Docker container
	@echo "Building test image..."
	@docker build -t $(TEST_IMAGE) -f Dockerfile.test . -q
	@echo "Running tests..."
	@docker run --rm $(TEST_IMAGE)

build: build-docker ## Build the site (Docker)

build-docker: ## Build the site using Docker
	@echo "Building site with Docker..."
	@docker build -t $(BUILD_IMAGE) .
	@echo "Extracting output from Docker container..."
	@rm -rf out/
	@docker create --name die-trying-builder $(BUILD_IMAGE)
	@docker cp die-trying-builder:/app/out ./out
	@docker rm die-trying-builder
	@echo "Build complete! Site generated in ./out/"

build-local: ## Build the site locally (requires local SBCL)
	@echo "Building site locally..."
	@sbcl --script build.lisp
	@echo "Build complete! Site generated in ./out/"

clean: ## Clean generated files (out/, test artifacts)
	@echo "Cleaning generated files..."
	@rm -rf out/
	@rm -f main.fasl
	@rm -rf /tmp/die-trying-test-*
	@echo "Clean complete!"

clean-docker: ## Remove Docker images
	@echo "Removing Docker images..."
	@docker rmi -f $(TEST_IMAGE) 2>/dev/null || true
	@docker rmi -f $(BUILD_IMAGE) 2>/dev/null || true
	@echo "Docker images removed!"

clean-all: clean clean-docker ## Clean everything (files + Docker images)
	@echo "Everything cleaned!"

dev: ## Start development server (requires local SBCL)
	@echo "Starting development server..."
	@sbcl --load ~/quicklisp/setup.lisp \
		--eval "(push #P\"$(PWD)/\" asdf:*central-registry*)" \
		--eval "(ql:quickload :die-trying :silent t)" \
		--eval "(die-trying:start-dev)"

dev-docker: ## Start development server in Docker with www/ mounted
	@echo "Ensuring Docker image is built..."
	@docker build -t $(BUILD_IMAGE) . -q > /dev/null || docker build -t $(BUILD_IMAGE) .
	@echo "Starting development server in Docker..."
	@echo "Server will be available at http://localhost:4321"
	@echo "Watching www/ for changes..."
	@echo "Press Ctrl+C to stop"
	@docker run --rm -it \
		-v "$(PWD)/www:/app/www" \
		-v "$(PWD)/out:/app/out" \
		-p 4321:4321 \
		$(BUILD_IMAGE) \
		sbcl --load ~/quicklisp/setup.lisp \
			--eval "(push #P\"/app/\" asdf:*central-registry*)" \
			--eval "(ql:quickload :die-trying :silent t)" \
			--eval "(die-trying:start-dev)"

