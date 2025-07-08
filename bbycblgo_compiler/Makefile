.PHONY: build clean deps

# Build the Go application
build:
	@echo "Building Go application..."
	go build -o bbycblgo .

# Run tests
run: build
	./bbycblgo


# Clean generated files
clean:
	@echo "Cleaning up..."
	rm -rf parser
	rm -f bbycblgo
	rm -f antlr-4.13.1-complete.jar

# Install dependencies
deps:
	go mod tidy
	go mod download
