.PHONY: build clean deps run antlr

ANTLR_JAR = antlr-4.13.1-complete.jar
ANTLR_URL = https://www.antlr.org/download/$(ANTLR_JAR)
GRAMMAR = bbyCBL.g4
PARSER_DIR = parser

# Build the Go application
build: antlr
	@echo "Building Go application..."
	go build -o bbycblgo .

# Run the built binary
run: build
	./bbycblgo

# Clean generated files
clean:
	@echo "Cleaning up..."
	rm -rf $(PARSER_DIR)
	rm -f bbycblgo
	rm -f $(ANTLR_JAR)

# Install Go dependencies
deps:
	go mod tidy
	go mod download

# Generate ANTLR parser
antlr: $(ANTLR_JAR)
	@echo "Generating ANTLR parser..."
	java -jar $(ANTLR_JAR) -Dlanguage=Go -o $(PARSER_DIR) -visitor $(GRAMMAR)

$(ANTLR_JAR):
	@echo "Downloading ANTLR jar..."
	@curl -O $(ANTLR_URL)

