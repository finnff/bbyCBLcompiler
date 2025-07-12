.PHONY: build clean deps run antlr

# ANTLR Configuration
ANTLR_JAR = antlr-4.13.1-complete.jar
ANTLR_URL = https://www.antlr.org/download/$(ANTLR_JAR)
GRAMMAR = bbyCBL.g4
PARSER_DIR = parser

# LLVM Version Detection
LLVM_VERSION := $(shell llvm-config --version 2>/dev/null | cut -d. -f1)
LLVM_TAG := llvm$(LLVM_VERSION)
BUILD_FLAGS := -tags=$(LLVM_TAG)

# Check if LLVM is available
ifeq ($(LLVM_VERSION),)
    $(warning Warning: llvm-config not found. Building without LLVM tags.)
    BUILD_FLAGS :=
else
    $(info Building with LLVM $(LLVM_VERSION) (tags: $(LLVM_TAG)))
endif

# Build the Go application
build: antlr
	@echo "Building Go application..."
	@echo "Using build flags: $(BUILD_FLAGS)"
	go build $(BUILD_FLAGS) -o bbycblgo .

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

# Download ANTLR JAR if needed
$(ANTLR_JAR):
	@echo "Downloading ANTLR jar..."
	@curl -O $(ANTLR_URL)
