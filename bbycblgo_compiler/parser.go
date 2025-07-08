// Package main provides the core logic for the COBOL parser performance test.
package main

import (
	"bufio"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"sync"
	"time"

	"bbycblgo_compiler/parser" // Import the generated ANTLR parser package
	"github.com/antlr4-go/antlr/v4"
)

// Colors for terminal output, used for better readability of test results.
const (
	ColorReset = "\033[0m"
	ColorRed   = "\033[31m"
	ColorGreen = "\033[32m"
	ColorCyan  = "\033[36m"
)

// ParseResult represents the outcome of parsing a single file.
// It includes the filename, success status, any error messages, and the duration of the parsing operation.
type ParseResult struct {
	Filename string
	Success  bool
	Error    string
	Duration time.Duration
}

// CustomErrorListener implements the antlr.ErrorListener interface.
// It captures syntax errors encountered during lexing and parsing.
type CustomErrorListener struct {
	HasError bool     // Indicates if any error occurred.
	Errors   []string // Stores a list of error messages.
}

// NewCustomErrorListener creates and returns a new instance of CustomErrorListener.
func NewCustomErrorListener() *CustomErrorListener {
	return &CustomErrorListener{
		HasError: false,
		Errors:   make([]string, 0),
	}
}

// SyntaxError is called by ANTLR when a syntax error is encountered.
// It records the error message and sets the HasError flag.
func (c *CustomErrorListener) SyntaxError(recognizer antlr.Recognizer, offendingSymbol interface{}, line, column int, msg string, e antlr.RecognitionException) {
	c.HasError = true
	errorMsg := fmt.Sprintf("line %d:%d %s", line, column, msg)
	c.Errors = append(c.Errors, errorMsg)
}

// ReportAmbiguity is part of the antlr.ErrorListener interface.
// Currently, it does not perform any specific action.
func (c *CustomErrorListener) ReportAmbiguity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, exact bool, ambigAlts *antlr.BitSet, configs *antlr.ATNConfigSet) {
	// Handle ambiguity if needed
}

// ReportAttemptingFullContext is part of the antlr.ErrorListener interface.
// Currently, it does not perform any specific action.
func (c *CustomErrorListener) ReportAttemptingFullContext(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, conflictingAlts *antlr.BitSet, configs *antlr.ATNConfigSet) {
	// Handle full context attempts if needed
}

// ReportContextSensitivity is part of the antlr.ErrorListener interface.
// Currently, it does not perform any specific action.
func (c *CustomErrorListener) ReportContextSensitivity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, prediction int, configs *antlr.ATNConfigSet) {
	// Handle context sensitivity if needed
}

// parseFileCobol parses a single COBOL file.
// It reads the file, preprocesses its content, and then uses the ANTLR4 lexer and parser
// to attempt parsing. It returns a ParseResult indicating success or failure and duration.
func parseFileCobol(filepath string) ParseResult {
	start := time.Now()
	filename := filepath[strings.LastIndex(filepath, "/")+1:]

	result := ParseResult{
		Filename: filename,
		Success:  false,
		Error:    "",
		Duration: 0,
	}

	// Read file content.
	file, err := os.Open(filepath)
	if err != nil {
		result.Error = fmt.Sprintf("Failed to open file: %v", err)
		result.Duration = time.Since(start)
		return result
	}
	defer file.Close()

	// Read lines from the file.
	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	// Check for scanner errors.
	if err := scanner.Err(); err != nil {
		result.Error = fmt.Sprintf("Failed to read file: %v", err)
		result.Duration = time.Since(start)
		return result
	}

	// Preprocess the COBOL source code.
	processedText := preprocessCobolAdvanced(lines)

	// Create ANTLR input stream from the preprocessed text.
	input := antlr.NewInputStream(processedText)

	// Create a new lexer instance and attach a custom error listener.
	lexer := parser.NewbbyCBLLexer(input) // Uses the generated lexer
	lexer.RemoveErrorListeners()
	lexErrorListener := NewCustomErrorListener()
	lexer.AddErrorListener(lexErrorListener)

	// Create a new token stream from the lexer.
	stream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)

	// Create a new parser instance and attach a custom error listener.
	p := parser.NewbbyCBLParser(stream) // Uses the generated parser
	p.RemoveErrorListeners()
	parseErrorListener := NewCustomErrorListener()
	p.AddErrorListener(parseErrorListener)

	// Parse the input using the grammar's start rule (Program).
	// The parse tree is intentionally unused for this performance test.
	_ = p.Program()

	// Check for any errors reported by the lexer or parser.
	if lexErrorListener.HasError || parseErrorListener.HasError {
		allErrors := append(lexErrorListener.Errors, parseErrorListener.Errors...)
		result.Error = strings.Join(allErrors, "; ")
	} else {
		result.Success = true
	}

	// Record the total duration of the parsing operation.
	result.Duration = time.Since(start)
	return result
}

// getTestFiles returns a sorted list of file paths matching the configured file extension
// within the specified directory. It applies a limit based on MaxTest configuration.
func getTestFiles(dir string) ([]string, error) {
	var files []string

	// Walk the directory to find all files with the specified extension.
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		// If it's a file and matches the extension, add it to the list.
		if !d.IsDir() && strings.HasSuffix(path, cfg.FileExt) {
			files = append(files, path)
		}

		return nil
	})

	if err != nil {
		return nil, err
	}

	// Sort the files alphabetically for consistent processing order.
	sort.Strings(files)

	// Apply MaxTest limit if configured (MaxTest > 0).
	if cfg.MaxTest > 0 && len(files) > cfg.MaxTest {
		files = files[:cfg.MaxTest]
	}

	return files, nil
}

// runSequential processes a list of files sequentially.
// It parses each file one by one and prints the result.
func runSequential(files []string) {
	fmt.Printf("Processing %d files sequentially\n", len(files))

	start := time.Now()
	passed := 0
	failed := 0
	var totalParseTime time.Duration

	for i, file := range files {
		result := parseFileCobol(file)
		totalParseTime += result.Duration

		if result.Success {
			passed++
			fmt.Printf("%s[PASS]%s (%d/%d) %s (%.2fms)\n", ColorGreen, ColorReset, i+1, len(files), result.Filename, float64(result.Duration.Nanoseconds())/1e6)
		} else {
			failed++
			fmt.Printf("%s[FAIL]%s (%d/%d) %s: %s\n", ColorRed, ColorReset, i+1, len(files), result.Filename, result.Error)
			// Copy failed test files to the configured FailedDir.
			copyFailedTest(file)
		}
	}

	totalTime := time.Since(start)

	fmt.Printf("\n%s=== SEQUENTIAL RESULTS ===%s\n", ColorCyan, ColorReset)
	fmt.Printf("Total time: %.2f seconds\n", totalTime.Seconds())
	fmt.Printf("Parse time: %.2f seconds\n", totalParseTime.Seconds())
	fmt.Printf("Overhead: %.2f seconds\n", (totalTime - totalParseTime).Seconds())
	fmt.Printf("Passed: %d, Failed: %d\n", passed, failed)
	fmt.Printf("Files per second: %.1f\n", float64(len(files))/totalTime.Seconds())
	fmt.Printf("Average per file: %.2fms\n", float64(totalTime.Nanoseconds())/float64(len(files))/1e6)
}

// runParallel processes a list of files concurrently using goroutines.
// It distributes parsing tasks among available CPU cores and collects results.
func runParallel(files []string) {
	numWorkers := runtime.NumCPU()
	fmt.Printf("Processing %d files with %d goroutines\n", len(files), numWorkers)

	start := time.Now()

	// Create channels for jobs (file paths) and results (ParseResult).
	jobs := make(chan string, len(files))
	results := make(chan ParseResult, len(files))

	// Start worker goroutines.
	var wg sync.WaitGroup
	for w := 0; w < numWorkers; w++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for filepath := range jobs {
				result := parseFileCobol(filepath)
				results <- result
			}
		}()
	}

	// Send file paths to the jobs channel.
	go func() {
		for _, file := range files {
			jobs <- file
		}
		close(jobs)
	}()

	// Wait for all workers to finish and then close the results channel.
	go func() {
		wg.Wait()
		close(results)
	}()

	// Collect and aggregate results from the results channel.
	passed := 0
	failed := 0
	var totalParseTime time.Duration
	processed := 0

	for result := range results {
		processed++
		totalParseTime += result.Duration

		if result.Success {
			passed++
			fmt.Printf("%s[PASS]%s (%d/%d) %s (%.2fms)\n", ColorGreen, ColorReset, processed, len(files), result.Filename, float64(result.Duration.Nanoseconds())/1e6)
		} else {
			failed++
			fmt.Printf("%s[FAIL]%s (%d/%d) %s: %s\n", ColorRed, ColorReset, processed, len(files), result.Filename, result.Error)
			// Copy failed test files to the configured FailedDir.
			copyFailedTest(result.Filename)
		}
	}

	totalTime := time.Since(start)

	fmt.Printf("\n%s=== PARALLEL RESULTS ===%s\n", ColorCyan, ColorReset)
	fmt.Printf("Total time: %.2f seconds\n", totalTime.Seconds())
	fmt.Printf("Parse time: %.2f seconds (cumulative)\n", totalParseTime.Seconds())
	fmt.Printf("Speedup: %.1fx\n", totalParseTime.Seconds()/totalTime.Seconds())
	fmt.Printf("Passed: %d, Failed: %d\n", passed, failed)
	fmt.Printf("Files per second: %.1f\n", float64(len(files))/totalTime.Seconds())
	fmt.Printf("Average per file: %.2fms\n", float64(totalTime.Nanoseconds())/float64(len(files))/1e6)
}

// copyFailedTest copies a failed test file from its original location to the configured FailedDir.
func copyFailedTest(filename string) {
	// Construct full paths for source and destination files.
	sourceFile := filepath.Join(cfg.TestDir, filename)
	destFile := filepath.Join(cfg.FailedDir, filename)

	// Read the content of the source file.
	input, err := os.ReadFile(sourceFile)
	if err != nil {
		fmt.Printf("Failed to read failed test file: %v\n", err)
		return
	}

	// Write the content to the destination file in the FailedDir.
	if err := os.WriteFile(destFile, input, 0644); err != nil {
		fmt.Printf("Failed to copy failed test file: %v\n", err)
	}
}

