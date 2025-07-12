// Package main provides the core logic for the COBOL parser performance test.
package main

import (
	"bufio"
	"fmt"
	"io/fs"
	"log"
	"os"
	path_filepath "path/filepath"
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
	Filename            string
	Success             bool
	Error               string
	Duration            time.Duration
	OriginalContent     string
	PreprocessedContent string
	OriginalFilePath    string
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
}

// ReportAttemptingFullContext is part of the antlr.ErrorListener interface.
// Currently, it does not perform any specific action.
func (c *CustomErrorListener) ReportAttemptingFullContext(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, conflictingAlts *antlr.BitSet, configs *antlr.ATNConfigSet) {
}

// ReportContextSensitivity is part of the antlr.ErrorListener interface.
// Currently, it does not perform any specific action.
func (c *CustomErrorListener) ReportContextSensitivity(recognizer antlr.Parser, dfa *antlr.DFA, startIndex, stopIndex int, prediction int, configs *antlr.ATNConfigSet) {
}

// parseFileCobol parses a single COBOL file.
func parseFileCobol(filepath string) ParseResult {
	start := time.Now()
	filename := filepath[strings.LastIndex(filepath, "/")+1:]

	result := ParseResult{
		Filename: filename,
		Success:  false,
		Error:    "",
		Duration: 0,
		OriginalFilePath: filepath,
	}

	file, err := os.Open(filepath)
	if err != nil {
		result.Error = fmt.Sprintf("Failed to open file: %v", err)
		result.Duration = time.Since(start)
		return result
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		result.Error = fmt.Sprintf("Failed to read file: %v", err)
		result.Duration = time.Since(start)
		return result
	}
	result.OriginalContent = strings.Join(lines, "\n")

	processedText := preprocessCobolAdvanced(lines)
	result.PreprocessedContent = processedText

	input := antlr.NewInputStream(processedText)
	lexer := parser.NewbbyCBLLexer(input)
	lexer.RemoveErrorListeners()
	lexErrorListener := NewCustomErrorListener()
	lexer.AddErrorListener(lexErrorListener)

	stream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)
	p := parser.NewbbyCBLParser(stream)
	p.RemoveErrorListeners()
	parseErrorListener := NewCustomErrorListener()
	p.AddErrorListener(parseErrorListener)

	tree := p.Program()

	_, semanticErrors := Analyze(tree)
	if len(semanticErrors) > 0 {
		for _, err := range semanticErrors {
			parseErrorListener.Errors = append(parseErrorListener.Errors, fmt.Sprintf("semantic error line %d: %s", err.line, err.msg))
		}
		parseErrorListener.HasError = true
	}

	if lexErrorListener.HasError || parseErrorListener.HasError {
		allErrors := append(lexErrorListener.Errors, parseErrorListener.Errors...)
		result.Error = strings.Join(allErrors, "; ")
	} else {
		result.Success = true
	}

	result.Duration = time.Since(start)
	return result
}

func getTestFiles(dir string) ([]string, error) {
	var files []string
	err := path_filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasSuffix(path, cfg.FileExt) {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	sort.Strings(files)
	if cfg.MaxFiles > 0 && len(files) > cfg.MaxFiles {
		files = files[:cfg.MaxFiles]
	}
	return files, nil
}

func runSequential(files []string, isFailedRun bool, printPassed bool, hideVerbose bool) {
	fmt.Printf("Processing %d files sequentially\n", len(files))
	start := time.Now()
	passed, failed := 0, 0
	var totalParseTime time.Duration

	for i, file := range files {
		result := parseFileCobol(file)
		totalParseTime += result.Duration
		if result.Success {
			passed++
			if printPassed {
				fmt.Printf("%s[PASS]%s (%d/%d) %s (%.2fms)\n", ColorGreen, ColorReset, i+1, len(files), result.Filename, float64(result.Duration.Nanoseconds())/1e6)
			}
		} else {
			failed++
			fmt.Printf("%s[FAIL]%s (%d/%d) %s: %s\n", ColorRed, ColorReset, i+1, len(files), result.Filename, result.Error)
			copyFailedTest(file, cfg.FailedDir)
			if isFailedRun && !hideVerbose {
				fmt.Printf("\n%s--- Original Content ---%s\n%s\n%s------------------------%s\n", ColorCyan, ColorReset, result.OriginalContent, ColorCyan, ColorReset)
				fmt.Printf("\n%s--- Preprocessed Content ---%s\n%s\n%s----------------------------%s\n", ColorCyan, ColorReset, result.PreprocessedContent, ColorCyan, ColorReset)
			}
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

func runParallel(files []string, isFailedRun bool, printPassed bool, hideVerbose bool) {
	numWorkers := runtime.NumCPU()
	fmt.Printf("Processing %d files with %d goroutines\n", len(files), numWorkers)
	start := time.Now()

	jobs := make(chan string, len(files))
	results := make(chan ParseResult, len(files))
	var wg sync.WaitGroup

	for w := 0; w < numWorkers; w++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for filepath := range jobs {
				results <- parseFileCobol(filepath)
			}
		}()
	}

	go func() {
		for _, file := range files {
			jobs <- file
		}
		close(jobs)
	}()

	go func() {
		wg.Wait()
		close(results)
	}()

	passed, failed := 0, 0
	var totalParseTime time.Duration
	processed := 0

	for result := range results {
		processed++
		totalParseTime += result.Duration
		if result.Success {
			passed++
			if printPassed {
				fmt.Printf("%s[PASS]%s (%d/%d) %s (%.2fms)\n", ColorGreen, ColorReset, processed, len(files), result.Filename, float64(result.Duration.Nanoseconds())/1e6)
			}
		} else {
			failed++
			fmt.Printf("%s[FAIL]%s (%d/%d) %s: %s\n", ColorRed, ColorReset, processed, len(files), result.Filename, result.Error)
			copyFailedTest(result.OriginalFilePath, cfg.FailedDir)
			if isFailedRun && !hideVerbose {
				fmt.Printf("\n%s--- Original Content ---%s\n%s\n%s------------------------%s\n", ColorCyan, ColorReset, result.OriginalContent, ColorCyan, ColorReset)
				fmt.Printf("\n%s--- Preprocessed Content ---%s\n%s\n%s----------------------------%s\n", ColorCyan, ColorReset, result.PreprocessedContent, ColorCyan, ColorReset)
			}
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

func copyFailedTest(sourceFile string, destDir string) {
	_, filename := path_filepath.Split(sourceFile)
	destFile := path_filepath.Join(destDir, filename)
	input, err := os.ReadFile(sourceFile)
	if err != nil {
		fmt.Printf("Failed to read failed test file: %v\n", err)
		return
	}
	if err := os.WriteFile(destFile, input, 0644); err != nil {
		fmt.Printf("Failed to copy failed test file: %v\n", err)
	}
}

func parseFile(filepath string) {
	fmt.Printf("Parsing %s\n", filepath)
	file, err := os.Open(filepath)
	if err != nil {
		log.Fatalf("Failed to open file: %v", err)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("Failed to read file: %v", err)
	}

	processedText := preprocessCobolAdvanced(lines)
	input := antlr.NewInputStream(processedText)
	lexer := parser.NewbbyCBLLexer(input)
	stream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)
	p := parser.NewbbyCBLParser(stream)

	errorListener := NewCustomErrorListener()
	p.RemoveErrorListeners()
	p.AddErrorListener(errorListener)
	lexer.RemoveErrorListeners()
	lexer.AddErrorListener(errorListener)

	tree := p.Program()

	if errorListener.HasError {
		fmt.Printf("%sSyntax errors in %s:%s\n", ColorRed, filepath, ColorReset)
		for _, e := range errorListener.Errors {
			fmt.Printf("- %s\n", e)
		}
		return
	}

	symbolTable, semanticErrors := Analyze(tree) // Now Analyze returns the symbolTable
	if len(semanticErrors) > 0 {
		fmt.Printf("%sSemantic errors in %s:%s\n", ColorRed, filepath, ColorReset)
		for _, e := range semanticErrors {
			fmt.Printf("- line %d: %s\n", e.line, e.msg)
		}
		return
	}

	// Print AST
	fmt.Printf("\n%s--- Abstract Syntax Tree ---%s\n", ColorCyan, ColorReset)
	astPrinter := &ASTPrinter{Indentation: 0}
	tree.Accept(astPrinter)

	// Print Symbol Table
	fmt.Printf("%s--- Symbol Table ---%s\n", ColorCyan, ColorReset)
	// Use the returned symbolTable here
	for name, fields := range symbolTable.rootScope.fields {
		for _, symbol := range fields {
			fmt.Printf("  %s: &{name:%s level:%d picture:0x%x likeRef:%s occurs:%d initialized:%t parent:0x%x children:[", name, symbol.name, symbol.level, symbol.picture, symbol.likeRef, symbol.occurs, symbol.initialized, symbol.parent)
			for i, child := range symbol.children {
				fmt.Printf("&{name:%s level:%d picture:0x%x likeRef:%s occurs:%d initialized:%t parent:0x%x}", child.name, child.level, child.picture, child.likeRef, child.occurs, child.initialized, child.parent)
				if i < len(symbol.children)-1 {
					fmt.Printf(" ")
				}
			}
			fmt.Println("]}")
		}
	}
	fmt.Printf("\n%s--------------------%s\n\n", ColorCyan, ColorReset)
}
