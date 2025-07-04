package main

import (
	"fmt"
	"io/ioutil"
	"os"
	fp "path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"sync"

	"bbyCBLcompiler/parser" // This will be the generated parser package
	"github.com/antlr4-go/antlr/v4"
)

const (
	MAX_TEST     = 0 // Amount of test cases to run, 0 for all
	PARSE        = true
	ALWAYS_PRINT = false
	SHOW_AST     = false
	TEST_DIR     = "./tests/recombined_formatted/"
	FAILED_DIR   = "./tests/failed_prev_run/"
)

// Colors for console output
type Colors struct{}

func (c Colors) Yellow() string  { return "\033[93m" }
func (c Colors) Green() string   { return "\033[92m" }
func (c Colors) Red() string     { return "\033[91m" }
func (c Colors) Blue() string    { return "\033[94m" }
func (c Colors) Cyan() string    { return "\033[96m" }
func (c Colors) Magenta() string { return "\033[95m" }
func (c Colors) Reset() string   { return "\033[0m" }

var colors = Colors{}

// LoggingErrorListener implements antlr.ErrorListener
type LoggingErrorListener struct {
	*antlr.DefaultErrorListener
	hasError      bool
	errorMessages []string
}

func NewLoggingErrorListener() *LoggingErrorListener {
	return &LoggingErrorListener{
		DefaultErrorListener: antlr.NewDefaultErrorListener(),
		hasError:             false,
		errorMessages:        []string{},
	}
}

func (l *LoggingErrorListener) SyntaxError(recognizer antlr.Recognizer, offendingSymbol interface{}, line, column int, msg string, e antlr.RecognitionException) {
	l.hasError = true
	recognizerName := ""
	if lexer, ok := recognizer.(*parser.bbyCBLLexer); ok {
		recognizerName = "bbyCBLLexer"
	} else if parser, ok := recognizer.(*parser.bbyCBLParser); ok {
		recognizerName = "bbyCBLParser"
	} else {
		recognizerName = fmt.Sprintf("%T", recognizer)
	}

	errorMsg := fmt.Sprintf("%s[ERROR] %s line %d:%d %s%s", colors.Red(), recognizerName, line, column, msg, colors.Reset())
	l.errorMessages = append(l.errorMessages, errorMsg)
}

var ID_KEYWORDS = map[string]bool{
	"program-id":    true,
	"author":        true,
	"date-written":  true,
	"date-compiled": true,
	"installation":  true,
	"security":      true,
}

var ALL_KEYWORDS = map[string]bool{
	"identification": true, "data": true, "procedure": true, "division": true,
	"accept": true, "add": true, "subtract": true, "multiply": true,
	"divide": true, "move": true, "display": true, "evaluate": true,
	"if": true, "then": true, "else": true, "end": true, "end-if": true,
	"end-evaluate": true, "perform": true, "varying": true, "loop": true,
	"while": true, "until": true, "go": true, "call": true, "copy": true,
	"signal": true, "stop": true, "run": true, "next": true, "sentence": true,
	"delimited": true, "by": true, "with": true, "no": true, "advancing": true,
	"occurs": true, "times": true, "using": true, "reference": true,
	"value": true, "content": true, "giving": true, "remainder": true,
	"of": true, "size": true, "space": true, "spaces": true, "picture": true,
	"pic": true, "is": true, "like": true, "when": true, "other": true,
	"also": true, "high-values": true, "replacing": true, "proceed": true,
	"alter": true, "and": true, "or": true, "xor": true, "base": true,
	"description": true, "error": true,
}

var _word_re = regexp.MustCompile(`[A-Za-z_$][A-Za-z0-9_$-]*`)

func validate_area(lineText string, startCol int, inProcDiv bool) error {
	text := strings.TrimSpace(lineText)
	upperText := strings.ToUpper(text)

	if strings.HasPrefix(upperText, "IDENTIFICATION DIVISION") ||
		strings.HasPrefix(upperText, "DATA DIVISION") ||
		strings.HasPrefix(upperText, "PROCEDURE DIVISION") {
		if startCol != 7 {
			parts := strings.Split(upperText, " ")
			return fmt.Errorf("Division header '%s DIVISION' must start in column 8, found col %d", parts[0], startCol+1)
		}
	} else if inProcDiv {
		if strings.HasSuffix(text, ".") && !strings.Contains(strings.TrimSpace(text), " ") && !strings.HasPrefix(upperText, "END-") {
			// This condition in Python was: `text.strip().endswith(".") and " " not in text.strip() and not text.strip().startswith("END-")`
			// The `" " not in text.strip()` part means it's a single word ending with a dot, likely a paragraph name.
			if startCol != 7 {
				return fmt.Errorf("Paragraph name '%s' must start in column 8, found col %d", strings.TrimSpace(text), startCol+1)
			}
			return nil
		} else {
			if startCol != 11 {
				parts := strings.Split(upperText, " ")
				return fmt.Errorf("In PROCEDURE DIVISION, statement '%s' must start in column 12, found col %d", parts[0], startCol+1)
			}
		}
	}
	return nil
}

func normalize_case(line string) string {
	var out strings.Builder
	i := 0
	inString := false
	inPicture := false
	inIDValue := false
	afterCopy := false

	for i < len(line) {
		ch := line[i]

		if ch == '"' {
			inString = !inString
			out.WriteRune(rune(ch))
			i++
			continue
		}

		if inString {
			out.WriteRune(rune(ch))
			i++
			continue
		}

		if inPicture {
			out.WriteRune(rune(ch))
			i++
			if ch == '.' {
				inPicture = false
			}
			continue
		}

		if inIDValue {
			out.WriteRune(rune(ch))
			i++
			if ch == '.' {
				inIDValue = false
			}
			continue
		}

		if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_' || ch == '$' {
			start := i
			for i < len(line) && ((line[i] >= 'a' && line[i] <= 'z') || (line[i] >= 'A' && line[i] <= 'Z') || (line[i] >= '0' && line[i] <= '9') || line[i] == '_' || line[i] == '-' || line[i] == '$') {
				i++
			}
			word := line[start:i]
			lower := strings.ToLower(word)

			if lower == "picture" || lower == "pic" {
				out.WriteString(strings.ToUpper(word))
				j := i
				for j < len(line) && (line[j] == ' ' || line[j] == '\t') {
					j++
				}
				if j+1 < len(line) && strings.ToLower(line[j:j+2]) == "is" {
					out.WriteString(line[i:j] + "IS")
					j += 2
				} else {
					out.WriteString(line[i:j])
				}
				i = j
				inPicture = true
				continue
			}

			if _, ok := ID_KEYWORDS[lower]; ok {
				out.WriteString(strings.ToUpper(word))
				dot := strings.Index(line[i:], ".")
				if dot != -1 {
					out.WriteString(line[i : i+dot+1])
					i = i + dot + 1
					inIDValue = true
				}
				continue
			}

			if lower == "copy" && !afterCopy {
				out.WriteString("COPY")
				afterCopy = true
				continue
			}

			if afterCopy {
				out.WriteString(word)
				afterCopy = false
				continue
			}

			if _, ok := ALL_KEYWORDS[lower]; ok {
				out.WriteString(strings.ToUpper(word))
			} else {
				out.WriteString(word)
			}
		} else {
			out.WriteRune(rune(ch))
			i++
		}
	}
	return out.String()
}

func preprocess_cobol(lines []string) (string, error) {
	var result []string
	inProcDiv := false
	currentLine := ""
	currentStartCol := -1

	for _, line := range lines {
		indicator := ' '
		if len(line) >= 7 {
			indicator = rune(line[6])
		}

		areaA := ""
		if len(line) >= 11 {
			areaA = line[7:11]
		}
		areaB := ""
		if len(line) >= 72 {
			areaB = line[11:72]
		} else if len(line) > 11 {
			areaB = line[11:]
		}

		if indicator == '*' {
			continue
		}

		if indicator == ' ' {
			content := ""
			if len(line) >= 72 {
				content = line[7:72]
			} else if len(line) > 7 {
				content = line[7:]
			}

			if !regexp.MustCompile(`\S`).MatchString(content) {
				continue
			}

			match := regexp.MustCompile(`\S`).FindStringIndex(content)
			startCol := -1
			if match != nil {
				startCol = 7 + match[0]
			} else {
				continue
			}

			isAreaB := startCol == 11
			shouldContinue := currentLine != "" && inProcDiv && isAreaB && !strings.HasSuffix(strings.TrimRight(currentLine, " "), ".")

			if shouldContinue {
				currentLine += " " + strings.TrimSpace(areaA+areaB)
				continue
			}

			if currentLine != "" {
				if err := validate_area(currentLine, currentStartCol, inProcDiv); err != nil {
					return "", err
				}
				result = append(result, currentLine)
			}

			currentLine = strings.TrimRight(areaA+areaB, " ")
			currentStartCol = startCol

			if strings.HasPrefix(strings.ToUpper(strings.TrimSpace(currentLine)), "PROCEDURE DIVISION") {
				inProcDiv = true
			}
			continue
		}
		if indicator == '-' {
			if currentLine == "" {
				return "", fmt.Errorf("Continuation without a preceding line")
			}

			continuationRaw := strings.TrimRight(areaA+areaB, " ")
			if strings.TrimSpace(continuationRaw) == "." {
				currentLine = strings.TrimRight(currentLine, " ") + " ."
				continue
			}

			continuation := strings.TrimLeft(continuationRaw, " ")

			lastMatch := regexp.MustCompile(`[A-Za-z0-9]+$`).FindString(strings.TrimRight(currentLine, " "))
			firstMatch := regexp.MustCompile(`^[A-Za-z0-9]+`).FindString(continuation)

			joinWithoutSpace := false
			if lastMatch != "" && firstMatch != "" {
				// Check if both are NOT keywords. If they are, they should be separated by a space.
				_, lastIsKeyword := ALL_KEYWORDS[strings.ToLower(lastMatch)]
				_, firstIsKeyword := ALL_KEYWORDS[strings.ToLower(firstMatch)]
				joinWithoutSpace = !lastIsKeyword && !firstIsKeyword
			}

			glue := " "
			if joinWithoutSpace {
				glue = ""
			}
			currentLine = strings.TrimRight(currentLine, " ") + glue + continuation
			continue
		}

		return "", fmt.Errorf("Invalid line indicator '%c' at column 7", indicator)
	}

	if currentLine != "" {
		if err := validate_area(currentLine, currentStartCol, inProcDiv); err != nil {
			return "", err
		}
		result = append(result, currentLine)
	}

	normalizedLines := make([]string, len(result))
	for i, line := range result {
		normalizedLines[i] = normalize_case(line)
	}

	return strings.Join(normalizedLines, "\n"), nil
}

// parseResult struct to hold results from parsing a single file
type parseResult struct {
	Filename      string
	Filepath      string
	Success       bool
	Error         bool
	Message       string
	Content       string
	ErrorMessages []string
	AST           string
}

// printASTTree recursively prints the AST. (Go equivalent of Python's print_ast_tree)
func printASTTree(tree antlr.Tree, parser *parser.bbyCBLParser, level int) string {
	indent := strings.Repeat("  ", level)

	if terminalNode, ok := tree.(*antlr.TerminalNodeImpl); ok {
		symbolText := terminalNode.GetSymbol().GetText()
		return fmt.Sprintf("%s%s[Terminal] %s%s%s", indent, colors.Cyan(), colors.Blue(), symbolText, colors.Reset())
	} else if ruleCtx, ok := tree.(antlr.RuleContext); ok {
		ruleName := parser.RuleNames[ruleCtx.GetRuleIndex()]
		result := fmt.Sprintf("%s%s[Rule] %s%s%s\n", indent, colors.Magenta(), colors.Green(), ruleName, colors.Reset())
		for _, child := range ruleCtx.GetChildren() {
			result += printASTTree(child, parser, level+1) + "\n"
		}
		return strings.TrimRight(result, "\n")
	}
	return ""
}

// parse_single_file function
func parse_single_file(filepath string) parseResult {
	filename := fp.Base(filepath)

	// Read raw contents
	rawBytes, err := ioutil.ReadFile(filepath)
	if err != nil {
		return parseResult{
			Filename:      filename,
			Filepath:      filepath,
			Success:       false,
			Error:         true,
			Message:       fmt.Sprintf("Failed to read file: %s", err),
			ErrorMessages: []string{fmt.Sprintf("Failed to read file: %s", err)},
		}
	}
	rawText := string(rawBytes)
	rawLines := strings.Split(rawText, "\n")

	// Preprocess COBOL fixed-format lines
	processedText, err := preprocess_cobol(rawLines)
	if err != nil {
		return parseResult{
			Filename:      filename,
			Filepath:      filepath,
			Success:       false,
			Error:         true,
			Message:       fmt.Sprintf("Preprocessing error: %s", err),
			Content:       rawText,
			ErrorMessages: []string{fmt.Sprintf("Preprocessing error: %s", err)},
		}
	}

	// Lexing and parsing setup
	inputStream := antlr.NewInputStream(processedText)
	lexer := parser.NewbbyCBLLexer(inputStream)
	_ = lexer // Mark as used
	lexer.RemoveErrorListeners()
	lexErrors := NewLoggingErrorListener()
	lexer.AddErrorListener(lexErrors)
	tokens := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)

	parser := parser.NewbbyCBLParser(tokens)
	_ = parser // Mark as used
	parser.RemoveErrorListeners()
	parseErrors := NewLoggingErrorListener()
	parser.AddErrorListener(parseErrors)

	var tree antlr.Tree
	if PARSE {
		func() {
			defer func() {
				if r := recover(); r != nil {
					// Catch ANTLR parsing panics (e.g., from malformed grammars or inputs)
					parseErrors.hasError = true
					parseErrors.errorMessages = append(parseErrors.errorMessages, fmt.Sprintf("Parser panic: %v", r))
				}
			}()
			tree = parser.Program()
		}()
	}

	hasSyntaxError := lexErrors.hasError || parseErrors.hasError

	if hasSyntaxError {
		errorMessages := append(lexErrors.errorMessages, parseErrors.errorMessages...)
		return parseResult{
			Filename:      filename,
			Filepath:      filepath,
			Success:       false,
			Error:         true,
			Message:       "Syntax error during parsing",
			Content:       rawText,
			ErrorMessages: errorMessages,
		}
	}

	// Successful parse
	result := parseResult{
		Filename:      filename,
		Filepath:      filepath,
		Success:       true,
		Error:         false,
		Message:       fmt.Sprintf("Successfully parsed %s", filename),
		ErrorMessages: []string{},
	}

	if ALWAYS_PRINT {
		result.Content = processedText
	}

	if SHOW_AST && tree != nil {
		result.AST = printASTTree(tree, parser, 0)
	}

	return result
}

// save_failed_tests function
func save_failed_tests(failedTests []string) {
	if len(failedTests) == 0 {
		return
	}

	// Create the directory if it doesn't exist
	err := os.MkdirAll(FAILED_DIR, 0755)
	if err != nil {
		fmt.Printf("%sError creating failed tests directory: %v%s\n", colors.Red(), err, colors.Reset())
		return
	}

	// Skip copying if we're already running from the failed tests directory
	if TEST_DIR == FAILED_DIR {
		fmt.Printf("\n%sAlready running from failed tests directory. Skipping copy.%s\n", colors.Yellow(), colors.Reset())
		return
	}

	for _, filename := range failedTests {
		srcPath := filepath.Join(TEST_DIR, filename)
		dstPath := fp.Join(FAILED_DIR, filename)

		// Read the source file content
		content, err := ioutil.ReadFile(srcPath)
		if err != nil {
			fmt.Printf("%sError reading source file %s: %v%s\n", colors.Red(), srcPath, err, colors.Reset())
			continue
		}

		// Write the content to the destination file
		err = ioutil.WriteFile(dstPath, content, 0644)
		if err != nil {
			fmt.Printf("%sError copying file %s to %s: %v%s\n", colors.Red(), srcPath, dstPath, err, colors.Reset())
		}
	}

	fmt.Printf("\n%sFailed tests copied to: %s%s\n", colors.Yellow(), FAILED_DIR, colors.Reset())
}

// clear_failed_tests function
func clear_failed_tests() {
	if _, err := os.Stat(FAILED_DIR); os.IsNotExist(err) {
		fmt.Printf("%sNo failed tests directory found.%s\n", colors.Yellow(), colors.Reset())
		return
	}

	dir, err := ioutil.ReadDir(FAILED_DIR)
	if err != nil {
		fmt.Printf("%sError reading failed tests directory: %v%s\n", colors.Red(), err, colors.Reset())
		return
	}

	for _, file := range dir {
		if !file.IsDir() {
			filePath := fp.Join(FAILED_DIR, file.Name())
			err := os.Remove(filePath)
			if err != nil {
				fmt.Printf("%sError removing file %s: %v%s\n", colors.Red(), filePath, err, colors.Reset())
			}
		}
	}
	fmt.Printf("%sCleared previous failed tests directory: %s%s\n", colors.Yellow(), FAILED_DIR, colors.Reset())
}

// display_usage function
func display_usage() {
	fmt.Printf("\n%sUsage options:%s\n", colors.Cyan(), colors.Reset())
	fmt.Printf("  %sgo run main.go%s              - Run all tests\n", colors.Cyan(), colors.Reset())
	fmt.Printf("  %sgo run main.go -FAILED%s      - Run only previously failed tests\n", colors.Cyan(), colors.Reset())
	fmt.Printf("  %sgo run main.go -CLEAR%s       - Clear the failed tests directory\n", colors.Cyan(), colors.Reset())
	fmt.Printf("  %sgo run main.go -WORKERS N%s   - Use N worker processes (default: all CPU cores)\n", colors.Cyan(), colors.Reset())
}

func main() {
	currentTestDir := TEST_DIR
	workers := runtime.NumCPU()

	args := os.Args[1:]
	if len(args) > 0 {
		if args[0] == "-FAILED" {
			if _, err := os.Stat(FAILED_DIR); err == nil && len(getBabyFiles(FAILED_DIR)) > 0 {
				currentTestDir = FAILED_DIR
				fmt.Printf("%sRunning only previously failed tests from: %s%s\n", colors.Yellow(), FAILED_DIR, colors.Reset())
			} else {
				fmt.Printf("%sNo previous failed tests found. Running all tests.%s\n", colors.Yellow(), colors.Reset())
			}
		} else if args[0] == "-CLEAR" {
			clear_failed_tests()
			return
		} else if args[0] == "-WORKERS" && len(args) > 1 {
			if n, err := strconv.Atoi(args[1]); err == nil && n > 0 {
				workers = n
				fmt.Printf("%sUsing %d worker processes%s\n", colors.Yellow(), workers, colors.Reset())
			} else {
				fmt.Printf("%sInvalid number of workers. Using default: %d%s\n", colors.Red(), workers, colors.Reset())
			}
		}
	}

	testFiles := getBabyFiles(currentTestDir)
	totalTests := len(testFiles)

	if MAX_TEST > 0 && MAX_TEST < totalTests {
		testFiles = testFiles[:MAX_TEST]
		totalTests = MAX_TEST
	}

	fmt.Printf("%sRunning %d tests using %d worker processes%s\n", colors.Yellow(), totalTests, workers, colors.Reset())

	results := make(chan parseResult, totalTests)
	var wg sync.WaitGroup

	// Create a worker pool
	filePaths := make(chan string, totalTests)
	for i := 0; i < workers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for filePath := range filePaths {
				results <- parse_single_file(filePath)
			}
		}()
	}

	// Send file paths to workers
	for _, file := range testFiles {
		filePaths <- fp.Join(currentTestDir, file)
	}
	close(filePaths)

	// Wait for all workers to finish
	wg.Wait()
	close(results)

	ran := 0
	passed := 0
	errors := 0
	var failedTests []string

	for result := range results {
		ran++
		if result.Success {
			passed++
			fmt.Printf("%s [PASS] (%d/%d) %s %s\n", colors.Green(), ran, totalTests, result.Filename, colors.Reset())

			if ALWAYS_PRINT && result.Content != "" {
				fmt.Printf("\n%s=== File Contents: %s ===%s\n", colors.Yellow(), result.Filename, colors.Reset())
				fmt.Println(result.Content)
			}

			if SHOW_AST && result.AST != "" {
				fmt.Printf("\n%s=== Abstract Syntax Tree: %s ===%s\n", colors.Yellow(), result.Filename, colors.Reset())
				fmt.Println(result.AST)
				fmt.Println()
			}
		} else {
			errors++
			failedTests = append(failedTests, result.Filename)

			for _, errMsg := range result.ErrorMessages {
				fmt.Printf("\n%s\n", errMsg)
			}

			fmt.Printf("\n=== Syntax error in: %s ===\n", result.Filepath)

			if result.Content != "" {
				fmt.Printf("\n%s=== File Contents: %s ===%s\n", colors.Yellow(), result.Filename, colors.Reset())
				fmt.Println(result.Content)
			}

			fmt.Printf("%s[FAILED] %s%s\n", colors.Red(), result.Filepath, colors.Reset())
		}
	}

	// Print summary
	fmt.Printf("\n\n\n%sTotal tests ran: %d%s\n", colors.Yellow(), ran, colors.Reset())
	fmt.Printf("%sTests passed: %d%s\n", colors.Green(), passed, colors.Reset())
	fmt.Printf("%sSyntax errors: %d%s\n", colors.Red(), errors, colors.Reset())

	if errors > 0 {
		fmt.Printf("%sFailed tests: \n %s%s\n", colors.Red(), strings.Join(failedTests, " "), colors.Reset())
		save_failed_tests(failedTests)
	}

	display_usage()
}

func getBabyFiles(dir string) []string {
	files, err := ioutil.ReadDir(dir)
	if err != nil {
		return []string{}
	}

	var babyFiles []string
	for _, file := range files {
		if !file.IsDir() && strings.HasSuffix(file.Name(), ".baby") {
			babyFiles = append(babyFiles, file.Name())
		}
	}
	return babyFiles
}
