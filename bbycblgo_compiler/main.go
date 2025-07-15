package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/exec"
	"runtime"
	"bufio"
	"strings"

	"bbycblgo_compiler/parser"
	"github.com/antlr4-go/antlr/v4"

	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
	"github.com/spf13/viper"
)

// Configuration constants for ANTLR
const (
	ANTLR_JAR  = "antlr-4.13.1-complete.jar"
	ANTLR_URL  = "https://www.antlr.org/download/antlr-4.13.1-complete.jar"
	GRAMMAR    = "bbyCBL.g4"
	PARSER_DIR = "parser"
)

// Config holds the configuration for the application
type Config struct {
	MaxFiles    int    `mapstructure:"maxfiles"`
	TestDir     string `mapstructure:"testdir"`
	FailedDir   string `mapstructure:"faileddir"`
	FileExt     string `mapstructure:"fileext"`
	RunMode     string `mapstructure:"runmode"`
	PrintPassed bool   `mapstructure:"printpassed"`
	HideVerbose bool   `mapstructure:"hideverbose"`
}

var cfg Config

var rootCmd = &cobra.Command{
	Use:   "bbycblgo",
	Short: "A parser for COBOL files",
	Long:  `A parser for COBOL files, with options for running tests, and managing failed tests.`,
	Run: func(cmd *cobra.Command, args []string) {
		cmd.Help()
	},
	CompletionOptions: cobra.CompletionOptions{
		HiddenDefaultCmd: true,
	},
}

var testCmd = &cobra.Command{
	Use:   "test",
	Short: "Run all tests",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("Running all tests...")
		runTests(cfg.TestDir, false)
	},
}

var failedCmd = &cobra.Command{
	Use:   "failed",
	Short: "Run only previously failed tests",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("Running failed tests...")
		runTests(cfg.FailedDir, true)
	},
}

var clearCmd = &cobra.Command{
	Use:   "clear",
	Short: "Clear the failed tests directory",
	Run: func(cmd *cobra.Command, args []string) {
		failedDir, _ := cmd.Flags().GetString("faileddir")
		fmt.Printf("Clearing failed tests directory: %s\n", failedDir)
		os.RemoveAll(failedDir)
		os.MkdirAll(failedDir, os.ModePerm)
	},
}

var setupCmd = &cobra.Command{
	Use:   "setup",
	Short: "Downloads ANTLR JAR and generates parser code",
	Long:  `This command downloads the ANTLR4 complete JAR and then uses it to generate the Go parser code from the bbyCBL.g4 grammar file.`,
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("Running setup...")
		downloadAntlrJar()
		generateParser()
		fmt.Println("Setup complete.")
	},
}

var compileCmd = &cobra.Command{
	Use:   "compile [file/directory]",
	Short: "Compile COBOL file(s)",
	Long:  `This command parses, analyzes, and compiles a single COBOL file or all COBOL files in a directory to LLVM IR.`,
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		verbose, _ := cmd.Flags().GetBool("verbose")

		for _, arg := range args {
			info, err := os.Stat(arg)
			if err != nil {
				log.Printf("Error stating %s: %v", arg, err)
				continue
			}

			if info.IsDir() {
				files, err := getTestFiles(arg)
				if err != nil {
					log.Fatalf("Failed to get files from directory %s: %v", arg, err)
				}
				// getTestFiles already respects cfg.MaxFiles
				for _, file := range files {
					compileFile(file, verbose)
				}
			} else {
				compileFile(arg, verbose)
			}
		}
	},
}

var parseCmd = &cobra.Command{
	Use:   "ast [file/directory]",
	Short: "Parse COBOL file(s) and print the AST",
	Long:  `This command parses a single COBOL file or all COBOL files in a directory and prints the AST.`,
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		for _, arg := range args {
			info, err := os.Stat(arg)
			if err != nil {
				log.Printf("Error stating %s: %v", arg, err)
				continue
			}

			if info.IsDir() {
				files, err := getTestFiles(arg)
				if err != nil {
					log.Fatalf("Failed to get files from directory %s: %v", arg, err)
				}
				// getTestFiles already respects cfg.MaxFiles
				for _, file := range files {
					parseFile(file)
				}
			} else {
				parseFile(arg)
			}
		}
	},
}

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	// Add flags to compile command
	compileCmd.Flags().BoolP("verbose", "v", false, "Enable verbose output")

	// Define flags.
	rootCmd.PersistentFlags().IntP("maxfiles", "n", 10000, "Set to 0 for all files")
	rootCmd.PersistentFlags().StringP("fileext", "", ".baby", "Test file extension")
	rootCmd.PersistentFlags().StringP("runmode", "", "parallel", "Run mode: sequential or parallel")

	testCmd.Flags().StringP("testdir", "", "../tests/recombined_formatted/", "Directory with test files")
	testCmd.Flags().BoolP("printpassed", "p", false, "Print passed test cases")

	failedCmd.Flags().StringP("faileddir", "", "../tests/failed/", "Directory for failed tests")
	failedCmd.Flags().BoolP("hideverbose", "d", false, "Hide verbose output for failed tests")

	clearCmd.Flags().StringP("faileddir", "", "../tests/failed/", "Directory for failed tests")

	// Set defaults.
	viper.SetDefault("maxfiles", 10000)
	viper.SetDefault("testdir", "../tests/recombined_formatted/")
	viper.SetDefault("faileddir", "../tests/failed/")
	viper.SetDefault("fileext", ".baby")
	viper.SetDefault("runmode", "parallel")
	viper.SetDefault("printpassed", false)
	viper.SetDefault("hideverbose", false)

	// Bind flags to Viper.
	viper.BindPFlags(rootCmd.PersistentFlags())
	testCmd.Flags().VisitAll(func(f *pflag.Flag) {
		viper.BindPFlag(f.Name, f)
	})
	failedCmd.Flags().VisitAll(func(f *pflag.Flag) {
		viper.BindPFlag(f.Name, f)
	})
	clearCmd.Flags().VisitAll(func(f *pflag.Flag) {
		viper.BindPFlag(f.Name, f)
	})

	// Add commands to root.
	rootCmd.AddCommand(testCmd)
	rootCmd.AddCommand(failedCmd)
	rootCmd.AddCommand(clearCmd)
	rootCmd.AddCommand(setupCmd)
	rootCmd.AddCommand(compileCmd)
	rootCmd.AddCommand(parseCmd)
}

func initConfig() {
	viper.SetConfigName("config")
	viper.SetConfigType("yaml")
	viper.AddConfigPath(".")

	if err := viper.ReadInConfig(); err != nil {
		if _, ok := err.(viper.ConfigFileNotFoundError); ok {
			// Config file not found; ignore error
		} else {
			log.Fatalf("Failed to read config file: %v", err)
		}
	}

	if err := viper.Unmarshal(&cfg); err != nil {
		log.Fatalf("Failed to unmarshal config: %v", err)
	}
}

func main() {
	Execute()
}

func runTests(dir string, isFailedRun bool) {
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		os.MkdirAll(dir, os.ModePerm)
	}

	fmt.Printf("Go ANTLR4 Parser Performance Test\n")
	fmt.Printf("Go version: %s\n", runtime.Version())
	fmt.Printf("CPU cores: %d\n", runtime.NumCPU())
	fmt.Printf("GOMAXPROCS: %d\n\n", runtime.GOMAXPROCS(0))

	files, err := getTestFiles(dir)
	if err != nil {
		log.Fatalf("Failed to get test files: %v", err)
	}

	if len(files) == 0 {
		fmt.Printf("No test files found in %s\n", dir)
		return
	}

	fmt.Printf("Found %d test files\n\n", len(files))

	if cfg.RunMode == "sequential" {
		runSequential(files, isFailedRun, cfg.PrintPassed, cfg.HideVerbose)
	} else {
		runParallel(files, isFailedRun, cfg.PrintPassed, cfg.HideVerbose)
	}
}

func downloadAntlrJar() {
	fmt.Printf("Downloading ANTLR4 JAR to %s...\n", ANTLR_JAR)
	if _, err := os.Stat(ANTLR_JAR); os.IsNotExist(err) {
		resp, err := http.Get(ANTLR_URL)
		if err != nil {
			log.Fatalf("Failed to download ANTLR4 JAR: %v", err)
		}
		defer resp.Body.Close()

		out, err := os.Create(ANTLR_JAR)
		if err != nil {
			log.Fatalf("Failed to create ANTLR4 JAR file: %v", err)
		}
		defer out.Close()

		_, err = io.Copy(out, resp.Body)
		if err != nil {
			log.Fatalf("Failed to save ANTLR4 JAR: %v", err)
		}
		fmt.Println("ANTLR4 JAR downloaded successfully.")
	} else {
		fmt.Println("ANTLR4 JAR already exists.")
	}
}

func generateParser() {
	fmt.Printf("Generating parser from %s...\n", GRAMMAR)
	if _, err := os.Stat(PARSER_DIR); os.IsNotExist(err) {
		os.MkdirAll(PARSER_DIR, os.ModePerm)
	}

	cmd := exec.Command("java", "-jar", ANTLR_JAR, "-Dlanguage=Go", "-o", PARSER_DIR, "-visitor", GRAMMAR)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err := cmd.Run()
	if err != nil {
		log.Fatalf("Failed to generate parser: %v", err)
	}
	fmt.Printf("Parser generated in %s/\n", PARSER_DIR)
}

func compileFile(filepath string, verbose bool) {
	if verbose {
		parseFile(filepath)
	}
	fmt.Printf("Compiling %s\n", filepath)
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

	// --- Semantic Analysis ---
	builder := NewSymbolTableBuilder()
	tree.Accept(builder)
	if len(builder.errors) > 0 {
		fmt.Printf("%sSemantic errors in %s:%s\n", ColorRed, filepath, ColorReset)
		for _, e := range builder.errors {
			fmt.Printf("- line %d: %s\n", e.line, e.msg)
		}
		return
	}

	checker := NewSemanticChecker(builder.symbolTable)
	tree.Accept(checker)
	checker.analyzeFlow()
	if len(checker.errors) > 0 {
		fmt.Printf("%sSemantic errors in %s:%s\n", ColorRed, filepath, ColorReset)
		for _, e := range checker.errors {
			fmt.Printf("- line %d: %s\n", e.line, e.msg)
		}
		return
	}
	// --- End Semantic Analysis ---

	// Generate LLVM IR
	ir, codegenErrors := Generate(tree, checker.symbolTable, verbose, filepath)
	if len(codegenErrors) > 0 {
		fmt.Printf("%sCode generation errors in %s:%s\n", ColorRed, filepath, ColorReset)
		for _, e := range codegenErrors {
			fmt.Printf("- line %d: %s\n", e.line, e.msg)
		}
		return
	}

	// Write IR to file
	outputFile := strings.TrimSuffix(filepath, ".baby") + ".ll"
	err = os.WriteFile(outputFile, []byte(ir), 0644)
	if err != nil {
		log.Fatalf("Failed to write LLVM IR to file: %v", err)
	}

	exeFile, err := CompileIRToExecutable(outputFile)
	if err != nil {
		log.Fatalf("Failed to compile LLVM IR: %v", err)
	}

	fmt.Printf("%sSuccessfully compiled %s to %s%s\n", ColorGreen, filepath, exeFile, ColorReset)
}