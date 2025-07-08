package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/exec"
	"runtime"

	"github.com/spf13/cobra"
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
	MaxTest   int    `mapstructure:"maxtest"`
	TestDir   string `mapstructure:"testdir"`
	FailedDir string `mapstructure:"faileddir"`
	FileExt   string `mapstructure:"fileext"`
	RunMode   string `mapstructure:"runmode"`
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
		runTests(cfg.TestDir)
	},
}

var failedCmd = &cobra.Command{
	Use:   "failed",
	Short: "Run only previously failed tests",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("Running failed tests...")
		runTests(cfg.FailedDir)
	},
}

var clearCmd = &cobra.Command{
	Use:   "clear",
	Short: "Clear the failed tests directory",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Printf("Clearing failed tests directory: %s\n", cfg.FailedDir)
		os.RemoveAll(cfg.FailedDir)
		os.MkdirAll(cfg.FailedDir, os.ModePerm)
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

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	// Define flags. Do NOT bind them directly to cfg fields here.
	// Viper will handle the binding and precedence.
	rootCmd.PersistentFlags().IntP("maxtest", "n", 10000, "Set to 0 for all files")
	rootCmd.PersistentFlags().StringP("testdir", "", "../tests/recombined_formatted/", "Directory with test files")
	rootCmd.PersistentFlags().StringP("faileddir", "", "../tests/failed/", "Directory for failed tests")
	rootCmd.PersistentFlags().StringP("fileext", "", ".baby", "Test file extension")
	rootCmd.PersistentFlags().StringP("runmode", "", "parallel", "Run mode: sequential or parallel")

	// Set hardcoded defaults in Viper. These have the lowest precedence.
	viper.SetDefault("maxtest", 10000)
	viper.SetDefault("testdir", "../tests/recombined_formatted/")
	viper.SetDefault("faileddir", "../tests/failed/")
	viper.SetDefault("fileext", ".baby")
	viper.SetDefault("runmode", "parallel")

	// Bind all persistent flags to Viper. This tells Viper to look at command-line flags.
	viper.BindPFlags(rootCmd.PersistentFlags())

	rootCmd.AddCommand(testCmd)
	rootCmd.AddCommand(failedCmd)
	rootCmd.AddCommand(clearCmd)
	rootCmd.AddCommand(setupCmd)
}

// initConfig reads the configuration from config.yaml and unmarshals into cfg.
// This function is called by cobra.OnInitialize, ensuring it runs after flags are parsed
// and before command Run functions are executed.
func initConfig() {
	// Set Viper to read from config.yaml
	viper.SetConfigName("config")
	viper.SetConfigType("yaml")
	viper.AddConfigPath(".")

	// Read the config file. If not found, Viper will use defaults and flag values.
	if err := viper.ReadInConfig(); err != nil {
		if _, ok := err.(viper.ConfigFileNotFoundError); ok {
			fmt.Println("Config file not found, using default values or flags.")
		} else {
			log.Fatalf("Failed to read config file: %v", err)
		}
	}

	// Unmarshal the final resolved configuration from Viper into the cfg struct.
	// Viper's precedence rules (flags > config file > defaults) are applied here.
	if err := viper.Unmarshal(&cfg); err != nil {
		log.Fatalf("Failed to unmarshal config: %v", err)
	}
}

func main() {
	// Execute the root command. This will parse flags and run the appropriate command.
	Execute()
}

// runTests orchestrates the test execution based on the provided directory.
func runTests(dir string) {
	// Create the directory if it doesn't exist
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		os.MkdirAll(dir, os.ModePerm)
	}

	fmt.Printf("Go ANTLR4 Parser Performance Test\n")
	fmt.Printf("Go version: %s\n", runtime.Version())
	fmt.Printf("CPU cores: %d\n", runtime.NumCPU())
	fmt.Printf("GOMAXPROCS: %d\n\n", runtime.GOMAXPROCS(0))

	// Get test files
	files, err := getTestFiles(dir)
	if err != nil {
		log.Fatalf("Failed to get test files: %v", err)
	}

	if len(files) == 0 {
		fmt.Printf("No test files found in %s\n", dir)
		return
	}

	fmt.Printf("Found %d test files\n\n", len(files))

	// Run different approaches based on command line args
	if cfg.RunMode == "sequential" {
		runSequential(files)
	} else {
		runParallel(files)
	}
}

// downloadAntlrJar downloads the ANTLR4 complete JAR if it's not already present.
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

// generateParser generates the Go parser code using the ANTLR4 JAR.
func generateParser() {
	fmt.Printf("Generating parser from %s...\n", GRAMMAR)
	// Create parser directory if it doesn't exist
	if _, err := os.Stat(PARSER_DIR); os.IsNotExist(err) {
		os.MkdirAll(PARSER_DIR, os.ModePerm)
	}

	cmd := exec.Command("java", "-jar", ANTLR_JAR, "-Dlanguage=Go", "-o", PARSER_DIR, GRAMMAR)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err := cmd.Run()
	if err != nil {
		log.Fatalf("Failed to generate parser: %v", err)
	}
	fmt.Printf("Parser generated in %s/\n", PARSER_DIR)
}
