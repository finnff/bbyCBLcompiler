import os
import sys
import shutil
import multiprocessing
from concurrent.futures import ProcessPoolExecutor, as_completed
from antlr4 import FileStream, CommonTokenStream, InputStream
from parser.bbyCBLLexer import bbyCBLLexer
from parser.bbyCBLParser import bbyCBLParser
from antlr4.error.ErrorListener import ErrorListener
from antlr4.tree.Tree import TerminalNode
from preProcessor import preprocess_cobol

MAX_TEST = 250  # Amount of test cases to run, 0 for all
PARSE = True  # Parse the data (instead of printing it)
ALWAYS_PRINT = False  # Print Passed Test Contents and on errors
SHOW_AST = False  # Print the abstract syntax tree for each file
TEST_DIR = "./tests/recombined_formatted/"  # Path to .baby test files
FAILED_DIR = "./tests/failed_prev_run/"  # Directory to store failed tests


# ANSI color codes
class Colors:
    YELLOW = "\033[93m"
    GREEN = "\033[92m"
    RED = "\033[91m"
    BLUE = "\033[94m"
    CYAN = "\033[96m"
    MAGENTA = "\033[95m"
    RESET = "\033[0m"


class LoggingErrorListener(ErrorListener):
    def __init__(self):
        self.has_error = False

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.has_error = True
        return f"{Colors.RED}[ERROR] {recognizer.__class__.__name__} line {line}:{column} {msg}{Colors.RESET}"


def print_ast_tree(tree, parser, level=0):
    indent = "  " * level
    if isinstance(tree, TerminalNode):
        symbol_text = tree.getSymbol().text
        return (
            f"{indent}{Colors.CYAN}[Terminal] {Colors.BLUE}{symbol_text}{Colors.RESET}"
        )
    else:
        rule_name = parser.__class__.ruleNames[tree.getRuleIndex()]
        result = (
            f"{indent}{Colors.MAGENTA}[Rule] {Colors.GREEN}{rule_name}{Colors.RESET}\n"
        )
        for i in range(tree.getChildCount()):
            result += print_ast_tree(tree.getChild(i), parser, level + 1) + "\n"
        return result.rstrip()


def parse_single_file(filepath):
    """Parse a single file and return its results - designed for multiprocessing."""
    filename = os.path.basename(filepath)

    # Read raw contents
    try:
        with open(filepath, "r") as f:
            raw_lines = f.readlines()
            raw_text = "".join(raw_lines)
    except Exception as e:
        return {
            "filename": filename,
            "filepath": filepath,
            "success": False,
            "error": True,
            "message": f"Failed to read file: {str(e)}",
            "content": "",
        }

    # Preprocess COBOL fixed-format lines (catch any indicator errors)
    try:
        processed_text = preprocess_cobol(raw_lines)
    except Exception as e:
        return {
            "filename": filename,
            "filepath": filepath,
            "success": False,
            "error": True,
            "message": f"Preprocessing error: {str(e)}",
            "content": raw_text,
        }

    # Lexing and parsing setup
    input_stream = InputStream(processed_text)
    lexer = bbyCBLLexer(input_stream)
    lexer.removeErrorListeners()
    lex_errors = LoggingErrorListener()
    lexer.addErrorListener(lex_errors)
    tokens = CommonTokenStream(lexer)

    parser = bbyCBLParser(tokens)
    parser.removeErrorListeners()
    parse_errors = LoggingErrorListener()
    parser.addErrorListener(parse_errors)

    if PARSE:
        try:
            tree = parser.program()
            has_syntax_error = lex_errors.has_error or parse_errors.has_error

            if has_syntax_error:
                return {
                    "filename": filename,
                    "filepath": filepath,
                    "success": False,
                    "error": True,
                    "message": "Syntax error during parsing",
                    "content": raw_text,
                }

            # Successful parse
            result = {
                "filename": filename,
                "filepath": filepath,
                "success": True,
                "error": False,
                "message": f"Successfully parsed {filename}",
                "content": processed_text if ALWAYS_PRINT else "",
            }

            # Add AST if requested
            if SHOW_AST:
                result["ast"] = print_ast_tree(tree, parser)

            return result

        except Exception as e:
            # Parser exception
            return {
                "filename": filename,
                "filepath": filepath,
                "success": False,
                "error": True,
                "message": f"Parser exception: {str(e)}",
                "content": raw_text if ALWAYS_PRINT else "",
            }
    else:
        # Non-parse mode: just return processed text
        return {
            "filename": filename,
            "filepath": filepath,
            "success": True,
            "error": False,
            "message": "Processed text (non-parse mode)",
            "content": processed_text,
        }


def save_failed_tests(failed_tests):
    """Copy failed test files to the failed tests directory"""
    if not failed_tests:
        return

    # Create the directory if it doesn't exist
    os.makedirs(FAILED_DIR, exist_ok=True)

    # Skip copying if we're already running from the failed tests directory
    if TEST_DIR == FAILED_DIR:
        print(
            f"\n{Colors.YELLOW}Already running from failed tests directory. Skipping copy.{Colors.RESET}"
        )
        return

    for filename in failed_tests:
        src_path = os.path.join(TEST_DIR, filename)
        dst_path = os.path.join(FAILED_DIR, filename)
        shutil.copy2(src_path, dst_path)

    print(f"\n{Colors.YELLOW}Failed tests copied to: {FAILED_DIR}{Colors.RESET}")


def clear_failed_tests():
    """Clear the failed tests directory"""
    if os.path.exists(FAILED_DIR):
        for file in os.listdir(FAILED_DIR):
            file_path = os.path.join(FAILED_DIR, file)
            if os.path.isfile(file_path):
                os.remove(file_path)
        print(
            f"{Colors.YELLOW}Cleared previous failed tests directory: {FAILED_DIR}{Colors.RESET}"
        )
    else:
        print(f"{Colors.YELLOW}No failed tests directory found.{Colors.RESET}")


def display_usage():
    """Display usage information"""
    print(f"\n{Colors.CYAN}Usage options:{Colors.RESET}")
    print(f"  {Colors.CYAN}python script.py{Colors.RESET}              - Run all tests")
    print(
        f"  {Colors.CYAN}python script.py -FAILED{Colors.RESET}      - Run only previously failed tests"
    )
    print(
        f"  {Colors.CYAN}python script.py -CLEAR{Colors.RESET}       - Clear the failed tests directory"
    )
    print(
        f"  {Colors.CYAN}python script.py -WORKERS N{Colors.RESET}   - Use N worker processes (default: all CPU cores)"
    )


def main():
    global MAX_TEST, TEST_DIR

    # Number of worker processes to use (default to CPU count)
    workers = multiprocessing.cpu_count()

    # Process command line arguments
    if len(sys.argv) > 1:
        if sys.argv[1] == "-FAILED":
            # Run only previously failed tests
            if os.path.exists(FAILED_DIR) and os.listdir(FAILED_DIR):
                TEST_DIR = FAILED_DIR
                print(
                    f"{Colors.YELLOW}Running only previously failed tests from: {FAILED_DIR}{Colors.RESET}"
                )
            else:
                print(
                    f"{Colors.YELLOW}No previous failed tests found. Running all tests.{Colors.RESET}"
                )
        elif sys.argv[1] == "-CLEAR":
            # Clear the failed tests directory
            clear_failed_tests()
            return
        elif sys.argv[1] == "-WORKERS" and len(sys.argv) > 2:
            try:
                workers = int(sys.argv[2])
                print(f"{Colors.YELLOW}Using {workers} worker processes{Colors.RESET}")
            except ValueError:
                print(
                    f"{Colors.RED}Invalid number of workers. Using default: {workers}{Colors.RESET}"
                )

    # Get test files
    test_files = sorted([f for f in os.listdir(TEST_DIR) if f.endswith(".baby")])
    total = len(test_files)
    MAX = MAX_TEST if 0 < MAX_TEST <= total else total
    test_files = test_files[:MAX]

    print(
        f"{Colors.YELLOW}Running {len(test_files)} tests using {workers} worker processes{Colors.RESET}"
    )

    # Create a list of full file paths
    file_paths = [os.path.join(TEST_DIR, filename) for filename in test_files]

    # Stats counters
    ran = 0
    passed = 0
    errors = 0
    failed_tests = []

    # Use process pool to parse files in parallel
    with ProcessPoolExecutor(max_workers=workers) as executor:
        # Submit all files for processing
        future_to_filepath = {
            executor.submit(parse_single_file, filepath): filepath
            for filepath in file_paths
        }

        # Process results as they complete
        for i, future in enumerate(as_completed(future_to_filepath), 1):
            filepath = future_to_filepath[future]
            filename = os.path.basename(filepath)

            try:
                result = future.result()
                ran += 1

                if result["success"]:
                    passed += 1
                    print(
                        f"{Colors.GREEN} [PASS] ({i}/{len(test_files)}) {filename} {Colors.RESET}"
                    )

                    # Print additional info if requested
                    if ALWAYS_PRINT and result["content"]:
                        print(
                            f"\n{Colors.YELLOW}=== File Contents: {filename} ==={Colors.RESET}"
                        )
                        print(result["content"])

                    if SHOW_AST and "ast" in result:
                        print(
                            f"\n{Colors.YELLOW}=== Abstract Syntax Tree: {filename} ==={Colors.RESET}"
                        )
                        print(result["ast"])
                        print()
                else:
                    errors += 1
                    failed_tests.append(filename)
                    print(
                        f"\n{Colors.RED}[ERROR] === {result['message']}: {filename} === {Colors.RESET}"
                    )

                    # Always print file contents on error
                    if result["content"]:
                        print(
                            f"\n{Colors.YELLOW}=== File Contents: {filename} ==={Colors.RESET}"
                        )
                        print(result["content"])

                    print(f"[FAILED] {filepath}")

            except Exception as e:
                # This shouldn't happen (errors should be captured in parse_single_file)
                print(
                    f"{Colors.RED}[ERROR] Failed to process {filename}: {e}{Colors.RESET}"
                )
                errors += 1
                failed_tests.append(filename)

    # Print summary
    print(f"\n\n\n{Colors.YELLOW}Total tests ran: {ran}{Colors.RESET}")
    print(f"{Colors.GREEN}Tests passed: {passed}{Colors.RESET}")
    print(f"{Colors.RED}Syntax errors: {errors}{Colors.RESET}")

    if errors > 0:
        print(f"{Colors.RED}Failed tests: \n {' '.join(failed_tests)}{Colors.RESET}")
        # Save failed tests for future runs
        save_failed_tests(failed_tests)

    # Display command options
    display_usage()


if __name__ == "__main__":
    # This guards against spawning multiple processes in Windows
    multiprocessing.freeze_support()
    main()
