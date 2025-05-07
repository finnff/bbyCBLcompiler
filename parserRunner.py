import os
import sys
import shutil
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
        print(
            f"{Colors.RED}[ERROR] {recognizer.__class__.__name__} line {line}:{column} {msg}{Colors.RESET}"
        )


def print_ast_tree(tree, level=0):
    indent = "  " * level
    if isinstance(tree, TerminalNode):
        symbol_text = tree.getSymbol().text
        print(
            f"{indent}{Colors.CYAN}[Terminal] {Colors.BLUE}{symbol_text}{Colors.RESET}"
        )
    else:
        rule_name = parser.__class__.ruleNames[tree.getRuleIndex()]
        print(f"{indent}{Colors.MAGENTA}[Rule] {Colors.GREEN}{rule_name}{Colors.RESET}")
        for i in range(tree.getChildCount()):
            print_ast_tree(tree.getChild(i), level + 1)


def parse_file(filepath, test_index, total_tests):
    # Read raw contents
    with open(filepath, "r") as f:
        raw_lines = f.readlines()
        raw_text = "".join(raw_lines)

    # Preprocess COBOL fixed-format lines (catch any indicator errors)
    try:
        processed_text = preprocess_cobol(raw_lines)
    except Exception as e:
        print(
            f"\n{Colors.RED}[ERROR] === Preprocessing error in: {filepath} === {Colors.RESET}"
        )
        print(
            f"\n{Colors.YELLOW}=== File Contents: {os.path.basename(filepath)} ==={Colors.RESET}"
        )
        print(raw_text)
        print(f"[FAILED] {filepath}: {e}")
        return False, True

    # Lexing and parsing setup
    input_stream = InputStream(processed_text)
    lexer = bbyCBLLexer(input_stream)
    lexer.removeErrorListeners()
    lex_errors = LoggingErrorListener()
    lexer.addErrorListener(lex_errors)
    tokens = CommonTokenStream(lexer)

    global parser
    parser = bbyCBLParser(tokens)
    parser.removeErrorListeners()
    parse_errors = LoggingErrorListener()
    parser.addErrorListener(parse_errors)

    if PARSE:
        try:
            tree = parser.program()
            has_syntax_error = lex_errors.has_error or parse_errors.has_error

            if has_syntax_error:
                print(f"\n=== Syntax error in: {filepath} ===")
                # Always print file contents on syntax error
                print(
                    f"\n{Colors.YELLOW}=== File Contents: {os.path.basename(filepath)} ==={Colors.RESET}"
                )
                print(raw_text)
                return False, True

            # Successful parse
            print(
                f"{Colors.GREEN} [PASS] ({test_index}/{total_tests}) {os.path.basename(filepath)} {Colors.RESET}"
            )
            # Print contents & AST if requested
            if ALWAYS_PRINT:
                print(
                    f"\n{Colors.YELLOW}=== File Contents: {os.path.basename(filepath)} ==={Colors.RESET}"
                )
                print(processed_text)
            if SHOW_AST:
                print(
                    f"\n{Colors.YELLOW}=== Abstract Syntax Tree: {os.path.basename(filepath)} ==={Colors.RESET}"
                )
                print_ast_tree(tree)
                print()
            return True, False

        except Exception as e:
            # Parser exception
            print(f"\n=== Error in: {filepath} ===")
            if ALWAYS_PRINT:
                print(
                    f"\n{Colors.YELLOW}=== File Contents: {os.path.basename(filepath)} ==={Colors.RESET}"
                )
                print(raw_text)
            print(f"[FAILED] {filepath}: {e}")
            return False, True
    else:
        # Non-parse mode: just print processed text
        print(processed_text)
        return False, False


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


def main():
    global MAX_TEST, TEST_DIR

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

    failed_tests = []
    test_files = sorted([f for f in os.listdir(TEST_DIR) if f.endswith(".baby")])
    total = len(test_files)
    MAX = MAX_TEST if 0 < MAX_TEST <= total else total
    test_files = test_files[:MAX]

    ran = passed = errors = 0
    for idx, filename in enumerate(test_files, 1):
        filepath = os.path.join(TEST_DIR, filename)
        ok, err = parse_file(filepath, idx, MAX)
        ran += 1
        if ok:
            passed += 1
        if err:
            errors += 1
            failed_tests.append(filename)

    print(f"\n\n\n{Colors.YELLOW}Total tests ran: {ran}{Colors.RESET}")
    print(f"{Colors.GREEN}Tests passed: {passed}{Colors.RESET}")
    print(f"{Colors.RED}Syntax errors: {errors}{Colors.RESET}")
    if errors > 0:
        print(f"{Colors.RED}Failed tests: \n {' '.join(failed_tests)}{Colors.RESET}")
        # Save failed tests for future runs
        save_failed_tests(failed_tests)

    # Display command options if there are failed tests or always
    display_usage()


if __name__ == "__main__":
    main()
