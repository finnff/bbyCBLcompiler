import os
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


def main():
    global MAX_TEST
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

    print(f"\n\n\n{Colors.YELLOW}Total tests ran: {ran}{Colors.RESET}")
    print(f"{Colors.GREEN}Tests passed: {passed}{Colors.RESET}")
    print(f"{Colors.RED}Syntax errors: {errors}{Colors.RESET}")


if __name__ == "__main__":
    main()
