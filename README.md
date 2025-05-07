## Mainframe Minds bbyCBL Antlr4 Parser 👶

### Prereqisities

1. Extract test files:

```bash
cd tests
tar -xvf recombined_formatted.tar.gz
```

2. Install requirements (`antlr4-tools` + `antlr4-python3-runtime`)

```bash 
pip3 install -r requirements.txt
```

3. Generate parser

```bash
antlr4 -Dlanguage=Python3 bbyCBL.g4 -o parser   
```



### Usage


1. Run Parser on test cases

```bash
python3 parserRunner.py
```

* *There are some params /debugging options (printing AST's, logging, No. iterations) that can be configured in* `parserRunner.py`


There are also some parameters that can be passed to the script:

```bash
python parserRunner.py              # Run all tests
python parserRunner.py -FAILED      # Run only previously failed tests
python parserRunner.py -CLEAR       # Clear the failed tests directory
python parserRunner.py -WORKERS N   # Use N worker processes (default: all CPU cores)
```



#### recombined_formatted Test Case coverage:
[
    <img
        src="https://github.com/user-attachments/assets/897e8f60-5af5-4ef5-be55-d0d27c2b18b6"
        width="40%"
        title="recombined_formatted Test Case coverage"
        alt="recombined_formatted Test Case coverage"
    />
](https://github.com/user-attachments/assets/897e8f60-5af5-4ef5-be55-d0d27c2b18b6)


> [!WARNING]
> This current implementation of the parser is tuned almost exclusively for the recombined_formatted test cases. It's not syntactically pretty, particularly readable or adaptable for anything else, or even something that we want to continue working with while implementing the LLVM compiler backend. We're currently working on a more generalized version in the [dev branch](https://github.com/SilasGitHub/software-evolution/blob/dev/bbyCBL.g4) with takeaways gained from the lecture on 7-5, but this is still lacking some features as compared to this version for claiming XP.

### Lexxer+Parser Progress:



##### Basic Parsing Requirements ✅ 

| Requirement | Implemented | Location |
|-------------|-------------|----------|
| 1XP - Basic statements (ACCEPT, ALTER, GO TO, IF, PERFORM, SIGNAL) | ✅ | bbyCBL.g4: `acceptStmt`, `alterStmt`, `gotoStmt`, `ifStmt`, `performStmt`, `signalStmt` |
| 2XP - COPY, DISPLAY, IDENTIFICATION DIVISION | ✅ | bbyCBL.g4: `identificationDivision`, `identificationClause`, `copyStmt`, `displayStmt` |
| 2XP - Arithmetic operations (ADD, CALL, DIVIDE, MOVE, MULTIPLY, SUBTRACT) | ✅ | bbyCBL.g4: `addStmt`, `callStmt`, `divideStmt`, `moveStmt`, `multiplyStmt`, `subtractStmt` |
| 3XP - DATA DIVISION (nested structures with OF) | ✅ | bbyCBL.g4: `dataDivision`, `dataEntry`, and `qualifiedId` for OF references |
| 3XP - EVALUATE, LOOP (detachable clauses) | ✅ | bbyCBL.g4: `evaluateStmt`, `loopStmt` with interleaved `loopControl` and `statement` |
| 4XP - Sentences, statements (NEXT SENTENCE, STOP) | ✅ | bbyCBL.g4: `sentence`, `nextSentenceStmt`, `stopStmt` |

##### Position-based Parsing & Line Continuations (4XP) ✅ 

| Requirement | Implemented | Location |
|-------------|-------------|----------|
| Ignore columns 1-6 (sequence number) | ✅ | preprocess_cobol(): Skips first 6 columns |
| Process column 7 (line status indicator) | ✅ | preprocess_cobol(): Uses `indicator = line[6]` |
| Handle space for normal line | ✅ | preprocess_cobol(): `if indicator == " "` |
| Handle asterisk for comment line | ✅ | preprocess_cobol(): `if indicator == "*": continue` |
| Handle hyphen for line continuation | ✅ | preprocess_cobol(): `if indicator == "-"` |
| Raise error for other indicators | ✅ | preprocess_cobol(): `raise ValueError(f"Invalid line indicator...")` |
| Parse Area A (columns 8-11) | ✅ | preprocess_cobol(): `area_a = line[7:11]` and `validate_area()` |
| Parse Area B (columns 12-72) | ✅ | preprocess_cobol(): `area_b = line[11:72]` and `validate_area()` |
| Ignore columns 73-80 | ✅ | preprocess_cobol(): Only uses columns up to 72 |
| Process combined lines with continuations | ✅ | preprocess_cobol(): `current_line = current_line.rstrip() + glue + continuation` |

##### Case Insensitivity (5XP) ✅ 

| Requirement | Implemented | Location |
|-------------|-------------|----------|
| Case-insensitive keywords and identifiers | ✅ | normalize_case(): Converts keywords to uppercase |
| Case-sensitive string literals | ✅ | normalize_case(): `if in_string: out.append(ch)` |
| Case-sensitive PICTURE clauses | ✅ | normalize_case(): `if in_picture: out.append(ch)` |
| Case-sensitive comment lines | ✅ | preprocess_cobol(): Comment lines are skipped entirely |
| Case-sensitive ID division values | ✅ | normalize_case(): `if in_id_value: out.append(ch)` |
| Ambiguity resolution - uppercase keywords | ✅ | normalize_case(): `if lower in ALL_KEYWORDS: out.append(word.upper())` |

##### Whitespace Insignificance (5XP) ⚠️

| Requirement | Implemented | Location |
|-------------|-------------|----------|
| Preserve whitespace in strings | ✅ | normalize_case(): Preserves all characters in strings |
| Preserve whitespace in comments | ✅ | preprocess_cobol(): Comment lines are kept intact (though skipped) |
| Preserve whitespace in ID division values | ✅ | normalize_case(): Preserves characters in ID values |
| Ignore other whitespace | ⚠️ | Partial: ANTLR grammar has `WS: [ \t\r\n]+ -> skip;` but doesn't handle all cases |
| Ambiguity resolution with whitespace | ⚠️ | Partial: join_without_space logic helps but doesn't fully implement the test cases |







#### Example Parse Error debug info:


[
    <img
        src="https://github.com/user-attachments/assets/19600a5c-9751-4663-b377-640dadfed993"
        width="70%"
        title="Example Parsing Errors have debugging"
        alt="Example Parsing Errors have debugging"
    />
](https://github.com/user-attachments/assets/19600a5c-9751-4663-b377-640dadfed993)


