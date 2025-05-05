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






####  Flawless Run from pre-Case Sensitivity Implementition
[
    <img
        src="https://github.com/user-attachments/assets/c5ed2b19-6457-4e60-b64c-0df62644f447" 
        width="75%"
        title="flawless run from pre-case sensitivity implementition"
        alt="Flawless run from pre-Case Sensitivity implementition"
    />
](https://github.com/user-attachments/assets/c5ed2b19-6457-4e60-b64c-0df62644f447)



### Lexxer+Parser Progress:

* ✅ 1XP - Basic statements (ACCEPT, ALTER, GO TO, IF, PERFORM, SIGNAL)
* ✅ 2XP - COPY, DISPLAY, IDENTIFICATION DIVISION
* ✅ 2XP - Arithmetic operations (ADD, CALL, DIVIDE, MOVE, MULTIPLY, SUBTRACT)
* ✅ 3XP - DATA DIVISION (nested structures with OF)
* ✅ 3XP - EVALUATE, LOOP (detachable clauses)
* ✅ 4XP - Position-based parsing & line continuations
* ✅ 4XP - Sentences, statements (NEXT SENTENCE, STOP)
* ✅ 5XP - Case insensitivity exceptions
* ⭐ 1XP - Ambiguous case handling -> see glue+join+continuation for line splitting of `preprocess_cobol()`

approx 26/30 XP?

### Bugs

- Set `MAX_TEST` to 2000+ and we run into the final last persisting issues with the parser. (This might be pre-processing related though)


bash 
```
[ERROR] bbyCBLParser line 4:34 mismatched input 'OF' expecting {'OCCURS', '.'}

=== Syntax error in: ./tests/recombined_formatted/test_105670.baby ===

=== File Contents: test_105670.baby ===
       IDENTIFICATION DIVISION.
       DATA DIVISION.
                           10 V1 PICTURE IS 999.
                           10 V2 LIKE V1 OF V1.



[ERROR] bbyCBLParser line 9:42 extraneous input 'C' expecting '.'

=== Syntax error in: ./tests/recombined_formatted/test_106837.baby ===

=== File Contents: test_106837.baby ===
       IDENTIFICATION DIVISION.
           PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       A.
           DISPLAY "A" WITH NO ADVANCING
           PERFORM B.
           PERFORM B THROUGH C.
           PERFORM D THROUGH E.
           PERFORM C THROUGH D.
           DISPLAY "THIS SHOULDNT BE DISPLAYED" WITH NO ADVANCING.
       B.
           LOOP VARYING B FROM C TO D BY "K"
        ..... 

```

#### Example Parse Error debug info:


[
    <img
        src="https://github.com/user-attachments/assets/e77a13fe-f5a0-4ae6-9072-d6ec03db8710" 
        width="70%"
        title="Example Parsing Errors have debugging"
        alt="Example Parsing Errors have debugging"
    />
](https://github.com/user-attachments/assets/e77a13fe-f5a0-4ae6-9072-d6ec03db8710)

