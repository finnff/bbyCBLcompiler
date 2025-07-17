## Checkpoint Data

### Lexical Analysis

| XP  | Description                                                                 | Test Files |
|:----|:----------------------------------------------------------------------------|:-----------|
| 1XP | Case Insensitivity in Keywords and Identifiers: Support mixed case for keywords and identifiers, treating them case-insensitively. | <ul><li>test01.baby (mixed case keywords)</li><li>test02.baby (case insensitive identifiers)</li><li>test11.baby (all lowercase structure)</li><li>test12.baby (mixed case variables)</li><li>test21.baby (mixed case PICTURE and variables)</li></ul> |
| 2XP | String Literal Continuation: Support proper continuation of string literals across lines using '-' in column 7, without closing quotes on the initial line. | <ul><li>test03.baby (valid continuation)</li><li>test08F.baby (invalid continuation no space)</li><li>test13.baby (invalid missing quotes)</li><li>test18.baby (valid continuation with space)</li><li>test23.baby (valid multi-line with NO ADVANCING)</li></ul> |
| 1XP | Comments Handling: Support '*' in column 7 for comment lines, preserving content exactly, including mixed case and code-like text. | <ul><li>test04.baby (preserved mixed case comments)</li><li>test14.baby (comments with code-like text)</li></ul> |
| 2XP | Figurative Constants: Support constants like HIGH-VALUES, LOW-VALUES, SPACES in MOVE and other operations. | <ul><li>test22.baby (HIGH-VALUES in MOVE)</li><li>test40.baby (LOW-VALUES in MOVE and ADD)</li><li>100doors.baby (HIGH-VALUES and SPACES in MOVE)</li></ul> |
| 1XP | Invalid Status Indicators: Reject invalid characters (e.g., '#') in column 7, allowing only '*' for comments or blank. | <ul><li>test07F.baby (invalid # indicator)</li><li>test17.baby (valid * comment)</li></ul> |

### Syntax Parsing

| XP  | Description                                                                 | Test Files |
|:----|:----------------------------------------------------------------------------|:-----------|
| 2XP | Area A/B Rules and Indentation: Enforce division/section headers in Area A (columns 8-11), statements in Area B (columns 12-72), and reject misalignments. | <ul><li>test09.baby (invalid header indentation)</li><li>test10.baby (valid statement indentation)</li><li>test19.baby (invalid procedure indentation)</li><li>test20.baby (valid move indentation)</li></ul> |
| 2XP | Statement Continuation: Support proper line continuation for statements and literals using '-' in column 7. | <ul><li>test03.baby (valid string continuation)</li><li>test08F.baby (invalid no space after -)</li><li>test13.baby (invalid missing quotes)</li><li>test18.baby (valid with space after -)</li><li>test23.baby (valid multi-line display)</li></ul> |
| 1XP | Keywords as Variable Names: Allow reserved keywords (e.g., END, IF) to be used as variable names without conflict. | <ul><li>test05.baby (END and IF as variables)</li><li>test15.baby (END as variable with END-IF)</li></ul> |
| 1XP | Basic Program Structure: Support minimal IDENTIFICATION, DATA, PROCEDURE divisions, with optional PROGRAM-ID. | <ul><li>test01.baby (minimal with PROGRAM-ID)</li><li>test06.baby (basic with MOVE and DISPLAY)</li><li>test11.baby (lowercase with STOP RUN)</li><li>test16.baby (basic alphanumeric MOVE)</li><li>test_display.baby (simple DISPLAY)</li></ul> |

### Symbol Table and Data Handling

| XP  | Description                                                                 | Test Files |
|:----|:----------------------------------------------------------------------------|:-----------|
| 1XP | PICTURE Clauses: Define variables with basic PICTURE (e.g., 9(3), X(5), S99) for numeric, alphanumeric, signed types. | <ul><li>test01.baby (numeric 99)</li><li>test02.baby (alphanumeric XX and numeric 99)</li><li>test06.baby (alphanumeric X(5))</li><li>test11.baby (numeric 99)</li><li>test12.baby (alphanumeric XX and numeric 99)</li><li>test16.baby (alphanumeric X(5))</li><li>test22.baby (signed S99)</li><li>test38.baby (signed S99)</li></ul> |
| 2XP | LIKE Clauses: Copy data types/pictures using LIKE for new variables or arrays. | <ul><li>test32.baby (LIKE for array copies)</li></ul> |
| 3XP | OCCURS Clauses: Support arrays/lists for fields/records with OCCURS n TIMES, including indexing. | <ul><li>test24.baby (OCCURS 3 TIMES with indexing)</li><li>test32.baby (LIKE with OCCURS 2 TIMES)</li><li>test41.baby (OCCURS 3 TIMES with LOOP indexing)</li><li>100doors.baby (OCCURS 100 TIMES with indexing)</li></ul> |

### Control Flow

| XP  | Description                                                                 | Test Files |
|:----|:----------------------------------------------------------------------------|:-----------|
| 2XP | IF/THEN/ELSE Statements: Basic conditional logic with THEN, ELSE, and proper scoping. | <ul><li>test05.baby (basic IF = THEN)</li><li>fizzbuzz.baby (multiple IF for remainders)</li><li>99bottles.baby (IF = 1 THEN ALTER)</li></ul> |
| 3XP | Nested IF Statements: Support nested IFs with explicit END or END-IF for scoping. | <ul><li>test15.baby (nested IF with END-IF)</li><li>test37.baby (nested IF ELSE with END)</li></ul> |
| 3XP | Contracted Conditions: Abbreviated conditions like IF x = a OR b OR c. | <ul><li>test25.baby (IF = OR contracted)</li></ul> |
| 4XP | EVALUATE Statement: Multi-condition evaluation with ALSO, WHEN THROUGH, OTHER. | <ul><li>test33.baby (EVALUATE ALSO with THROUGH and OTHER)</li></ul> |
| 2XP | LOOP Constructs: Support LOOP VARYING (FROM/TO/BY/UNTIL), WHILE, UNTIL conditions. | <ul><li>test24.baby (VARYING FROM TO)</li><li>test28.baby (WHILE <)</li><li>test38.baby (VARYING BY UNTIL <)</li><li>test41.baby (VARYING FROM TO)</li><li>fizzbuzz.baby (VARYING TO)</li><li>99bottles.baby (VARYING FROM BY)</li><li>100doors.baby (VARYING UNTIL =)</li></ul> |
| 3XP | PERFORM Statements: PERFORM para THROUGH para, PERFORM n TIMES. | <ul><li>test29.baby (PERFORM n TIMES with GO TO)</li><li>test35.baby (PERFORM THROUGH paras)</li><li>99bottles.baby (PERFORM THROUGH END)</li></ul> |
| 2XP | GO TO and Labels: Unconditional jumps with GO TO label, including within procedures. | <ul><li>test29.baby (GO TO in PERFORM)</li><li>test30.baby (GO TO with ALTER)</li><li>99bottles.baby (GO TO in altered paths)</li></ul> |
| 4XP | ALTER Statement: Dynamically alter GO TO targets with ALTER para TO PROCEED TO label. | <ul><li>test30.baby (ALTER TO PROCEED TO)</li><li>99bottles.baby (ALTER COUNT-BOTTLES TO SINGLE-BOTTLE)</li></ul> |
| 3XP | NEXT SENTENCE: Skip to the next sentence in control flow. | <ul><li>test25.baby (NEXT SENTENCE after IF)</li><li>test35.baby (NEXT SENTENCE in PERFORM)</li><li>99bottles.baby (NEXT SENTENCE in COUNT-BOTTLES)</li></ul> |
| 5XP | Error Handling: Support SIGNAL HANDLER ON ERROR for runtime error trapping. | <ul><li>test34.baby (SIGNAL ON ERROR with divide by zero)</li></ul> |

### Arithmetic Operations

| XP  | Description                                                                 | Test Files |
|:----|:----------------------------------------------------------------------------|:-----------|
| 2XP | ADD Operation: ADD with TO, multiple operands, GIVING result (without modifying sources). | <ul><li>test26.baby (ADD GIVING)</li><li>test40.baby (ADD TO multiple with LOW-VALUES)</li></ul> |
| 2XP | SUBTRACT Operation: SUBTRACT with FROM, multiple operands/sources, GIVING result. | <ul><li>test36.baby (SUBTRACT FROM GIVING)</li><li>100doors.baby (SUBTRACT FROM in LOOP)</li></ul> |
| 2XP | MULTIPLY Operation: MULTIPLY with BY, updating target. | <ul><li>test28.baby (MULTIPLY BY in LOOP)</li></ul> |
| 2XP | DIVIDE Operation: DIVIDE INTO with GIVING quotient REMAINDER. | <ul><li>test27.baby (DIVIDE INTO GIVING REMAINDER)</li><li>fizzbuzz.baby (DIVIDE for remainders in IF)</li></ul> |

### Input/Output

| XP  | Description                                                                 | Test Files |
|:----|:----------------------------------------------------------------------------|:-----------|
| 1XP | DISPLAY Statement: Output literals/variables, with options like NO ADVANCING, DELIMITED BY SIZE/SPACE. | <ul><li>test06.baby (basic DISPLAY variable)</li><li>test16.baby (DISPLAY alphanumeric)</li><li>test23.baby (DISPLAY with NO ADVANCING continuation)</li><li>test31.baby (DISPLAY DELIMITED BY SPACE)</li><li>test39.baby (DISPLAY DELIMITED BY SIZE)</li><li>test_display.baby (simple literal DISPLAY)</li><li>fizzbuzz.baby (DISPLAY with NO ADVANCING)</li><li>99bottles.baby (multiple DISPLAYs)</li><li>100doors.baby (DISPLAY with NO ADVANCING)</li></ul> |
| 2XP | ACCEPT Statement: Input to variables from user/console. | <ul><li>test31.baby (ACCEPT to variable)</li></ul> |

### Other Features

| XP  | Description                                                                 | Test Files |
|:----|:----------------------------------------------------------------------------|:-----------|
| 1XP | MOVE Statement: Assign values, literals, figuratives to variables/arrays, including multiple targets. | <ul><li>test01.baby (MOVE numeric literal)</li><li>test02.baby (MOVE string to alphanumeric)</li><li>test06.baby (MOVE string to X(5))</li><li>test11.baby (MOVE to numeric)</li><li>test12.baby (MOVE to mixed variables)</li><li>test16.baby (MOVE string)</li><li>test22.baby (MOVE HIGH-VALUES)</li><li>test24.baby (MOVE to array elements)</li><li>test26.baby (MOVE to ADD operands)</li><li>test27.baby (MOVE to DIVIDE target)</li><li>test28.baby (MOVE to LOOP counter)</li><li>test32.baby (MOVE SPACES and string to arrays)</li><li>test36.baby (MOVE to SUBTRACT targets)</li><li>test38.baby (MOVE to signed counter)</li><li>test40.baby (MOVE LOW-VALUES)</li><li>test41.baby (MOVE to array elements)</li><li>100doors.baby (MOVE SPACES and HIGH-VALUES to array)</li></ul> |
| 1XP | Program Termination: Support STOP, STOP RUN. | <ul><li>test11.baby (STOP RUN)</li><li>test27.baby (STOP RUN after DIVIDE)</li><li>test28.baby (STOP after LOOP)</li><li>test29.baby (STOP after PERFORM)</li><li>test30.baby (STOP after ALTER)</li><li>test34.baby (STOP in HANDLER)</li><li>test35.baby (STOP after PERFORM THROUGH)</li><li>99bottles.baby (STOP after NO-BOTTLES-LEFT)</li></ul> 
