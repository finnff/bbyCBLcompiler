# Handmade babyCOBOL Tests, ASTs IR and Binarys

This table tracks the implementation progress of COBOL language parsing requirements, showing which features have been completed, which test files validate them, and the current status of each requirement.

Each test case consists of four files located in `./bbycblgo_compiler/handmade/`:
- `.baby` - Handwritten test cases
- `.ast` - Abstract Syntax Tree representation
- `.ll` - LLVM IR representation  
- Binary - Compiled linux x86_64 executable (linked against RHEL libs)

## Legend

| Emoji | Meaning |
|:------|:--------|
| ✅ | Fully implemented |
| 🚧 | Missing some features |
| 🚫 | Not implemented |
| 📜 | Semantically enforced |
| 📟 | ANTLR parsed |



## Checkpoint Parsing

### General Parsing Requirements

| XP  | Description                                                                 | Test Files | Score | Explanation |
|:----|:----------------------------------------------------------------------------|:-----------|:------|:------------|
| 1XP | Full Parsing of Basic Statements: Support parsing for ACCEPT, ALTER, GO TO, IF, PERFORM, SIGNAL. Ensure AST/IR representation captures all elements. | <ul><li>test31.baby (ACCEPT)</li><li>test30.baby (ALTER)</li><li>test29.baby (GO TO)</li><li>test05.baby (IF)</li><li>test15.baby (nested IF)</li><li>test37.baby (nested IF ELSE)</li><li>fizzbuzz.baby (multiple IF)</li><li>99bottles.baby (IF with ALTER)</li><li>test29.baby (PERFORM)</li><li>test35.baby (PERFORM THROUGH)</li><li>test34.baby (SIGNAL)</li></ul> | ✅📟📜 |  |
| 2XP | Parsing of COPY, DISPLAY, IDENTIFICATION DIVISION: Include REPLACING clauses in COPY and DELIMITED BY in DISPLAY. Store key-value pairs from IDENTIFICATION DIVISION (e.g., PROGRAM-ID, AUTHOR). Infrastructure should handle all three divisions (IDENTIFICATION, DATA, PROCEDURE). | <ul><li>test_display.baby (DISPLAY)</li><li>test06.baby (DISPLAY)</li><li>test16.baby (DISPLAY)</li><li>test23.baby (DISPLAY with NO ADVANCING)</li><li>test31.baby (DISPLAY DELIMITED)</li><li>test39.baby (DISPLAY DELIMITED BY SIZE)</li><li>fizzbuzz.baby (DISPLAY)</li><li>99bottles.baby (DISPLAY)</li><li>100doors.baby (DISPLAY)</li><li>test01.baby (IDENTIFICATION DIVISION)</li><li>test11.baby (lowercase structure)</li></ul> | 🚧📟📜 | DISPLAY DELIMITED BY parsed but not fully in codegen; COPY parsed no IR; IDENTIFICATION parsed not used in IR. |
| 2XP | Parsing of Arithmetic Statements: Support ADD, CALL, DIVIDE, MOVE, MULTIPLY, SUBTRACT, including multiple targets (e.g., ADD 1 TO X Y Z). | <ul><li>test26.baby (ADD)</li><li>test40.baby (ADD multiple)</li><li>test27.baby (DIVIDE)</li><li>fizzbuzz.baby (DIVIDE)</li><li>test01.baby (MOVE)</li><li>test02.baby (MOVE)</li><li>test06.baby (MOVE)</li><li>test11.baby (MOVE)</li><li>test12.baby (MOVE)</li><li>test16.baby (MOVE)</li><li>test22.baby (MOVE)</li><li>test24.baby (MOVE to arrays)</li><li>test32.baby (MOVE to arrays)</li><li>test36.baby (MOVE to SUBTRACT)</li><li>test38.baby (MOVE)</li><li>test41.baby (MOVE to arrays)</li><li>100doors.baby (MOVE)</li><li>test28.baby (MULTIPLY)</li><li>test36.baby (SUBTRACT)</li><li>100doors.baby (SUBTRACT)</li></ul> | ✅📟📜 |  |
| 3XP | Parsing of DATA DIVISION: Handle nesting with level numbers (e.g., 01 A. 03 B. 05 C.). Support references like MOVE 42 TO A OF B OF C. Reject malformed data entries (e.g., invalid level hierarchies or PICTURE strings). | <ul><li>test01.baby (DATA DIVISION with PICTURE)</li><li>test02.baby (PICTURE XX and 99)</li><li>test06.baby (PICTURE X(5))</li><li>test11.baby (PICTURE 99)</li><li>test12.baby (PICTURE XX and 99)</li><li>test16.baby (PICTURE X(5))</li><li>test22.baby (PICTURE S99)</li><li>test38.baby (PICTURE S99)</li></ul> | ✅📟📜 |  |
| 3XP | Parsing of EVALUATE and LOOP: All LOOP clauses (VARYING, FROM, TO, BY, WHILE, UNTIL) are detachable and can appear anywhere in the block; their effect depends on position (e.g., WHILE in body checks at that point). | <ul><li>test33.baby (EVALUATE)</li><li>test24.baby (LOOP VARYING)</li><li>test28.baby (LOOP WHILE)</li><li>test38.baby (LOOP UNTIL)</li><li>test41.baby (LOOP VARYING)</li><li>fizzbuzz.baby (LOOP VARYING)</li><li>99bottles.baby (LOOP VARYING)</li><li>100doors.baby (LOOP UNTIL)</li></ul> | 🚧📟📜 | EVALUATE in IR; LOOP clauses parsed/basic IR but dynamic positioning not fully in code. |
| 4XP | Position-Based Parsing and Line Continuations: | <ul><li>test03.baby (line continuations)</li><li>test08F.baby (invalid continuation)</li><li>test13.baby (invalid continuation)</li><li>test18.baby (valid continuation)</li><li>test23.baby (multi-line continuation)</li><li>test09.baby (position rules)</li><li>test10.baby (valid indentation)</li><li>test19.baby (invalid indentation)</li><li>test20.baby (valid indentation)</li><li>test07F.baby (invalid indicator)</li><li>test17.baby (valid comment indicator)</li></ul> | ✅📟📜 |  |
| 4XP | Sentences and Statements, NEXT SENTENCE, STOP: Sentences are statement lists ended by dot (.). Support NEXT SENTENCE for jumping to next sentence. Valid in last sentence of paragraph (continues to next paragraph or terminates if none). STOP terminates execution. | <ul><li>test25.baby (NEXT SENTENCE)</li><li>test35.baby (NEXT SENTENCE)</li><li>99bottles.baby (NEXT SENTENCE)</li><li>test11.baby (STOP)</li><li>test27.baby (STOP RUN)</li><li>test28.baby (STOP)</li><li>test29.baby (STOP)</li><li>test30.baby (STOP)</li><li>test34.baby (STOP)</li><li>test35.baby (STOP)</li><li>99bottles.baby (STOP)</li></ul> | 🚧📟📜 | Sentences/STOP in IR; NEXT SENTENCE parsed but complex control flow not implemented. |
| 5XP | Case Insensitivity with Exceptions: Keywords, symbols, identifiers are case-insensitive. Exceptions: quoted strings, PICTURE strings, comment lines, IDENTIFICATION DIVISION values are case-sensitive. In ambiguity, uppercase resolves to keywords. | <ul><li>test21.baby (mixed case)</li><li>test01.baby (mixed case keywords)</li><li>test02.baby (case insensitive identifiers)</li><li>test11.baby (all lowercase)</li><li>test12.baby (mixed case variables)</li><li>test04.baby (comments case-sensitive)</li><li>test14.baby (comments mixed case)</li></ul> | 🚧📟📜 | PICTURE made insensitive (deviation); ambiguity resolution not fully implemented. |
| 5XP | Whitespace Insignificance: Whitespace (space, tab, newline, CR/LF) ignored in code columns except in quoted strings, comments, IDENTIFICATION values. Can break keywords/identifiers (e.g., "AC COUNT" parses as "ACCOUNT"). In ambiguity, use original whitespace for resolution. |  | 🚧📟 | Basic handling; advanced breaking/ambiguity not implemented. |

## Checkpoint Data

### Symbol Table and Data Handling

| XP  | Description                                                                 | Test Files | Score | Explanation |
|:----|:----------------------------------------------------------------------------|:-----------|:------|:------------|
| 1XP | Symbol Table for Types and Fields: Implement symbol table to track types and fields across divisions. | <ul><li>test01.baby (basic types)</li><li>test02.baby (multiple fields)</li><li>test05.baby (keywords as fields)</li><li>test15.baby (keywords as fields)</li></ul> | ✅📟📜 |  |
| 1XP | IDENTIFICATION DIVISION Storage: Store key-value pairs (e.g., PROGRAM-ID) as metadata; inaccessible from executable code but testable. | <ul><li>test01.baby (PROGRAM-ID)</li><li>test03.baby (PROGRAM-ID)</li><li>test21.baby (PROGRAM-ID)</li></ul> | 🚫📟 | Parsed but not stored/processed. |
| 2XP | DATA DIVISION Implementation: Fully support PICTURE patterns (9, A, X, Z, S, V). Demonstrate with ACCEPT/DISPLAY. MOVE and others must preserve PICTURE consistency. Support full qualification (e.g., A OF B OF C). Architect for future statements. | <ul><li>test01.baby (PICTURE 99)</li><li>test02.baby (PICTURE XX and 99)</li><li>test06.baby (PICTURE X(5))</li><li>test11.baby (PICTURE 99)</li><li>test12.baby (PICTURE XX and 99)</li><li>test16.baby (PICTURE X(5))</li><li>test22.baby (S99)</li><li>test38.baby (S99)</li></ul> | ✅📟📜 |  |
| 2XP | LIKE Clauses: Inherit type (PICTURE or structure) but not values or OCCURS cardinality. | <ul><li>test32.baby (LIKE for arrays)</li></ul> | ✅📟📜 |  |
| 3XP | OCCURS Clauses: Support arrays/lists for fields/records. | <ul><li>test24.baby (OCCURS 3 TIMES)</li><li>test32.baby (OCCURS 2 TIMES)</li><li>test41.baby (OCCURS 3 TIMES)</li><li>100doors.baby (OCCURS 100 TIMES)</li></ul> | ✅📟📜 |  |
| 3XP | Figurative Constants in MOVE: Support SPACES (default value, e.g., 0 for 9, space for A/X/Z, . for V), HIGH-VALUES (max, e.g., 9 for 9/Z, z for A, #255 for X, + for S), LOW-VALUES (min, e.g., 0 for 9, space for A/Z, #0 for X, - for S). MOVE SPACES TO A B C adapts per field type. | <ul><li>test22.baby (HIGH-VALUES)</li><li>test40.baby (LOW-VALUES)</li><li>100doors.baby (SPACES)</li><li>test32.baby (SPACES)</li></ul> | 🚧📟📜 | HIGH/LOW-VALUES implemented type-adaptive; SPACES parsed but type-adaptive not in IR. |
| 4XP | Semantic Parsing and Defaults: | <ul><li>test26.baby (ADD defaults)</li><li>test27.baby (DIVIDE defaults)</li><li>test28.baby (MULTIPLY defaults)</li><li>test36.baby (SUBTRACT defaults)</li><li>test40.baby (ADD with LOW-VALUES)</li></ul> | ✅📟📜 |  |
| 4XP | MOVE Implementation: Support partial assignment for records (corresponding fields by name, others untouched). E.g., with 01 A. 03 B. 05 C. 07 D. 05 E. 07 D. 07 F.; MOVE E TO C assigns only D, leaves F. Name matching ignores sufficient qualification; MOVE E TO B does nothing if no match. | <ul><li>test01.baby (basic MOVE)</li><li>test02.baby (MOVE to mixed)</li><li>test06.baby (MOVE)</li><li>test11.baby (MOVE)</li><li>test12.baby (MOVE)</li><li>test16.baby (MOVE)</li><li>test22.baby (MOVE HIGH-VALUES)</li><li>test24.baby (MOVE to arrays)</li><li>test26.baby (MOVE for ADD)</li><li>test27.baby (MOVE for DIVIDE)</li><li>test28.baby (MOVE for LOOP)</li><li>test32.baby (MOVE SPACES)</li><li>test36.baby (MOVE for SUBTRACT)</li><li>test38.baby (MOVE to signed)</li><li>test40.baby (MOVE LOW-VALUES)</li><li>test41.baby (MOVE to arrays)</li><li>100doors.baby (MOVE SPACES/HIGH-VALUES)</li></ul> | 🚫 |  |
| 5XP | Non-Reserved Keywords: Keywords usable as field names. In ambiguity, uppercase resolves to keywords. Examples for ADD A TO B ADD C TO D ADD E TO F. with variants showing parses (e.g., lowercase "to" as identifier part). Ambiguous cases (e.g., mixed case with multiple parses) yield errors. | <ul><li>test05.baby (keywords as variables)</li><li>test15.baby (END as variable)</li></ul> | 🚧📟 | Keywords as identifiers parsed; ambiguity resolution not full. |
| 5XP | Contracted Expressions: In IF/EVALUATE/WHEN: Contract conditions (e.g., IF X = 10 OR 20 THEN... ≡ IF X = 10 OR X = 20 THEN...; IF X > 10 AND < 20 OR 100 AND > 80 THEN...). | <ul><li>test25.baby (contracted IF)</li><li>test33.baby (contracted in EVALUATE)</li></ul> | ✅📟📜 |  |

## Checkpoint Flow

### Execution and Control Flow

| XP  | Description                                                                 | Test Files | Score | Explanation |
|:----|:----------------------------------------------------------------------------|:-----------|:------|:------------|
| 1XP | Program Execution: Support full execution of BabyCobol programs. | <ul><li>test_display.baby (simple execution)</li><li>test01.baby (execution with MOVE)</li><li>test06.baby (execution with DISPLAY)</li><li>test11.baby (execution with STOP RUN)</li><li>test16.baby (execution)</li><li>fizzbuzz.baby (full program)</li><li>99bottles.baby (full program)</li><li>100doors.baby (full program)</li></ul> | ✅📟📜 |  |
| 1XP | COPY Implementation: Support nested file inclusion with REPLACING; arguments/even statement can depend on REPLACING. |  | 🚫📟 | Parsed but no file inclusion/replacement. |
| 2XP | PICTURE with Statements: DISPLAY respects PICTURE, supports NO ADVANCING/DELIMITED BY. ACCEPT interprets input per PICTURE. MOVE/others preserve consistency. | <ul><li>test31.baby (ACCEPT/DISPLAY DELIMITED)</li><li>test39.baby (DELIMITED BY SIZE)</li><li>test06.baby (DISPLAY with PICTURE)</li><li>test16.baby (DISPLAY with PICTURE)</li><li>test23.baby (DISPLAY NO ADVANCING)</li><li>fizzbuzz.baby (DISPLAY with PICTURE)</li><li>99bottles.baby (DISPLAY with PICTURE)</li><li>100doors.baby (DISPLAY with PICTURE)</li></ul> | 🚧📟📜 | DISPLAY/ACCEPT partial PICTURE in IR; NO ADVANCING implemented; DELIMITED BY parsed not functional. |
| 2XP | CALL Basic: Call other programs/paragraphs; respect signatures, report mismatches. |  | 🚫📟📜 | Parsed/sem checked; no LLVM call/arg IR. |
| 2XP | GO TO Basic: Static label; terminates active LOOPs; if out of PERFORM THROUGH, acts as redirected return (no resume). See PERFORM-GO TO example. | <ul><li>test29.baby (GO TO in PERFORM)</li><li>test30.baby (GO TO with ALTER)</li><li>99bottles.baby (GO TO in altered paths)</li></ul> | 🚧📟📜 | Basic static in IR; no complex LOOP/PERFORM interactions. |
| 3XP | ALTER: Alter single-statement GO TO paragraphs (e.g., ALTER X TO PROCEED TO Z changes GO TO Y in X to Z). Permanent until re-altered; target must exist; non-single GO TO invalid. | <ul><li>test30.baby (ALTER GO TO)</li><li>99bottles.baby (ALTER in loop)</li></ul> | ✅📟📜 |  |
| 3XP | Computable GO TO: GO TO <field>; runtime value as label. Error if no matching paragraph. ALTER makes it static. |  | 🚫 |  |
| 3XP | PERFORM (THROUGH): Call paragraph/range with return; support TIMES. THROUGH executes from start to end label. | <ul><li>test29.baby (PERFORM TIMES)</li><li>test35.baby (PERFORM THROUGH)</li><li>99bottles.baby (PERFORM THROUGH)</li></ul> | 🚧📟📜 | PERFORM paragraph/TIMES in IR; THROUGH parsed not implemented. |
| 4XP | LOOP Full: Defaults: FROM/BY=1, TO=type max (HIGH-VALUES). Clauses detachable/scattered; multiple same-kind (e.g., multiple WHILE). | <ul><li>test28.baby (LOOP WHILE)</li><li>test38.baby (LOOP BY UNTIL)</li><li>test24.baby (LOOP VARYING)</li><li>test41.baby (LOOP VARYING)</li><li>fizzbuzz.baby (LOOP VARYING)</li><li>99bottles.baby (LOOP VARYING)</li><li>100doors.baby (LOOP UNTIL)</li></ul> | 🚧📟📜 | Defaults/basic in IR; full dynamic/scattered not implemented. |
| 4XP | SIGNAL: SIGNAL X ON ERROR sets global handler; on error, GO TO X (terminates LOOP/PERFORM, no return). Another error in handler terminates normally. SIGNAL OFF ON ERROR resumes default. Ambiguous (e.g., paragraph OFF) errors. | <ul><li>test34.baby (SIGNAL ON ERROR)</li></ul> | 🚫📟 | Parsed but no functionality in IR. |
| 5XP | Sufficient Qualification: Refer fields without full path (e.g., D OF C sufficient if unambiguous). Works in all statements/LIKE. Ambiguous: compilation error. Examples with nested A.B.C.D, A.B.E.D.F showing valid/invalid refs. | <ul><li>test32.baby (LIKE with qualification)</li></ul> | ✅📟📜 |  |

## Cross-Checkpoint Interactions

### Mixed Features

| XP  | Description                                                                 | Test Files | Score | Explanation |
|:----|:----------------------------------------------------------------------------|:-----------|:------|:------------|
| 1XP | NEXT SENTENCE Advanced: Exits IF/LOOP cleanly. Valid in last sentence (next paragraph or terminate). Does not force next if already planned (e.g., end of PERFORM). | <ul><li>test25.baby (NEXT SENTENCE)</li><li>test35.baby (NEXT SENTENCE)</li><li>99bottles.baby (NEXT SENTENCE)</li></ul> | 🚫📟 | Parsed but not in IR. |
| 1XP | COPY Nested/Duplicates: Support nested COPY; reject duplicates (paragraphs/fields) from COPY. |  | 🚫📟 | No COPY implementation. |
| 2XP | EVALUATE ALSO: Match ALSO counts in EVALUATE/WHEN (except OTHER). Spaces separate expressions in WHEN. | <ul><li>test33.baby (EVALUATE ALSO)</li></ul> | 🚧📟📜 | Parsed/sem checked counts; complex multiple ALSO not fully in IR. |
| 2XP | WHEN THROUGH: Equivalent to multiple WHEN expressions; ensure unambiguous parsing. | <ul><li>test33.baby (WHEN THROUGH)</li></ul> | 🚫📟 | Parsed but not in IR. |
| 2XP | Arithmetic with Records: ADD/SUBTRACT/DIVIDE/MULTIPLY support records (corresponding numeric fields, like MOVE CORR). |  | 🚫 |  |
| 2XP | Dangling ELSE: Detect/report dangling ELSE (unmatched). | <ul><li>test37.baby (nested ELSE)</li><li>test15.baby (nested IF ELSE)</li></ul> | 🚫 | Not handled by sem checks. |
| 3XP | PICTURE Arithmetics, Overflows, Conversions: Overflows decimal (not binary); numeric left, string right. Essentially numeric PICTURE (no A/X) supports arithmetics (e.g., $99 + SZ9%). | <ul><li>test26.baby (ADD)</li><li>test27.baby (DIVIDE)</li><li>test28.baby (MULTIPLY)</li><li>test36.baby (SUBTRACT)</li><li>test40.baby (ADD with LOW-VALUES)</li><li>fizzbuzz.baby (DIVIDE remainders)</li><li>100doors.baby (SUBTRACT)</li></ul> | 🚫 | Basic int arith; no advanced handling. |
| 3XP | CALL BY Clauses: BY REFERENCE: modify propagates; BY CONTENT: read-only; BY VALUE: local mods only. AS PRIMITIVE/STRUCT optional for cross-lang. |  | 🚫📟 | Parsed but not in IR. |
| 3XP | CALL Paragraphs of Other Programs: CALL FunctionName OF ProgramName to specific paragraphs. |  | 🚫 | Not parsed/implemented. |
| 3XP | PERFORM THROUGH + GO TO: If GO TO exits THROUGH range, continues outside (no return). See example program (prints "ABBCDECDEF"). | <ul><li>test29.baby (PERFORM with GO TO)</li><li>test30.baby (GO TO with PERFORM)</li><li>99bottles.baby (PERFORM with GO TO)</li></ul> | 🚫 | PERFORM THROUGH not implemented. |
| 3XP | LOOP Multi-Clauses and PICTURE Defaults: Multiple clauses; defaults per PICTURE (e.g., TO=HIGH-VALUES). | <ul><li>test38.baby (multiple clauses)</li><li>test24.baby (VARYING defaults)</li><li>test28.baby (WHILE)</li><li>test41.baby (VARYING)</li><li>fizzbuzz.baby (VARYING)</li><li>99bottles.baby (VARYING)</li><li>100doors.baby (UNTIL)</li></ul> | 🚧📟📜 | Multiple parsed/basic; PICTURE-based defaults not. |
| 3XP | CALL Error Handling with USING Clashes: Enforce BY semantics; read-only for CONTENT; one-way for VALUE. |  | 🚫 | Not implemented. 
