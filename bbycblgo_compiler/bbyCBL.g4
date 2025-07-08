grammar bbyCBL;

/* =====================
   Parser rules
   ===================== */

program
    : identificationDivision dataDivision? procedureDivision? EOF  // make iden = mandatory. procedure+data are optional
    ;

/* IDENTIFICATION DIVISION and its clauses */
identificationDivision
    : IDENTIFICATION_DIVISION identificationClause*
    ;
identificationClause
    : PROGRAM_ID   DOT (identifier | DATE | DATE_UNDERSCORE | ANY_DATE | ANY_DATE_UNDERSCORE) DOT
    | AUTHOR       DOT identifier    DOT            
    | DATE_WRITTEN DOT (DATE | DATE_UNDERSCORE | identifier) DOT         
    | INSTALLATION DOT identifier    DOT                  
    | SECURITY     DOT identifier    DOT                  
    | DATE_COMPILED DOT (DATE | identifier) DOT          
    | BASE         DOT identifier    DOT                  
    | COPY         copySource (REPLACING replacePair+)? DOT?
    | DESCRIPTION  DOT identifier    DOT              // simple identifier for DESCRIPTION
    | simpleId DOT (DATE | DATE_UNDERSCORE | identifier)? DOT   // NEW – covers "ELSE." "LIKE." …
    ;

/* DATA DIVISION */
dataDivision
    : DATA_DIVISION (dataEntry | dataCopyStmt)*
    ;
dataEntry
    : levelNumber identifier (pictureClause | likeClause)? occursClause? DOT
    ;
dataCopyStmt
    : COPY copySource (REPLACING replacePair+)? DOT
    ;
levelNumber
    : NUMBER
    ;
pictureClause
    : PICTURE IS picturePattern
    ;

// now parse picture‐patterns purely in the parser
picturePattern
    : pictureElement+
    ;
pictureElement
    : (NUMBER
      | IDENTIFIER
      | PROGRAM_ID | AUTHOR | DATE_WRITTEN | STOP
      | DATE | DATE_UNDERSCORE | ANY_DATE | ANY_DATE_UNDERSCORE
      | MINUS | PLUS    
      ) (LPAREN NUMBER RPAREN)?   // old form
    | LPAREN (NUMBER | IDENTIFIER) RPAREN   // 9(I) as in test 195684
    ;

likeClause
    : LIKE identifierSegment (OF identifierSegment)* // 0‑N "OF …"
    ;
occursClause
    : OCCURS NUMBER (TIMES)?                           // support optional TIMES
    ;

/* PROCEDURE DIVISION */
procedureDivision
    : PROCEDURE_DIVISION (USING usingClause)? DOT (paragraph | sentence)*  // Keep DOT after USING
    ;
usingClause
    : (BY (REFERENCE | VALUE | CONTENT) expr)+         // Changed IDENTIFIER to expr
    ;
paragraph
    : identifier DOT sentence*                          // use identifier to allow keywords
    ;
sentence
    : statement+ DOT DOT*                              // allow consecutive dots
    ;
statement
    : acceptStmt
    | addStmt
    | subtractStmt
    | multiplyStmt
    | divideStmt
    | moveStmt
    | displayStmt
    | evaluateStmt
    | ifStmt
    | loopStmt
    | performStmt
    | nextSentenceStmt
    | gotoStmt
    | callStmt
    | copyStmt
    | signalStmt
    | stopStmt
    | alterStmt                                       // add ALTER support
    | varyingStmt                                     // support standalone VARYING
    | BASE                                           // Add BASE to allow it as identifier
    ;
identifier
    : simpleId (OF simpleId)+        # qualified
    | simpleId                       # single
    ;


simpleId
    : IDENTIFIER
    | DESCRIPTION            
    | PROGRAM_ID | AUTHOR | DATE_WRITTEN | DATE | DATE_UNDERSCORE
    | ANY_DATE | ANY_DATE_UNDERSCORE
    | HIGH_VALUES | LOW_VALUES | SPACE | SPACES
    | BASE | TRUE | FALSE | END | ADD | MOVE | TO | THEN | ELSE
    | INSTALLATION | SECURITY | DATE_COMPILED
    | DISPLAY | OF | BY | COPY | VARYING | STOP | LIKE
    ;


evalSubject
    : exprList
    | condition
    ;

/* Statements */
/* Expressions and conditions */
acceptStmt       : ACCEPT exprList?                                        ;
addStmt          : ADD exprList TO exprList givingClause*                 ; // support multiple GIVING
alterStmt        : ALTER identifier TO PROCEED TO identifier             ; // use identifier instead of IDENTIFIER
callStmt         : CALL expr (USING usingClause)?                           ;
copySource       
    : STRING
    | simpleId                ; // now covers DISPLAY, COPY, etc.
copyStmt         : COPY copySource (REPLACING replacePair+)? identifier?    ;
replacePair      : replaceBlock BY replaceBlock                          ;   // ===FOO=== BY ===BAR===
replaceBlock     : equalDelim ( . )+? equalDelim                     ;   // wildcard tokens until next "===" or "=="

equalDelim
    : TRIPLE_EQUAL
    | DOUBLE_EQUAL
    ;
displayItem      : expr delimiterSpec?                                   ;
delimiterSpec    
    : DELIMITED_BY valueSpec
    | DELIMITED BY valueSpec                                             ;
valueSpec        : expr | SIZE | SPACE                                   ;
displayStmt      : DISPLAY displayItem+ // 1‑N items
                    (WITH_NO_ADVANCING | WITH NO ADVANCING)? // optional flag
                    displayItem* // more items allowed;
;
divideStmt       : DIVIDE exprList INTO exprList givingClause* (REMAINDER exprList)?                  ; // support multiple GIVING
evaluateStmt     : EVALUATE evalSubject? (ALSO evalSubject)* whenClause+ (END_EVALUATE | END identifier?) ;
givingClause     : GIVING exprList                                       ; // separate GIVING clause
gotoStmt         : GO TO exprList                                        ;
ifStmt           : IF condition THEN statement+ (ELSE statement+)? (END_IF | END)? ;
loopStmt         : LOOP (loopControl | statement)* END identifier?  ;  // fully inter-leaved with optional identifier

loopControl
    : varyingClause
    | whileClause
    | untilClause
    ;
moveStmt         : MOVE exprList TO exprList                             ;
multiplyStmt     : MULTIPLY exprList BY exprList givingClause*            ; // support multiple GIVING
nextSentenceStmt : NEXT_SENTENCE identifier? ;
performStmt      : PERFORM expr (THROUGH expr)? (expr TIMES)?   | PERFORM exprList TIMES   ;   // THROUGH … [n] TIMES
signalStmt
    : SIGNAL OFF ON ERROR identifier?               # signalDisable
    | SIGNAL expr (ON ERROR identifier?)?           # signalEnable
    ;
stopStmt         : STOP (RUN | identifier)?                              ;   // RUN   | paragraph‑name
subtractStmt     : SUBTRACT exprList FROM exprList givingClause*         ;   // support multiple GIVING
untilClause      : UNTIL condition                                       ;
varyingClause
    : VARYING (qualifiedId | identifierSegment)?
      (FROM expr)? (TO expr)? (BY expr)? ;


varyingStmt      
    : varyingClause                     // full form
    | VARYING                          // single keyword (old tests)
    ;
whenClause
    : WHEN OTHER statement+                                              # whenOther
    | WHEN evalSubject (THROUGH evalSubject)?
      (ALSO evalSubject (THROUGH evalSubject)?)* ALSO?                   // dangling ALSO ok
      statement+                                                         # whenValues
    ;
whileClause      : WHILE condition                                       ;
exprList
    : expr (COMMA? expr)*
    ;
condition
    : simpleCond ((AND | OR | XOR) simpleCond)*                               // support AND/OR chains
    ;
simpleCond
    : LPAREN condition RPAREN                          // ( …boolean… )
    | NOT? expr comparator expr                        // support NOT prefix
    | NOT? comparator expr                             // allow "IF = 2" shorthand with NOT
    | NOT? expr                                        // support NOT with single expression
    | expr
    ;
comparator
    : EQ | NE | LT | LE | GT | GE
    ;
identifierSegment
    : simpleId (LPAREN expr RPAREN)*                    ;   // VAR(10)
qualifiedId
    : identifierSegment (OF identifierSegment)+         ;
expr
    : PLUS expr                # UPlus
    | MINUS expr               # UMinus
    | expr POW expr            # Exp
    | expr (MULT|DIV) expr     # MulDiv
    | expr (PLUS|MINUS) expr   # AddSub
    | LPAREN expr RPAREN       # Parens
    | literal                  # LitExpr
    | qualifiedId              # QualifiedIdExpr
    | identifierSegment        # IdExpr
    ;
literal
    : FLOAT
    | NUMBER
    | STRING
    | DATE | DATE_UNDERSCORE | ANY_DATE | ANY_DATE_UNDERSCORE
    ;

/* =====================
   Lexer rules
   ===================== */

/* Floating-point literal */
FLOAT
    : [0-9]+ '.' [0-9]+                            // match numbers like 4.5, 12.121
    ;

/* Integer literal */
NUMBER
    : [0-9]+
    ;

/* DATE tokens (YYYY-MM-DD and YYYY_MM_DD) */
DATE
    : [0-9][0-9][0-9][0-9] '-' [0-9][0-9] '-' [0-9][0-9]
    ;
DATE_UNDERSCORE
    : [0-9][0-9][0-9][0-9] '_' [0-9][0-9] '_' [0-9][0-9]
    ;

/* Any Date pattern for identifiers */
ANY_DATE
    : [0-9][0-9] '-' [0-9][0-9] '-' [0-9][0-9][0-9][0-9]
    ;
ANY_DATE_UNDERSCORE
    : [0-9][0-9] '_' [0-9][0-9] '_' [0-9][0-9][0-9][0-9]
    ;

/* Division headers */
IDENTIFICATION_DIVISION
    : 'IDENTIFICATION' WS+ 'DIVISION' WS* DOT
    ;
DATA_DIVISION
    : 'DATA' WS+ 'DIVISION' WS* DOT
    ;
PROCEDURE_DIVISION
    : 'PROCEDURE' WS+ 'DIVISION'                      // No DOT here - it comes in parser
    ;

/* Identification clauses */
PROGRAM_ID
    : 'PROGRAM-ID' | 'PROGRAMID'
    ;
AUTHOR
    : 'AUTHOR'
    ;
DATE_WRITTEN
    : 'DATE-WRITTEN' | 'DATA-WRITTEN' | 'DATE_WRITTEN'  // support underscore variant
    ;
INSTALLATION
    : 'INSTALLATION'
    ;
SECURITY
    : 'SECURITY'
    ;
DATE_COMPILED
    : 'DATE-COMPILED'
    ;
BASE
    : 'BASE'
    ;
COPY
    : 'COPY'
    ;
DESCRIPTION
    : 'DESCRIPTION'
    ;

/* Data clauses */
PICTURE
    : 'PICTURE'
    ;
IS
    : 'IS'
    ;
LIKE
    : 'LIKE'
    ;
OCCURS
    : 'OCCURS'
    ;

/* Statement keywords & clauses */

ACCEPT            : 'ACCEPT' ;
ADD               : 'ADD' ;
ALSO              : 'ALSO' ;
ALTER             : 'ALTER' ;                        // add ALTER keyword
AND               : 'AND' ;
BY                : 'BY' ;
CALL              : 'CALL' ;
CONTENT           : 'CONTENT' ;                      // add CONTENT keyword
DELIMITED_BY      : 'DELIMITED' WS+ 'BY' ;
DELIMITED         : 'DELIMITED' ;
DISPLAY           : 'DISPLAY' ;
DIVIDE            : 'DIVIDE' ;
ELSE              : 'ELSE' ;
END               : 'END' ;
OFF               : 'OFF' ;                          // add OFF keyword
END_EVALUATE      : 'END-EVALUATE' ;
END_IF            : 'END-IF' ;
ERROR             : 'ERROR' ;                        // add ERROR keyword
EVALUATE          : 'EVALUATE' ;
FALSE             : 'FALSE' ;
FROM              : 'FROM' ;
GIVING            : 'GIVING' ;
GO                : 'GO' ;
IF                : 'IF' ;
INTO              : 'INTO' ;
LOOP              : 'LOOP' ;
LOW_VALUES        : 'LOW-VALUES' ;
MOVE              : 'MOVE' ;
MULTIPLY          : 'MULTIPLY' ;
NEXT_SENTENCE     : 'NEXT' WS+ 'SENTENCE' ;
NOT               : 'NOT' ;                          // add NOT keyword
OF                : 'OF' ;                           // add OF keyword for nested structure references
ON                : 'ON' ;                           // add ON keyword
OR                : 'OR' ;
OTHER             : 'OTHER' ;                        // add OTHER keyword (for WHEN OTHER)
PERFORM           : 'PERFORM' ;
PROCEED           : 'PROCEED' ;                      // add PROCEED keyword
REFERENCE         : 'REFERENCE' ;                    // add REFERENCE keyword
REMAINDER         : 'REMAINDER' ;
REPLACING         : 'REPLACING' ;                    // add REPLACING keyword
RUN               : 'RUN' ;
SIGNAL            : 'SIGNAL' ;
SIZE              : 'SIZE' ;
SPACE             : 'SPACE' ;
SPACES            : 'SPACES' ;                       // for move statements
STOP              : 'STOP' ;
SUBTRACT          : 'SUBTRACT' ;
THEN              : 'THEN' ;
THROUGH           : 'THROUGH' | 'THRU' ;
TIMES             : 'TIMES' ;
TO                : 'TO' ;
TRUE              : 'TRUE' ;
UNTIL             : 'UNTIL' ;
USING             : 'USING' ;
VALUE             : 'VALUE' ;                        // add VALUE keyword
VARYING           : 'VARYING' ;
WHEN              : 'WHEN' ;
WHILE             : 'WHILE' ;
WITH_NO_ADVANCING : 'WITH' WS+ 'NO' WS+ 'ADVANCING' ;
WITH              : 'WITH' ;
NO                : 'NO' ;
ADVANCING         : 'ADVANCING' ;
XOR               : 'XOR' ;                          // add XOR keyword  
/* Operators & punctuation */
COMMA         : ',' ;
DOT           : '.' ;
LPAREN        : '(' ;
RPAREN        : ')' ;
PLUS          : '+' ;
MINUS         : '-' ;
MULT          : '*' ;                                     // separate from **
DIV           : '/' ;
POW           : '**' ;                                    // add exponentiation operator
EQ            : '=' ;
NE            : '<>' ;
LE            : '<=' ;
GE            : '>=' ;
LT            : '<' ;
GT            : '>' ;
TRIPLE_EQUAL  : '===' ;                                   // add TRIPLE_EQUAL for COPY REPLACING
DOUBLE_EQUAL  : '==' ;                                    // add DOUBLE_EQUAL for COPY REPLACING

/* Identifiers & literals */
STRING
    : '"' (~["\\] | '\\' .)* '"'
    ;
HIGH_VALUES
    : 'HIGH-VALUES'
    ;
IDENTIFIER
    : [A-Za-z_$][A-Za-z0-9_$-]*             
    | [0-9]+ [A-Za-z_$-] [A-Za-z0-9_$-]*     // 9V9, 77-NAME, etc.
    ;

/* Whitespace */
WS
    : [ \t\r\n]+ -> skip
    ;
