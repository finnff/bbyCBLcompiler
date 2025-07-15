grammar bbyCBL;

/* =====================
   Parser rules
   ===================== */

program
    : identificationDivision dataDivision? procedureDivision? EOF
    ;

/* IDENTIFICATION DIVISION and its clauses */
identificationDivision
    : IDENTIFICATION_DIVISION identificationClause*
    ;
identificationClause
    : PROGRAM_ID   DOT (identifier | DATE | DATE_UNDERSCORE | ANY_DATE | ANY_DATE_UNDERSCORE) DOT
    | AUTHOR       DOT freeFormText DOT
    | DATE_WRITTEN DOT (DATE | DATE_UNDERSCORE | freeFormText) DOT
    | INSTALLATION DOT freeFormText DOT
    | SECURITY     DOT freeFormText DOT
    | DATE_COMPILED DOT (DATE | freeFormText) DOT
    | BASE         DOT freeFormText DOT
    | COPY         copySource (REPLACING replacePair+)? DOT?
    | DESCRIPTION  DOT freeFormText DOT
    | simpleId DOT freeFormText? DOT
    ;

freeFormText
    : (simpleId | STRING | NUMBER | FLOAT | literal | DATE | DATE_UNDERSCORE | ANY_DATE | ANY_DATE_UNDERSCORE)+
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
    : PICTURE (IS)? picturePattern
    ;

picturePattern
    : pictureElement+
    ;
pictureElement
    : (NUMBER
      | IDENTIFIER
      | PROGRAM_ID | AUTHOR | DATE_WRITTEN | STOP
      | DATE | DATE_UNDERSCORE | ANY_DATE | ANY_DATE_UNDERSCORE
      | MINUS | PLUS
      ) (LPAREN NUMBER RPAREN)?
    | LPAREN (NUMBER | IDENTIFIER) RPAREN
    ;

likeClause
    : LIKE identifierSegment (OF identifierSegment)*
    ;
occursClause
    : OCCURS NUMBER (TIMES)?
    ;

/* PROCEDURE DIVISION */
procedureDivision
    : PROCEDURE_DIVISION (USING usingClause)? DOT (paragraph | sentence)*
    ;
usingClause
    : (BY (REFERENCE | VALUE | CONTENT) expr)+
    ;
paragraph
    : identifier DOT sentence*
    ;

sentence
    : statement+ DOT DOT*
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
    | alterStmt
    | varyingStmt
    | BASE
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
    | OFF | ON | ERROR | PROCEED | REFERENCE | VALUE | CONTENT
    | OTHER | NOT | OR | AND | XOR | TIMES | USING | WHILE | UNTIL
    | INTO | THROUGH | GO | IF | LOOP | MULTIPLY
    | DIVIDE | SUBTRACT | CALL | ACCEPT | PERFORM | SIGNAL | ALTER
    | EVALUATE | WHEN | RUN | SIZE | DELIMITED | WITH
    | NO | ADVANCING | ALSO | NEXT | SENTENCE
    ;

evalSubject
    : exprList
    | condition
    ;

/* Statements */
acceptStmt
    : ACCEPT exprList
    ;
addStmt 
    : ADD exprList TO exprList givingClause*     # addToForm
    | ADD exprList givingClause                  # addGivingForm  
    ;
alterStmt        : ALTER identifier TO PROCEED TO identifier             ;
callStmt         : CALL expr (USING usingClause)?                           ;
copySource
    : STRING
    | simpleId
    ;
copyStmt         : COPY copySource (REPLACING replacePair+)? identifier?    ;
replacePair      : replaceBlock BY replaceBlock                          ;
replaceBlock     : equalDelim ( . )+? equalDelim                     ;

equalDelim
    : TRIPLE_EQUAL
    | DOUBLE_EQUAL
    ;
displayItem      : expr delimiterSpec?                                   ;
delimiterSpec
    : DELIMITED_BY valueSpec
    | DELIMITED BY valueSpec
    ;
valueSpec        : expr | SIZE | SPACE                                   ;
displayStmt      : DISPLAY displayItem+ withNoAdvancingClause? displayItem* ;

withNoAdvancingClause
    : WITH NO ADVANCING
    | WITH_NO_ADVANCING
    ;
divideStmt
    : DIVIDE exprList INTO exprList givingClause* (REMAINDER exprList)?    # divideIntoForm
    | DIVIDE exprList BY exprList givingClause* (REMAINDER exprList)?      # divideByForm
    ;
evaluateStmt     : EVALUATE evalSubject? (ALSO evalSubject)* whenClause+ (END_EVALUATE | END identifier?) ;
givingClause     : GIVING exprList                                       ;
gotoStmt         : GO TO exprList                                        ;
ifStmt           : IF condition THEN statement+ (ELSE statement+)? (END_IF | END)? ;
loopStmt         : LOOP loopContent* END identifier?
    ;

loopContent
    : loopControl
    | statement
    | sentence
    ;

loopControl
    : varyingClause
    | whileClause
    | untilClause
    ;
moveStmt         : MOVE exprList TO exprList                             ;
multiplyStmt     : MULTIPLY exprList BY exprList givingClause*            ;
nextSentenceStmt 
    : NEXT_SENTENCE identifier? 
    | NEXT SENTENCE identifier?   
    ;
performStmt      : PERFORM expr (THROUGH expr)? (expr TIMES)?   | PERFORM exprList TIMES   ;
signalStmt
    : SIGNAL OFF ON ERROR identifier?               # signalDisable
    | SIGNAL simpleId ON ERROR identifier?          # signalEnable
    | SIGNAL expr (ON ERROR identifier?)?           # signalExpr
    ;
stopStmt         : STOP (RUN | identifier)?                              ;
subtractStmt     : SUBTRACT exprList FROM exprList givingClause*         ;
untilClause      : UNTIL condition                                       ;
varyingClause
    : VARYING (qualifiedId | identifierSegment)?
      (FROM expr)? (TO expr)? (BY expr)? ;

varyingStmt
    : varyingClause
    | VARYING
    ;
whenClause
    : WHEN OTHER statement+                                              # whenOther
    | WHEN evalSubject (THROUGH evalSubject)?
      (ALSO evalSubject (THROUGH evalSubject)?)* ALSO?
      statement+                                                         # whenValues
    ;
whileClause      : WHILE condition                                       ;
exprList
    : expr (COMMA? expr)*
    ;
condition
    : simpleCond ((AND | OR | XOR) simpleCond)*
    ;

simpleCond
    : LPAREN condition RPAREN
    | NOT? expr (comparator expr)?
    | NOT? comparator expr
    ;
comparator
    : EQ | NE | LT | LE | GT | GE
    ;
identifierSegment
    : simpleId (LPAREN expr RPAREN)*
    ;
qualifiedId
    : identifierSegment (OF identifierSegment)+
    ;
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

/* Case-insensitive fragments */
fragment A: [aA]; fragment B: [bB]; fragment C: [cC]; fragment D: [dD];
fragment E: [eE]; fragment F: [fF]; fragment G: [gG]; fragment H: [hH];
fragment I: [iI]; fragment J: [jJ]; fragment K: [kK]; fragment L: [lL];
fragment M: [mM]; fragment N: [nN]; fragment O: [oO]; fragment P: [pP];
fragment Q: [qQ]; fragment R: [rR]; fragment S: [sS]; fragment T: [tT];
fragment U: [uU]; fragment V: [vV]; fragment W: [wW]; fragment X: [xX];
fragment Y: [yY]; fragment Z: [zZ];

/* Floating-point literal */
FLOAT
    : [0-9]+ '.' [0-9]+
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
    : I D E N T I F I C A T I O N WS+ D I V I S I O N WS* DOT
    ;
DATA_DIVISION
    : D A T A WS+ D I V I S I O N WS* DOT
    ;
PROCEDURE_DIVISION
    : P R O C E D U R E WS+ D I V I S I O N
    ;

/* Identification clauses */
PROGRAM_ID
    : P R O G R A M '-' I D | P R O G R A M I D
    ;
AUTHOR
    : A U T H O R
    ;
DATE_WRITTEN
    : D A T E '-' W R I T T E N | D A T A '-' W R I T T E N | D A T E '_' W R I T T E N
    ;
INSTALLATION
    : I N S T A L L A T I O N
    ;
SECURITY
    : S E C U R I T Y
    ;
DATE_COMPILED
    : D A T E '-' C O M P I L E D
    ;
BASE
    : B A S E
    ;
COPY
    : C O P Y
    ;
DESCRIPTION
    : D E S C R I P T I O N
    ;

/* Data clauses */
PICTURE
    : P I C T U R E
    ;
IS
    : I S
    ;
LIKE
    : L I K E
    ;
OCCURS
    : O C C U R S
    ;

/* Statement keywords & clauses */

ACCEPT            : A C C E P T ;
ADD               : A D D ;
ALSO              : A L S O ;
ALTER             : A L T E R ;
AND               : A N D ;
BY                : B Y ;
CALL              : C A L L ;
CONTENT           : C O N T E N T ;
DELIMITED_BY      : D E L I M I T E D WS+ B Y ;
DELIMITED         : D E L I M I T E D ;
DISPLAY           : D I S P L A Y ;
DIVIDE            : D I V I D E ;
ELSE              : E L S E ;
END               : E N D ;
OFF               : O F F ;
END_EVALUATE      : E N D '-' E V A L U A T E ;
END_IF            : E N D '-' I F ;
ERROR             : E R R O R ;
EVALUATE          : E V A L U A T E ;
FALSE             : F A L S E ;
FROM              : F R O M ;
GIVING            : G I V I N G ;
GO                : G O ;
IF                : I F ;
INTO              : I N T O ;
LOOP              : L O O P ;
LOW_VALUES        : L O W '-' V A L U E S ;
MOVE              : M O V E ;
MULTIPLY          : M U L T I P L Y ;
NEXT_SENTENCE     : N E X T WS+ S E N T E N C E ;
NEXT              : N E X T;
SENTENCE          : S E N T E N C E;
NOT               : N O T ;
OF                : O F ;
ON                : O N ;
OR                : O R ;
OTHER             : O T H E R ;
PERFORM           : P E R F O R M ;
PROCEED           : P R O C E E D ;
REFERENCE         : R E F E R E N C E ;
REMAINDER         : R E M A I N D E R ;
REPLACING         : R E P L A C I N G ;
RUN               : R U N ;
SIGNAL            : S I G N A L ;
SIZE              : S I Z E ;
SPACE             : S P A C E ;
SPACES            : S P A C E S ;
STOP              : S T O P ;
SUBTRACT          : S U B T R A C T ;
THEN              : T H E N ;
THROUGH           : T H R O U G H | T H R U ;
TIMES             : T I M E S ;
TO                : T O ;
TRUE              : T R U E ;
UNTIL             : U N T I L ;
USING             : U S I N G ;
VALUE             : V A L U E ;
VARYING           : V A R Y I N G ;
WHEN              : W H E N ;
WHILE             : W H I L E ;
WITH_NO_ADVANCING : W I T H WS* N O WS* A D V A N C I N G ;
WITH              : W I T H ;
NO                : N O ;
ADVANCING         : A D V A N C I N G ;
XOR               : X O R ;

/* Operators & punctuation */
COMMA         : ',' ;
DOT           : '.' ;
LPAREN        : '(' ;
RPAREN        : ')' ;
PLUS          : '+' ;
MINUS         : '-' ;
MULT          : '*' ;
DIV           : '/' ;
POW           : '**' ;
EQ            : '=' ;
NE            : '<>' ;
LE            : '<=' ;
GE            : '>=' ;
LT            : '<' ;
GT            : '>' ;
TRIPLE_EQUAL  : '===' ;
DOUBLE_EQUAL  : '==' ;

/* Identifiers & literals */
STRING
    : '"' (~["\\] | '\\' .)* '"'
    ;
HIGH_VALUES
    : H I G H '-' V A L U E S
    ;
IDENTIFIER
    : [A-Za-z_$][A-Za-z0-9_$-]*
    | [0-9]+ [A-Za-z_$-] [A-Za-z0-9_$-]*
    ;

/* Whitespace */
WS
    : [ \t\r\n]+ -> skip
    ;
