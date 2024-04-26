lexer grammar CustomLanguageLexer;

// Tokens

DEV          : 'dev';
LANG         : 'lang';
IF           : 'if';
ELSE         : 'else';
LOOP         : 'loop';
LOOP_WITH    : 'loopwith';
LOOP_RANGE   : 'looprange';
INT          : 'int';
TOUT         : 'tout';
OTHERWISE    : 'otherwise';
AND          : 'and';
OR           : 'or';
NOT          : 'not';
LET          : 'let';
TRUE         : 'true';
FALSE        : 'false';
ASSIGN       : '=';
COMMA        : ',';
QUOTE          : '"';
OPEN_BRACE   : '{';
CLOSE_BRACE  : '}';
OPEN_PAREN   : '(';
CLOSE_PAREN  : ')';
OPEN_BRACKET : '[';
CLOSE_BRACKET: ']';
QUESTION     : '?';
COLON        : ':';
//newly added
LESS_THAN    : '<';
GREATER_THAN  : '>';
EQUAL_EQUAL  : '==';
CHARR        : 'charr';
ADDITION     : '+';
SUBTRACTION  : '-';
MULTIPLICATION : '*';
DIVISION      : '/';


// Identifiers and literals
IDENTIFIER   : [a-zA-Z]+;
INTEGER      : [0-9]+;
CHAR         : '\'' (~['\\])* '\'';
STRING       :  [a-zA-Z0-9]+;
BOOL         : 'bool';




// Whitespace and comments
WS           : [ \t\r\n]+ -> skip;
COMMENT      : '//' ~[\r\n]* -> skip;


// Token list separated by commas
TOKENLIST    : (DEV | LANG | IF | QUOTE | ELSE | LOOP | LOOP_WITH | LOOP_RANGE | INT | TOUT | OTHERWISE | AND | OR | NOT | LET | TRUE | FALSE | ASSIGN | OPEN_BRACE | CLOSE_BRACE | OPEN_PAREN | CLOSE_PAREN | OPEN_BRACKET | CLOSE_BRACKET | QUESTION | COLON | IDENTIFIER | INTEGER | CHAR | STRING | BOOL| LESS_THAN | GREATER_THAN | EQUAL_EQUAL | CHARR | ADDITION | SUBTRACTION | MULTIPLICATION | DIVISION) (COMMA ( | DEV | LANG | IF | ELSE | LOOP | LOOP_WITH | LOOP_RANGE | INT | TOUT | OTHERWISE | AND | OR | NOT | LET | TRUE | FALSE | ASSIGN | OPEN_BRACE | CLOSE_BRACE | OPEN_PAREN | CLOSE_PAREN | OPEN_BRACKET | CLOSE_BRACKET | QUESTION | COLON | IDENTIFIER | INTEGER | CHAR | STRING | BOOL | LESS_THAN | GREATER_THAN | EQUAL_EQUAL | CHARR | ADDITION | SUBTRACTION | MULTIPLICATION | DIVISION | QUOTE))*;

