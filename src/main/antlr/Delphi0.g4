/*------------------------------------------------------------------
 * ANTLR4 Delphi grammar
 * 
 * Based on previous work done by Kathleen Jensen, Niklaus Wirth, 
 * Hakki Dogusan, Piet Schoutteten, Terence Parr and Marton Papp.
 *
 * 2013:
 * ANTLR4-adaption and Delphi syntax by Florian Winkelman.
 *------------------------------------------------------------------*/
grammar Delphi0;

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

delphiUnit: UNIT identifier ES interfaceSection implementationSection END? DOT;

interfaceSection: INTERFACE interfaceDeclarationSection*;

interfaceDeclarationSection: usesSection | typeSection | constSection | varSection | labelSection | proceduralDeclaration;

implementationSection: IMPLEMENTATION implementationDeclarationSection* implementationBlocks* compoundStatement?;

implementationDeclarationSection: usesSection | typeSection | constSection | varSection | labelSection;

usesSection: USES identifier (COMMA identifier)* ES;

labelSection: LABEL identifierList ES;

typeSection: TYPE typeDeclarations ES;

typeDeclarations: typeDeclaration (ES typeDeclaration)*;

typeDeclaration: identifier EQ type;

variableDeclarations: variableDeclaration (ES variableDeclarations)* ES?;

ancestorDeclaration: '(' identifierList? ')';

interfaceDeclarationBody: guidSpecifier? interfaceMemberDeclarations END;

guidSpecifier: '[' (identifier | literalString) ']';

interfaceMemberDeclarations: interfaceMemberDeclaration*;

interfaceMemberDeclaration: (propertyDeclaration | methodDeclaration) ES;

classDeclarationBody: classMemberDeclaration* END;

classMemberDeclaration: PRIVATE | PROTECTED | PUBLIC | PUBLISHED
	| CLASS? (fieldDeclaration | propertyDeclaration | methodDeclaration) ES;

fieldDeclaration: variableDeclaration;

variableDeclaration: identifierList typeSpecifier? defaultValue?;

propertyDeclaration: PROPERTY identifier propertyAccessDeclaration? typeSpecifier readAccessorDeclaration? writeAccessorDeclaration? defaultModifier?;

defaultModifier: ES DEFAULT;

readAccessorDeclaration: READ identifier;

writeAccessorDeclaration: WRITE identifier;

propertyAccessDeclaration: '[' parameterList ']';

defaultValue: EQ expression;

typeSpecifier: COLON type;

identifierList: identifier (COMMA identifier)*;

proceduralDeclaration: functionDeclaration | procedureDeclaration;

procedureDeclaration: PROCEDURE identifier parametersDeclaration? functionDirective* ES;

functionDeclaration: FUNCTION identifier parametersDeclaration? typeSpecifier functionDirective* ES;

methodDeclaration: memberConstructorDeclaration | memberDestructorDeclaration
	| memberProcedureDeclaration | memberFunctionDeclaration;

functionDirective: 
	overloadDirective
	| inlineDirective
	| assemblerDirective
	| callConventionDirective
	| oldCallConventionDirective
	| hintingDirective
	| varargsDirective
	| externalDirective
	| forwardDirective
	| UNSAFE;

overloadDirective: ES OVERLOAD;
forwardDirective: ES FORWARD;
inlineDirective: ES INLINE;
assemblerDirective: ES ASSEMBLER;

methodDirective:
	reintroduceDirective
	| overloadDirective
	| bindingDirective
	| abstractDirective
	| finalDirective
	| inlineDirective
	| assemblerDirective
	| callConventionDirective
	| hintingDirective
	| dispIDDirective; 
	
reintroduceDirective: ES REINTRODUCE;
dispIDDirective: ES DISPID expression;

bindingDirective:
	messageDirective
	| staticDirective
	| dynamicDirective
	| overrideDirective
	| virtualDirective;

messageDirective: ES MESSAGE expression;
staticDirective: ES STATIC;
dynamicDirective: ES DYNAMIC;
overrideDirective: ES OVERRIDE;
virtualDirective: ES VIRTUAL;
abstractDirective: ES ABSTRACT;
finalDirective: ES FINAL;
	 		
callConventionDirective:
	cdeclDirective
	| pascalDirective
	| registerDirective
	| safecallDirective
	| stdcallDirective
	| exportDirective;

cdeclDirective: ES CDECL;
pascalDirective: ES PASCAL;
registerDirective: ES REGISTER;
safecallDirective: ES SAFECALL;
stdcallDirective: ES STDCALL;
exportDirective: ES EXPORT;
	 		
oldCallConventionDirective:
	farDirective
	| localDirective
	| nearDirective;
	 		
farDirective: ES FAR;
localDirective: ES LOCAL;
nearDirective: ES NEAR;
	 		
hintingDirective:
	deprecatedDirective
	| experimentalDirective
	| platformDirective
	| libraryDirective;

deprecatedDirective: ES DEPRECATED literalString?;
experimentalDirective: ES EXPERIMENTAL;
platformDirective: ES PLATFORM;
libraryDirective: ES LIBRARY;
varargsDirective: ES VARARGS;
externalDirective: ES EXTERNAL constant externalSpecifier*;

externalSpecifier: NAME constant | INDEX constant;

memberProcedureDeclaration: PROCEDURE identifier parametersDeclaration? methodDirective*;

memberFunctionDeclaration: FUNCTION identifier parametersDeclaration? typeSpecifier methodDirective*;

memberConstructorDeclaration: CONSTRUCTOR identifier parametersDeclaration? methodDirective*;

memberDestructorDeclaration: DESTRUCTOR identifier methodDirective*;

parametersDeclaration: '(' parameterList? ')';

parameterList: parameterDeclaration (ES parameterDeclaration)*;

parameterDeclaration: paramModifier? identifierList typeSpecifier? defaultValue?;

paramModifier: CONST | VAR | OUT;

constSection: CONST (constDeclaration ES)*;

constDeclaration: identifier typeDefinition? '=' initializer;

initializer: arrayInitializer | recordInitializer | expression;

arrayInitializer: '(' constantList ')';

recordInitializer: '(' recordItemList ')';

typeDefinition: COLON type;

elementList: element ( COMMA element )*;

element: expression ( DOTDOT expression )?;

varSection: VAR (variableDeclaration ES)*;

memberImplementation: memberConstructorImplementation | memberDestructorImplementation 
	| memberProcedureImplementation | memberFunctionImplementation;

memberConstructorImplementation: CONSTRUCTOR qualifiedIdentifier parametersDeclaration? ES codeBlock;

memberDestructorImplementation: DESTRUCTOR qualifiedIdentifier ES codeBlock;

memberProcedureImplementation: PROCEDURE qualifiedIdentifier parametersDeclaration? ES codeBlock;

memberFunctionImplementation: FUNCTION qualifiedIdentifier parametersDeclaration? typeSpecifier ES codeBlock;

functionImplementation: FUNCTION identifier (parametersDeclaration? typeSpecifier)? functionDirective* ES codeBlock;

procedureImplementation: PROCEDURE identifier parametersDeclaration? functionDirective* ES codeBlock;

implementationBlock: memberImplementation | proceduralImplementation;

proceduralImplementation: procedureImplementation | functionImplementation;

localBlock: (proceduralImplementation | proceduralDeclaration) ES?;

implementationBlocks: implementationBlock (ES implementationBlock)* ES?; 

qualifiedIdentifier: identifier DOT identifier;

codeBlock: (localBlock | implementationDeclarationSection)* compoundStatement;

statements: statement (ES statement)*;

statement: labelledStatement | unlabelledStatement;

labelledStatement: label COLON unlabelledStatement;

unlabelledStatement: simpleStatement | structuredStatement;

simpleStatement: assignmentStatement | memberAccessStatement | raiseException | gotoStatement | emptyStatement;

structuredStatement: compoundStatement | conditionalStatement | repetetiveStatement | withStatement;

compoundStatement: BEGIN statements END;

conditionalStatement: ifStatement | caseStatement | tryBlock;

repetetiveStatement: whileStatement | repeatStatement | forStatement;

assignmentStatement: (variable | memberAccess) ASSIGNMENT expression;

memberAccessStatement: memberAccess;

forStatement: FOR identifier ASSIGNMENT expression (TO | DOWNTO) expression DO statement;

whileStatement: WHILE expression DO statement;

repeatStatement: REPEAT statements UNTIL expression;

ifStatement: IF expression THEN statement (ELSE statement)?;

memberAccess: INHERITED | INHERITED? memberInvocation (DOT memberInvocation)*;

memberInvocation: (bracketedExpression | functionCall) dimensionQualifiers? POINTER?;

functionCall: identifier argumentList?;

argumentList: '(' expressionList? ')';

expressionList: expression (COMMA expression)*;

expression: simpleExpression (expressionOperator simpleExpression)?;

expressionOperator: EQ | NEQ | LT | LTE | GTE | GT | IN;

simpleExpression: term (simpleOperator term)*;

simpleOperator: PLUS | MINUS | OR | XOR;

term: signedFactor (termOperator signedFactor)*;

termOperator: STAR | MOD | AND | SLASH | DIV | SLASH | SHL | SHR;

sign: PLUS | MINUS;

signedFactor: sign? factor;

factor: (variable | constant | memberAccess | bracketedExpression 
	| typeCheck | negation) (AS typeIdentifier)?;

negation: NOT factor;

variable: AT? identifier (dimensionQualifiers | POINTER)*;

recordItemList: recordItem (COMMA recordItem)*;

recordItem: '(' recordFieldList ')' | recordFieldList;

recordFieldList: recordField (ES recordField)*;

recordField: identifier COLON expression;

typeCheck: memberAccess IS type;

bracketedExpression: '(' expression ')';

raiseException: RAISE expression?;

tryBlock: TRY statements (exceptClause | finallyClause);

exceptClause: EXCEPT (exceptionHandlers ES? | statements) elseCase? END;

exceptionHandlers: exceptionHandler (ES exceptionHandler)*; 

exceptionHandler: ON identifier typeSpecifier? DO statement;

finallyClause: FINALLY statements END;

caseStatement: CASE memberAccess OF caseItems elseCase? END;

caseItems: caseItem (ES caseItem)* ES?;

caseItem: constantList COLON statement;

elseCase: ELSE statements ES?;

label: unsignedInteger | identifier;

gotoStatement: GOTO label;

emptyStatement: ;

withStatement: WITH expressionList DO statement;

type: simpleType | structuredType | pointerType | classType | classOfClassType | interfaceType 
	| procedureType | functionType;

classType: CLASS ancestorDeclaration? classDeclarationBody?;

classOfClassType: CLASS OF typeIdentifier;

interfaceType: INTERFACE (ancestorDeclaration? interfaceDeclarationBody)?;

procedureType: PROCEDURE parametersDeclaration? (OF OBJECT)? functionDirective*;

functionType: FUNCTION parametersDeclaration? typeSpecifier (OF OBJECT)? functionDirective*;

simpleType: scalarType | subrangeType | typeIdentifier | qualifiedTypeIdentifier | staticStringType;

scalarType: '(' scalarDeclarations ')';

scalarDeclarations: scalarDeclaration (COMMA scalarDeclaration)*;

scalarDeclaration: identifier (EQ constant)?;

/* the subrangeType takes const arguments but must support complex const expressions like Low(TFooEnum) */
subrangeType: expression DOTDOT expression;

structuredType: PACKED unpackedStructuredType | unpackedStructuredType;

unpackedStructuredType: arrayType | recordType | setType | fileType;

staticStringType: STRING '(' (identifier | unsignedNumber) ')';

arrayType: ARRAY dimensionQualifiers? OF componentType;

/* adds dimensional qualifiers to an expression, which tell which subordinate element of the object to resolve. */
dimensionQualifiers: '[' dimensionQualifier ( COMMA dimensionQualifier )* ']';

/* The qualifier must support constants (0), ranges (0..1), expressions (intIndex + 3) etc. */
dimensionQualifier: simpleType | expression;

componentType: type;

recordType: RECORD recordFieldDeclaration END;

recordFieldDeclaration: ( fixedPart ( ES variantPart | ES )? | variantPart );

fixedPart: recordSection ( ES recordSection )*;

recordSection: identifierList COLON type;

variantPart: CASE (identifier COLON)? typeIdentifier OF variant ( ES variant | ES )*;

variant: constantList COLON '(' recordFieldDeclaration ')';

setType: SET OF simpleType;

fileType: FILE'^' OF'!' type | FILE;

pointerType: POINTER'^' typeIdentifier;

/* The identifier must be considered a constant because it may identify one. */
constant: unsignedNumber | signedNumber | literalString | primitiveFunctionConstant 
	| charLiteralConstant | setConstant | nil | identifier;

nil: NIL;

constantList: constant ( COMMA constant )*;

setConstant: '[' elementList? ']';

signedNumber: sign unsignedNumber;

unsignedNumber: unsignedInteger | unsignedReal;

unsignedInteger: LITERAL_INTEGER | HEX_LITERAL;

unsignedReal: LITERAL_REAL;

literalString: LITERAL_STRING;

primitiveFunctionConstant: typeIdentifier '(' constant ')';

charLiteralConstant: HASH unsignedInteger;

/* the identifier has to cover every lexer token because otherwise method calls and 
   indirections will fail if they match such a token. Not the END token though. */
identifier: IDENT | typeIdentifier | NAME | MESSAGE | INDEX | WRITE | READ | OBJECT | SET | PROPERTY  | UNIT 
	| INTERFACE | IMPLEMENTATION | USES | TYPE | CLASS | BEGIN | OUT;
typeIdentifier: IDENT | CHR | CHAR | BOOLEAN | INTEGER | CARDINAL | REAL | DOUBLE | STRING;
qualifiedTypeIdentifier: IDENT DOT IDENT;

test: IDENT ':' ;

/*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*/

COMMA: ',';
END: 'end';
LINE_COMMENT: '//' ~[\r\n]* '\r'? '\n' -> channel(HIDDEN);
BLOCK_COMMENT1: '{' .*? '}' -> channel(HIDDEN);
BLOCK_COMMENT2: '(*' .*? '*)' -> channel(HIDDEN);
WS: ( '\t' | ' ' | '\r' | '\n' )+ -> channel(HIDDEN);
ES: ';';
UNIT: 'unit';
INTERFACE: 'interface';
USES: 'uses';
TYPE: 'type';
CLASS: 'class';
PRIVATE: 'private';
PROTECTED: 'protected';
PUBLIC: 'public';
PUBLISHED: 'published';
PROCEDURE: 'procedure';
FUNCTION: 'function';
CONSTRUCTOR: 'constructor';
DESTRUCTOR: 'destructor';
BEGIN: 'begin';
VAR: 'var';
CONST: 'const';
OUT: 'out';
FORWARD: 'forward';
VIRTUAL: 'virtual';
ABSTRACT: 'abstract';
OVERRIDE: 'override';
OVERLOAD: 'overload';
REINTRODUCE: 'reintroduce';
INHERITED: 'inherited';
IMPLEMENTATION: 'implementation';
INTEGER: 'integer';
CARDINAL: 'cardinal';
BOOLEAN: 'boolean';
STRING: 'string';
DOT: '.';
DOTDOT: '..';
CHAR: 'char';
CHR: 'chr';
HASH: '#';
REAL: 'real';
DOUBLE: 'double';
PACKED: 'packed';
RECORD: 'record';
ARRAY: 'array';
COLON: ':';
FILE: 'file';
FOR: 'for';
WHILE: 'while';
REPEAT: 'repeat';
UNTIL: 'until';
IF: 'if';
THEN: 'then';
ELSE: 'else';
CASE: 'case';
OF: 'of';
ON: 'on';
TRY: 'try';
EXCEPT: 'except';
FINALLY: 'finally';
RAISE: 'raise';
POINTER: '^';
ASSIGNMENT: ':=';
TO: 'to';
DOWNTO: 'downto';
DO: 'do';
AS: 'as';
IS: 'is';
MINUS: '-';
PLUS: '+';
SLASH: '/';
STAR: '*';
NEQ: '<>';
EQ: '=';
GT: '>'; 
LT: '<'; 
GTE: '>=';
LTE: '<=';
MOD: 'mod';
DIV: 'div';
NOT: 'not';
AND: 'and';
OR: 'or';
XOR: 'xor';
SHL: 'shl';
SHR: 'shr';
OBJECT: 'object';
SET: 'set';
IN: 'in';
NIL: 'nil';
AT: '@';
GOTO: 'goto';
PROPERTY: 'property';
READ: 'read';
WRITE: 'write';
CDECL: 'cdecl';
STDCALL: 'stdcall';
UNSAFE: 'unsafe';
INLINE: 'inline';
ASSEMBLER: 'assembler';
DISPID: 'dispid';
MESSAGE: 'message';
STATIC: 'static';
DYNAMIC: 'dynamic';
FINAL: 'final';
PASCAL: 'pascal';
REGISTER: 'register';
SAFECALL: 'safecall';
EXPORT: 'export';
FAR: 'far';
LOCAL: 'local';
NEAR: 'near';
DEPRECATED: 'deprecated';
EXPERIMENTAL: 'experimental';
PLATFORM: 'platform';
LIBRARY: 'library';
VARARGS: 'varargs';
EXTERNAL: 'external';
NAME: 'name';
INDEX: 'index';
DEFAULT: 'default';
LABEL: 'label';
WITH: 'with';
IDENT: [a-zA-Z_] [a-zA-Z0-9_]*;
LITERAL_INTEGER: [0-9]+;
HEX_LITERAL: '$' [a-fA-F0-9]+;
fragment EXPONENTIAL_NOTATION: [0-9]+ (DOT [0-9]+)? 'e' (MINUS | PLUS)? [0-9]+; 
fragment FP_NOTATION: [0-9]+ (DOT [0-9]+); 
LITERAL_REAL: EXPONENTIAL_NOTATION | FP_NOTATION;
LITERAL_STRING: '\'' (~ '\'' | '\'\'')* '\'';
