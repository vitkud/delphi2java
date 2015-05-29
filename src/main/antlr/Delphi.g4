grammar Delphi;

//****************************
//section start
//****************************
file                         : program | library | unit | packageE
                             ;
//****************************
//section fileDefinition
//****************************

program                      : (programHead)? (usesFileClause)? block '.'
                             ;
programHead                  : PROGRAM namespaceName (programParmSeq)? ';'
                             ;
programParmSeq               : '(' (ident (',' ident)* )? ')'
                             ;
library                      : libraryHead (usesFileClause)? block '.'
                             ;
libraryHead                  : LIBRARY namespaceName (hintingDirective)* ';'
                             ;
packageE                     : packageHead requiresClause (containsClause)? END '.'
                             ;
packageHead                  : PACKAGE namespaceName ';'
                             ;
unit                         : unitHead unitInterface unitImplementation unitBlock '.'
                             ;
unitHead                     : UNIT namespaceName (hintingDirective)* ';'
                             ;
unitInterface                : INTERFACE (usesClause)? (interfaceDecl)*
                             ;
unitImplementation           : IMPLEMENTATION (usesClause)? (declSection)*
                             ;
unitBlock                    : unitInitialization END
                             | compoundStatement
                             | END
                             ;
unitInitialization           : INITIALIZATION statementList (unitFinalization)?
                             ;
unitFinalization             : FINALIZATION statementList
                             ;
//****************************
//section fileUsage
//****************************
containsClause               : CONTAINS namespaceFileNameList
                             ;
requiresClause               : REQUIRES namespaceNameList
                             ;
usesClause                   : USES namespaceNameList
                             ;
usesFileClause               : USES namespaceFileNameList
                             ;
namespaceFileNameList        : namespaceFileName (',' namespaceFileName)* ';'
                             ;
namespaceFileName            : namespaceName (IN QuotedString)?
                             ;
namespaceNameList            : namespaceName (',' namespaceName)* ';'
                             ;
//****************************
//section declaration
//****************************
block                        : (declSection)* (blockBody)?
                             ;
blockBody                    : compoundStatement
                             | assemblerStatement
                             ;
declSection                  : labelDeclSection
                             | constSection
                             | typeSection
                             | varSection
                             | exportedProcHeading
                             | methodDecl
                             | procDecl
                             | exportsSection
                             | assemblyAttribute  			// .net only
                             ;
interfaceDecl                : constSection
                             | typeSection
                             | varSection
                             | exportedProcHeading
                             | exportsSection
                             | procDecl
   			     			 | methodDecl
                             | assemblyAttribute
                             ;	// ? .net only
labelDeclSection             : LABEL label (',' label)* ';'
                             ;
constSection                 : constKey (constDeclaration)*
                             ;
constKey                     : CONST
                             | RESOURCESTRING
                             ;
constDeclaration             : (customAttribute)? ident (':' typeDecl)? '=' constExpression (hintingDirective)* ';'
                             ;
typeSection                  : TYPE typeDeclaration (typeDeclaration)*
                             ;
typeDeclaration              : (customAttribute)? genericTypeIdent '=' typeDecl (hintingDirective)* ';'
                             ;
varSection                   : varKey varDeclaration (varDeclaration)*
                             ;
varKey                       : VAR
                             | THREADVAR
                             ;
// threadvar geen initializations alleen globaal
varDeclaration               : (customAttribute)? identListFlat ':' typeDecl (varValueSpec)? (hintingDirective)* ';'
                             ;
varValueSpec                 : ABSOLUTE ident
                             | ABSOLUTE constExpression
                             | '=' constExpression
                             ;
exportsSection               : EXPORTS ident exportItem (',' ident exportItem)* ';'
                             ;
exportItem                   : ('(' (formalParameterList)? ')')? (INDEX expression)? (NAME expression)? (RESIDENT)?
                             ;
//****************************
//section type
//****************************
typeDecl                     : strucType
                             | pointerType
                             | stringType
                             | procedureType 
                             | variantType
                             | (TYPE)? typeId (genericPostfix)?
                             | simpleType
                             ;
strucType                    : (PACKED)? strucTypePart
                             ;
strucTypePart                : arrayType
                             | setType
                             | fileType
                             | classDecl
                             ;

arrayType                    :  ARRAY ('[' (arrayIndex)? (',' (arrayIndex)?)* ']')? OF arraySubType 
                             ;

// empty Array index for .NET only
arrayIndex                   : typeId
                             | constExpression '..' constExpression
                             ;

arraySubType                 : CONST
                             | typeDecl
                             ;
setType                      : SET OF typeDecl
                             ;
// set type alleen ordinal of subrange type
fileType                     : FILE (OF typeDecl)?
                             ;
pointerType                  : '^' typeDecl
                             | POINTER
                             ;
stringType                   : STRING ('[' expression ']')?
                             ;
codePageNumber               : '(' constExpression ')'
                             ;
procedureType                : methodType
							 | simpleProcedureType
                             | procedureReference
                             ;
methodType					 : procedureTypeHeading OF OBJECT
							 ;
simpleProcedureType          : procedureTypeHeading ( (';')? callConventionNoSemi)?
                             ;
procedureReference           : REFERENCE TO procedureTypeHeading
                             ;
procedureTypeHeading         : FUNCTION (formalParameterSection)? ':' (customAttribute)? typeDecl
                             | PROCEDURE (formalParameterSection)?
                             ;
variantType                  : VARIANT // SzJ TODO TEMP
                             ;
simpleType                   : ident
                             | subRangeType
                             | enumType
                             ;
subRangeType                 : constExpression ('..' constExpression)?
                             ;
enumType                     : '(' ident ('=' expression)? (',' ident ('=' expression)? )* ')'
                             ;
typeId                       : namespacedQualifiedIdent
                             ;

//****************************
//section generics
//****************************
genericTypeIdent             : ident (genericDefinition)?
                             ;
genericDefinition            : simpleGenericDefinition
                             | constrainedGenericDefinition
                             ;
simpleGenericDefinition      : '<' ident (',' ident)* '>'
                             ;
constrainedGenericDefinition : '<' constrainedGeneric (';' constrainedGeneric)* '>'
                             ;
constrainedGeneric           : ident (':' genericConstraint (',' genericConstraint)*)?
                             ;
genericConstraint            : ident
                             | RECORD
                             | CLASS
                             | CONSTRUCTOR
                             ;
genericPostfix               : '<' typeDecl (',' typeDecl)* '>'
                             ;
//****************************
//section class
//****************************
classDecl                    : classTypeTypeDecl
                             | classTypeDecl
                             | classHelperDecl
                             | interfaceTypeDecl
                             | objectDecl
                             | recordDecl
                             | recordHelperDecl
                             ;
classTypeTypeDecl            : CLASS OF typeId
                             ;
classTypeDecl                : CLASS (classState)? (classParent)? (classItem)* END
                             | CLASS (classParent)?
                             ;
classState                   : SEALED
                             | ABSTRACT
                             ;
classParent                  : '(' genericTypeIdent (',' genericTypeIdent)* ')'
							 ;
classItem                    : visibility
                             | classMethod
                             | classMethodResolution
                             | classField
                             | classProperty
                             | constSection
                             | typeSection
                             | (CLASS)? varSection
                             ;
classHelperDecl              : CLASS HELPER (classParent)? FOR typeId (classHelperItem)* END
                             ;
classHelperItem              : visibility
                             | classMethod
                             | classProperty
                             | (CLASS)? varSection
                             ;
interfaceTypeDecl            : interfaceKey (classParent)? (interfaceGuid)? (interfaceItem)* END
                             | interfaceKey (classParent)? 
                             ;
interfaceKey                 : INTERFACE
                             | DISPINTERFACE
                             ;
interfaceGuid                : '[' QuotedString ']'
                             ;
interfaceItem                : classMethod
                             | (CLASS)? classProperty
                             ;
objectDecl                   : OBJECT (classParent)? (objectItem)* END
                             ;
objectItem                   : visibility
                             | classMethod
                             | classField
                             ;
recordDecl                   : simpleRecord
                             | variantRecord
                             ;
simpleRecord                 : RECORD (recordField)* (recordItem)* END
                             ;
variantRecord                : RECORD (recordField)* recordVariantSection END
                             ;
recordItem                   : visibility			//ADDED
							 | classMethod
                             | classProperty
                             | constSection
                             | typeSection
                             | recordField
                             | (CLASS)? varSection
                             ;
recordField                  : identList ':' typeDecl (hintingDirective)* (';')?	//CHANGED not needed ; at the end
                             ;
recordVariantField           : identList ':' typeDecl (hintingDirective)* (';')?
                             ;
recordVariantSection         : CASE (ident ':')? typeDecl OF (recordVariant | ';') (recordVariant | ';')*
                             ;
recordVariant                : constExpression (',' constExpression)* ':' '(' (recordVariantField)* ')'		//CHANGED to recordVariantField from recordField
                             ;
recordHelperDecl             : RECORD HELPER FOR typeId (recordHelperItem)* END
                             ;
recordHelperItem             : classMethod
                             | classProperty
                             ;
classMethod                  : methodKey ident (genericDefinition)? (formalParameterSection)? ';' (methodDirective)* 
                             | (CLASS)? FUNCTION ident (genericDefinition)? (formalParameterSection)? ':' (customAttribute)? typeDecl ';' (methodDirective)*
                             ;
classMethodResolution        : (CLASS)? procKey typeId '.' ident '=' ident ';'
                             ;
classField                   : (customAttribute)? identList ':' typeDecl ';' (hintingDirective)* 
                             ;
classProperty                : (customAttribute)? (CLASS)? PROPERTY ident (classPropertyArray)? (':' genericTypeIdent)? (classPropertyIndex)? (classPropertySpecifier)* ';' (classPropertyEndSpecifier)*
							 // CHANGED added (classPropertySpecifier)* at end for "default;"
							 // CHANGEDD to genericTypeIdent for "property QueryBuilder : IQueryBuilder<GenericRecord>"
                             ;
classPropertyArray           : '[' formalParameterList ']'
                             ;
classPropertyIndex           : INDEX expression (';')?	//CHANGED to (';')?
                             ;
classPropertySpecifier       : classPropertyReadWrite		//CHANGED removed ';'
                             | classPropertyDispInterface
                             | STORED expression 
                             | DEFAULT expression 
                             | DEFAULT              	// for array properties only (1 per class)
                             | NODEFAULT 
                             | IMPLEMENTS typeId   	// not on .NET
                             ;
classPropertyEndSpecifier    : STORED expression ';'		//ADDED used in classProperty at end
                             | DEFAULT expression ';'
                             | DEFAULT ';'             
                             | NODEFAULT ';'
							 ;

classPropertyReadWrite       : READ qualifiedIdent ('[' expression ']')?  // Waarom qualified ident???	//ADDED []
                             | WRITE qualifiedIdent ('[' expression ']')?	//ADDED []
                             | ADD qualifiedIdent    	 		// .NET multicast event
                             | REMOVE qualifiedIdent    		// .NET multicast event
                             ;
classPropertyDispInterface   : READONLY ';'
                             | WRITEONLY ';'
                             | dispIDDirective
                             ;
visibility                   : (STRICT)? PROTECTED 
                             | (STRICT)? PRIVATE
                             | PUBLIC
                             | PUBLISHED 
                             | AUTOMATED     // win32 deprecated
                             ;
//****************************
//section procedure
//****************************
exportedProcHeading          : PROCEDURE ident (formalParameterSection)? ':' (customAttribute)? typeDecl ';' (functionDirective)*
                             | FUNCTION ident (formalParameterSection)? ';' (functionDirective)*
                             ;
methodDecl                   : methodDeclHeading ';' (methodDirective)* (methodBody)?
                             ;
methodDeclHeading            : (customAttribute)? methodKey qualifiedIdent (genericDefinition ('.' ident)? )? (formalParameterSection)?
                             | (customAttribute)? (CLASS)? FUNCTION qualifiedIdent (genericDefinition ('.' ident)? )? (formalParameterSection)? (':' (customAttribute)? typeDecl)?
                             ;
                                                                                                          
methodKey                    : (CLASS)? PROCEDURE
                             | CONSTRUCTOR
                             | DESTRUCTOR
                             | CLASS OPERATOR
                             ;
procDecl                     : procDeclHeading ';' (functionDirective)* (procBody)?
                             ;
procDeclHeading              : (customAttribute)? PROCEDURE ident (formalParameterSection)?
                             | (customAttribute)? FUNCTION ident (formalParameterSection)? ':' typeDecl
                             ;
procKey                      : FUNCTION
                             | PROCEDURE
                             ;
formalParameterSection       : '(' (formalParameterList)? ')'
                             ;
formalParameterList          : formalParameter (';' formalParameter)*
                             ;
formalParameter              : (customAttribute)? (parmType)? identListFlat (':' typeDecl)? ('=' expression)?
                             ;
parmType                     : CONST
                             | VAR
                             | OUT
                             ;
methodBody                   : block ';'
                             ;
procBody                     : FORWARD ';' (functionDirective)*   // CHECKEN ; en directive plaats!
                             | EXTERNAL (NAME expression | INDEX expression)* (functionDirective)* // CHECKEN directive plaats
                             | block ';'
                             ;
//****************************
//section customAttributes // (.NET only)
//****************************
customAttribute              : '[' customAttributeDecl ']'
                             | assemblyAttribute
                             ;
assemblyAttribute            : '[' ASSEMBLY ':' customAttributeDecl ']'
                             ;
customAttributeDecl          :  namespacedQualifiedIdent '(' expressionList ')'
                             ;
//****************************
//section expression
//****************************
expression                   : simpleExpression (relOp simpleExpression)? ('=' expression)? 	//CHANGED, added expression for: "if( functionCall(x, 7+66) = true ) then" syntax
                             | closureExpression
                             ;                           
closureExpression            : PROCEDURE (formalParameterSection)? block
                             | FUNCTION (formalParameterSection)? ':' typeDecl block
                             ;
simpleExpression             : term (addOp term)*
                             ;
term                         : factor (mulOp factor)*
                             ;
factor                       : '@' factor
                             | '@@' factor       // used to get address of proc var
                             | NOT factor
                             | '+' factor
                             | '-' factor
                             | '^' ident           // geeft volgnummer van letter
                             | intNum
                             | realNum
                             | TkAsmHexNum          // Alleen in asm statement
                             | TRUE
                             | FALSE
                             | NIL
                             | '(' expression ')' ('^')? ('.' expression)?				//CHANGED, added  ('^')? ('.' qualifiedIdent)?
                             | stringFactor
                             | setSection
                             | designator
                             | typeId '(' expression ')'
                             ;
stringFactor                 : ControlString (QuotedString ControlString)* (QuotedString)?
                             | QuotedString (ControlString QuotedString)* (ControlString)?
                             ;
setSection                   : '[' (expression ((',' | '..') expression)*)? ']'
                             ;

designator                   : (INHERITED)? ( (namespacedQualifiedIdent | typeId) )? (designatorItem)*
                             ;
designatorItem               : '^'
                             | ('.' | '@') ident 							//CHANGED added '@'
                             | ('<' ident (',' ident)* '>')				//ADDED for proc<sth, sth>.foo;
                             | '[' expressionList ']'
                             | '(' (expression (colonConstruct)? (',' expression (colonConstruct)?)*)? ')'
                             ;
expressionList               : expression (',' expression)*
                             ;
colonConstruct               : ':' expression (':' expression)?
                             ;
// Alleen voor Write/WriteLn.
addOp                        : '+'
                             | '-'
                             | OR
                             | XOR
                             ;
mulOp                        : '*'
                             | '/'
                             | DIV
                             | MOD
                             | AND
                             | SHL
                             | SHR
                             | AS
                             ;
relOp                        : '<'
                             | '>'
                             | '<='
                             | '>='
                             | '<>'
                             | '='
                             | IN
                             | IS
                             ;
//****************************
//section statement
//****************************

statement                    :  statementPart
			     			 |	label ':' 					//CHANGED
                             ;
statementPart                : ifStatement
                             | caseStatement
                             | repeatStatement
                             | whileStatement
                             | forStatement
                             | withStatement
                             | tryStatement
                             | raiseStatement
                             | assemblerStatement
                             | compoundStatement
                             | simpleStatement
                             ;
ifStatement                  : IF expression THEN statement (ELSE statement)? 
                             ;
caseStatement                : CASE expression OF (caseItem)* (ELSE statementList (';')?)? END
                             ;
caseItem                     : caseLabel (',' caseLabel)* ':' statement (';')? // checken of ; sep of scheider is
                             ;
caseLabel   				 : expression ('..' expression)?
                             ;
repeatStatement              : REPEAT (statementList)? UNTIL expression
                             ;
whileStatement               : WHILE expression DO statement
                             ;
forStatement                 : FOR designator ':=' expression TO expression DO statement
                             | FOR designator ':=' expression DOWNTO expression DO statement
                             | FOR designator IN expression DO statement
                             ;
withStatement                : WITH withItem DO statement
                             ;
withItem					 : designator AS designator 			//ADDED
							 | designator (',' designator)*
							 ;
compoundStatement            : BEGIN (statementList)? END
                             ;
statementList                : (statement)? (';' (statement)?)*
                             ;
simpleStatement              : designator ':=' expression			//CHANGED the order of the rules, gotoStatement was first but produced 'continue' errors
                             | designator // call
                             | designator ':=' newStatement // .NET only
                             | gotoStatement
                             ;
gotoStatement                : GOTO label
                             | EXIT ('(' expression ')')?  	
                             | BREAK                          
                             | CONTINUE
                             ;
newStatement                 : NEW '(' (expression)? (',' (expression)?)* (',' constExpression)? ')'
                             ;
//****************************
//section constExpression
//****************************
constExpression              : '(' recordConstExpression (';' recordConstExpression)* ')'	//CHANGED reversed order
                             | '(' constExpression (',' constExpression)* ')'
                             | expression
                             ;
recordConstExpression        : ident ':' constExpression
                             ;
//****************************
//section exceptionStatement
//****************************
tryStatement                 : TRY (statementList)? EXCEPT handlerList END  
                             | TRY (statementList)? FINALLY (statementList)? END
                             ;
handlerList                  : (handler)* (ELSE statementList)?
                             | statementList
                             ;
handler                      : ON (handlerIdent)? typeId DO handlerStatement 	//CHANGED - ; is not required ; handlerIdent not required, example:  "on einvalidoperation do;"
                             ;
handlerIdent				 : ident ':'
							 ;
handlerStatement             : statement (';')?
							 | ';'
							 ;
raiseStatement               : RAISE (designator)? (AT designator)? // CHECKEN!
                             ;			     
//****************************
//section AssemblerStatement
//****************************
assemblerStatement           : ASM ~(END)* END		//ADDED we don't realy care about assembler statements, since they don't contribute to
							 ;								//any measure, just skip, allow all
//****************************
//section directive
//****************************
methodDirective              : reintroduceDirective       	// 1
                             | overloadDirective          	// 2
                             | bindingDirective           	// 3
                             | abstractDirective          	// 3 virtual;
                             | inlineDirective            	// 4 niet virtual or dynamic
                             | callConvention             	// 4
                             | hintingDirective ';'      	// 4 (niet abstract)
                             | oldCallConventionDirective 	// 1
                             | dispIDDirective
                             ;
functionDirective            : overloadDirective          // 1
                             | inlineDirective            // 1
                             | callConvention             // 1
                             | oldCallConventionDirective // 1
                             | hintingDirective ';'      // 1
                             | externalDirective          // 1
                             | UNSAFE ';'              // 1 .net?
                             ;
reintroduceDirective         : REINTRODUCE ';'
                             ;
overloadDirective            : OVERLOAD (';')?		//CHANGE ; not needed
                             ;
bindingDirective             : MESSAGE expression ';'
                             | STATIC ';'
                             | DYNAMIC ';'
                             | OVERRIDE ';'
                             | VIRTUAL ';'
                             ;
abstractDirective            : ABSTRACT ';'
                             | FINAL ';'
                             ;
inlineDirective              : INLINE ';'
                             | ASSEMBLER ';' // deprecated
                             ;
callConvention               : CDECL ';'    //
                             | PASCAL ';'   //
                             | REGISTER ';' //
                             | SAFECALL ';' //
                             | STDCALL ';'  //
                             | EXPORT ';'   // deprecated
                             ;
callConventionNoSemi		 : CDECL    //		//ADDED for procedureType error fixing, without ';' at the end
                             | PASCAL   //
                             | REGISTER //
                             | SAFECALL //
                             | STDCALL  //
                             | EXPORT   // deprecated
                             ;
oldCallConventionDirective   : FAR ';'      // deprecated
                             | LOCAL ';'    // niet in windows maakt functie niet exporteerbaar
                             | NEAR ';'     // deprecated
                             ;
hintingDirective             : DEPRECATED (stringFactor)?
                             | EXPERIMENTAL  // added 2006
                             | PLATFORM
                             | LIBRARY
                             ;
externalDirective            : VARARGS ';'   // alleen bij external cdecl
                             | EXTERNAL ';'
                             | EXTERNAL constExpression (externalSpecifier)* ';' // expression : dll name
                             ;
externalSpecifier            : NAME constExpression
                             | INDEX constExpression   // specific to a platform
                             ;
dispIDDirective              : DISPID expression ';'
                             ;
//****************************
////section general
//****************************
ident                        : TkIdentifier
                             | '&' reservedWord
                             | usedKeywordsAsNames 						//ASSUMPTION: fill with more keywords if needed
                             ;
                             
usedKeywordsAsNames			 : NAME
							 | READONLY
							 | ADD
							 | AT
							 | MESSAGE
							 | POINTER
							 | INDEX
							 | DEFAULT
							 | STRING
							 | CONTINUE
							 | READ
							 | WRITE
							 | REGISTER
							 | OBJECT
							 | VARIANT
							 | OPERATOR
							 | NEW
							 | REMOVE
							 | LOCAL
							 | REFERENCE
							 | CONTAINS
							 | FINAL
							 | BREAK
							 | EXIT
							 ;   
reservedWord                 : TkIdentifier  // Keyword
                             ;
identList                    : ident (',' ident)*
                             ;
identListFlat                : ident (',' ident)*		//ADDED used in formalParemeter
                             ;                                                          
label                        : TkIdentifier
                             | TkIntNum
                             | TkHexNum
                             ;
intNum                       : TkIntNum
                             | TkHexNum
                             ;                             
realNum                      : TkRealNum
                             ;                             
namespacedQualifiedIdent     : (namespaceName '.')? qualifiedIdent
                             ;
namespaceName                : ident ('.' ident)*
                             ;
qualifiedIdent               :  (ident '.')*  ident 	//must stay the way it is, with '.' for proper class method identyfication
          		     		 ;
// KEYWORDS
ABSOLUTE       : A B S O L U T E             ;
ABSTRACT       : A B S T R A C T             ;
ADD            : A D D                       ;
AND            : A N D                       ;
ARRAY          : A R R A Y                   ;
AS             : A S                         ;
ASM            : A S M                       ;
ASSEMBLER      : A S S E M B L E R           ;
ASSEMBLY       : A S S E M B L Y             ;
AT             : A T                         ;
AUTOMATED      : A U T O M A T E D           ;
BEGIN          : B E G I N                   ;
BREAK          : B R E A K                   ;
CASE           : C A S E                     ;
CDECL          : C D E C L                   ;
CLASS          : C L A S S                   ;
CONST          : C O N S T                   ;
CONSTRUCTOR    : C O N S T R U C T O R       ;
CONTAINS       : C O N T A I N S             ;
CONTINUE       : C O N T I N U E             ;
DEFAULT        : D E F A U L T               ;
DEPRECATED     : D E P R E C A T E D         ;
DESTRUCTOR     : D E S T R U C T O R         ;
DISPID         : D I S P I D                 ;
DISPINTERFACE  : D I S P I N T E R F A C E   ;
DIV            : D I V                       ;
DO             : D O                         ;
DOWNTO         : D O W N T O                 ;
DQ             : D Q                         ;
DW             : D W                         ;
DYNAMIC        : D Y N A M I C               ;
ELSE           : E L S E                     ;
END            : E N D                       ;
EXCEPT         : E X C E P T                 ;
EXIT           : E X I T                     ;
EXPERIMENTAL   : E X P E R I M E N T A L     ;
EXPORT         : E X P O R T                 ;
EXPORTS        : E X P O R T S               ;
EXTERNAL       : E X T E R N A L             ;
FAR            : F A R                       ;
FILE           : F I L E                     ;
FINAL          : F I N A L                   ;
FINALIZATION   : F I N A L I Z A T I O N     ;
FINALLY        : F I N A L L Y               ;
FOR            : F O R                       ;
FORWARD        : F O R W A R D               ;
FUNCTION       : F U N C T I O N             ;
GOTO           : G O T O                     ;
HELPER         : H E L P E R                 ;
IF             : I F                         ;
IMPLEMENTATION : I M P L E M E N T A T I O N ;
IMPLEMENTS     : I M P L E M E N T S         ;
IN             : I N                         ;
INDEX          : I N D E X                   ;
INHERITED      : I N H E R I T E D           ;
INITIALIZATION : I N I T I A L I Z A T I O N ;
INLINE         : I N L I N E                 ;
INTERFACE      : I N T E R F A C E           ;
IS             : I S                         ;
LABEL          : L A B E L                   ;
LIBRARY        : L I B R A R Y               ;
LOCAL          : L O C A L                   ;
MESSAGE        : M E S S A G E               ;
MOD            : M O D                       ;
NAME           : N A M E                     ;
NEAR           : N E A R                     ;
NEW            : N E W                       ;
NIL            : N I L                       ;
NODEFAULT      : N O D E F A U L T           ;
NOT            : N O T                       ;
OBJECT         : O B J E C T                 ;
OF             : O F                         ;
ON             : O N                         ;
OPERATOR       : O P E R A T O R             ;
OR             : O R                         ;
OUT            : O U T                       ;
OVERLOAD       : O V E R L O A D             ;
OVERRIDE       : O V E R R I D E             ;
PACKAGE        : P A C K A G E               ;
PACKED         : P A C K E D                 ;
PASCAL         : P A S C A L                 ;
PLATFORM       : P L A T F O R M             ;
POINTER        : P O I N T E R               ;
PRIVATE        : P R I V A T E               ;
PROCEDURE      : P R O C E D U R E           ;
PROGRAM        : P R O G R A M               ;
PROPERTY       : P R O P E R T Y             ;
PROTECTED      : P R O T E C T E D           ;
PUBLIC         : P U B L I C                 ;
PUBLISHED      : P U B L I S H E D           ;
RAISE          : R A I S E                   ;
READ           : R E A D                     ;
READONLY       : R E A D O N L Y             ;
RECORD         : R E C O R D                 ;
REFERENCE      : R E F E R E N C E           ;
REGISTER       : R E G I S T E R             ;
REINTRODUCE    : R E I N T R O D U C E       ;
REMOVE         : R E M O V E                 ;
REPEAT         : R E P E A T                 ;
REQUIRES       : R E Q U I R E S             ;
RESIDENT       : R E S I D E N T             ;
RESOURCESTRING : R E S O U R C E S T R I N G ;
SAFECALL       : S A F E C A L L             ;
SEALED         : S E A L E D                 ;
SET            : S E T                       ;
SHL            : S H L                       ;
SHR            : S H R                       ;
STATIC         : S T A T I C                 ;
STDCALL        : S T D C A L L               ;
STORED         : S T O R E D                 ;
STRICT         : S T R I C T                 ;
STRING         : S T R I N G                 ;
THEN           : T H E N                     ;
THREADVAR      : T H R E A D V A R           ;
TO             : T O                         ;
TRY            : T R Y                       ;
TYPE           : T Y P E                     ;
UNIT           : U N I T                     ;
UNSAFE         : U N S A F E                 ;
UNTIL          : U N T I L                   ;
USES           : U S E S                     ;
VAR            : V A R                       ;
VARARGS        : V A R A R G S               ;
VARIANT        : V A R I A N T               ;
VIRTUAL        : V I R T U A L               ;
WHILE          : W H I L E                   ;
WITH           : W I T H                     ;
WRITE          : W R I T E                   ;
WRITEONLY      : W R I T E O N L Y           ;
XOR            : X O R                       ;
FALSE          : F A L S E                   ;
TRUE           : T R U E                     ;

//----------------------------------------------------------------------------
// OPERATORS
//----------------------------------------------------------------------------
PLUS            : '+'   ;
MINUS           : '-'   ;
STAR            : '*'   ;
SLASH           : '/'   ;
ASSIGN          : ':='  ;
COMMA           : ','   ;
SEMI            : ';'   ;
COLON           : ':'   ;
EQUAL           : '='   ;
NOT_EQUAL       : '<>'  ;
LT              : '<'   ;
LE              : '<='  ;
GE              : '>='  ;
GT              : '>'   ;
LPAREN          : '('   ;
RPAREN          : ')'   ;
LBRACK          : '['   ; // line_tab[line]
LBRACK2         : '(.'  ; // line_tab(.line.)
RBRACK          : ']'   ;
RBRACK2         : '.)'  ;
POINTER2        : '^'   ;
AT2             : '@'   ;
DOT             : '.'   ;// ('.' {$setType(DOTDOT);})?  ;
DOTDOT          : '..'  ;
LCURLY          : '{'   ;
RCURLY          : '}'   ;

//****************************
//section token
//****************************
TkGlobalFunction	     	 : 'FUNCTION_GLOBAL'
			     		 	 ;
TkFunctionName		     	 : 'FUNCTION_NAME'
			     		 	 ;
TkFunctionArgs		     	 : 'FUNCTION_ARGS'
			     		 	 ;
TkFunctionBody		     	 : 'FUNCTION_BODY'
			     		 	 ;
TkFunctionReturn	     	 : 'FUNCTION_RETURN'
			     		 	 ;
TkNewType				 	 : 'NEW_TYPE'
						 	 ;
TkClassOfType				 : 'CLASS_OF_TYPE'
							 ;				
TkVariableType				 : 'VARIABLE_TYPE'
							 ;				
TkVariableIdents			 : 'VARIABLE_IDENTS'
							 ;		
TkVariableParam				 : 'VARIABLE_PARAM'
							 ;		
TkGuid						 : 'INTERFACE_GUID'
							 ;							 					 							 					 			 							 		 	 
TkClassParents				 : 'CLASS_PARENTS'
							 ;							 
TkClassField				 : 'CLASS_FIELD'
							 ;
TkIdentifier                 : (Alpha | '_') (Alpha | Digit | '_')*
                             ;  
TkIntNum                     : Digitseq
                             ;
//TkRealNum                    : Digitseq ('.' (Digitseq)?)? (('e'|'E') ('+'|'-')? Digitseq)?
TkRealNum                    : Digitseq ('.' Digitseq)? (('e'|'E') ('+'|'-')? Digitseq)?	//CHANGED
                             ;
TkHexNum                     : '$' Hexdigitseq
                             ;
TkAsmHexNum                  : Hexdigitseq ('h'|'H')
                             ;
QuotedString                 : '\'' ('\'\'' | ~('\''))* '\''   //taken from PASCAL grammar
                             ;
ControlString                : Controlchar (Controlchar)*
                             ;
fragment
Controlchar                  : '#' Digitseq
                             | '#' '$' Hexdigitseq
                             ;
fragment
Alpha                        : 'a'..'z'
                             | 'A'..'Z'
                             | '\u0080'..'\uFFFE' 			//ADDED unicode support
                             ;
fragment
Digit                        : '0'..'9'
                             ;
fragment
Digitseq                     : Digit (Digit)*
                             ;
fragment
Hexdigit                     : Digit | 'a'..'f' | 'A'..'F'
                             ;
Hexdigitseq                  : Hexdigit (Hexdigit)*
                             ;
COMMENT    					 : ( '//' ~('\n'|'\r')* '\r'? '\n'
    						 |   '(*'  .*? '*)'
    						 |   '{' .*? '}'
    						 ) -> channel(HIDDEN)
    						 ;    						 
WS  						 :   ( ' '
        					 | '\t'
        					 | '\r'
        					 | '\n'
        					 | '\f'
        					 )+ -> channel(HIDDEN)
    						 ;

// case insensitive chars
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
