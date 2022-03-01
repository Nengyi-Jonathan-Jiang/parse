const symbols_and_operators = `
	&& || ^^ ++ -- += *= -= /= %= == <= >= != << >> ; ! = < > ( ) [ ] { } , . + - * / % ? :
	| & \\^ \\$ # @ \\\\
`.trim().split(/ |\n/g).map(i=>i.trim())

const keywords = [
	"attribute",
    "const",
    "break", "continue",
    "do", "else", "for", "if", "while",
    "discard",
    "return",
    "in", "out", "inout",
    "uniform", "varying", "centroid",
    "struct",
    "invariant"
]

const types = [
    "int",
    "void",
    "bool",
    "float",
    "bvec2", "bvec3", "bvec4",
    "ivec2", "ivec3", "ivec4",
    "vec2",  "vec3",  "vec4",
    "mat2",  "mat3",  "mat4",
    "mat2x2", "mat2x3", "mat2x4",
	"mat3x2", "mat3x3", "mat3x4",
    "mat4x2", "mat4x3", "mat4x4",
    "sampler1d", "sampler2d", "sampler3d",
    "samplercube",
    "sampler1dshadow", "sampler2dshadow",
]

var allowed_tokens = [
		//Comments
		["COMMENT", /^(\/\/[^\n]*)/],
		["MULTILINE_COMMENT", /^\/\\*.*\\*\//s],

		//Symbols and operators
		...`
			&& || ^^ ++ -- += *= -= /= %= == <= >= != << >> ; ! = < > ( ) [ ] { } , . + - * / % ? :
			| & \\^ \\$ # @ \\\\
		`.trim().split(/ |\n/g).map(i=>i.trim()).map(i => [i, new RegExp(`^(${i.replace(/\||\+|\*|\(|\)|\[|]|\.|\?|\^/g,"\\$&")})`)]),

		//Keywords
		...[
			"attribute",
			"const",
			"break", "continue",
			"if", "else", 
			"do", "for", "while",
			"switch", "case",
			"return",
			"class",
			"struct",
			"public",
			"private",
		].map(i=>[i.toUpperCase(), new RegExp(`^${i}\\b`)]),

		//Literals
		["BOOL_CONST", /^(true|false)\b/],
		["STRING_CONST", /^"(\\\\|\\"|[^"])*"/],
		["CHAR_CONST", /^'.'/],
		
		//Identifiers
		["IDENTIFIER", /^([a-zA-Z_][a-zA-Z0-9_]*)\b/],

		//Number literal
		["NUMBER_CONST", /^(\d+\.\d*|\.\d+|[1-9]\d*|0)\b/],
		["HEX_CONST", /^0x[0-9A-Fa-f]+\b/],
		["OCTAL_CONST", /^0[0-7]+\b/],
		["BINARY_CONST", /^0b[01]+\b/],
		
]

var tokenizer = new Tokenizer(
	allowed_tokens.map(i=>[i[0],{regex:i[1],func:s=>s}]),
	["COMMENT","MULTILINE_COMMENT"]
);

var grammar_s = `
__START__ := statements
statements := statement_list
statements := ε

variable_identifier := IDENTIFIER

number := NUMBER_CONST
number := HEX_CONST
number := OCTAL_CONST
number := BINARY_CONST

primary_expression  := variable_identifier
primary_expression  := number
primary_expression  := BOOL_CONST
primary_expression  := STRING_CONST
primary_expression  := ( expression )

postfix_expression := primary_expression
postfix_expression := postfix_expression [ integer_expression ]
postfix_expression := function_call
postfix_expression := postfix_expression . IDENTIFIER
postfix_expression := postfix_expression ++
postfix_expression := postfix_expression --

integer_expression := expression

unary_expression := postfix_expression
unary_expression := unary_operator unary_expression

unary_operator := -
unary_operator := !
unary_operator := ~
unary_operator := *
unary_operator := ++
unary_operator := --

multiplicative_expression := unary_expression
multiplicative_expression := multiplicative_expression * unary_expression
multiplicative_expression := multiplicative_expression / unary_expression
multiplicative_expression := multiplicative_expression % unary_expression

additive_expression := multiplicative_expression
additive_expression := additive_expression + multiplicative_expression
additive_expression := additive_expression - multiplicative_expression

shift_expression := additive_expression
shift_expression := shift_expression << additive_expression
shift_expression := shift_expression >> additive_expression

relational_expression := shift_expression
relational_expression := relational_expression < shift_expression
relational_expression := relational_expression > shift_expression
relational_expression := relational_expression <= shift_expression
relational_expression := relational_expression >= shift_expression

equality_expression := relational_expression
equality_expression := equality_expression == relational_expression
equality_expression := equality_expression != relational_expression

and_expression := equality_expression
and_expression := and_expression & equality_expression

exclusive_or_expression := and_expression
exclusive_or_expression := exclusive_or_expression ^ and_expression
inclusive_or_expression := exclusive_or_expression
inclusive_or_expression := inclusive_or_expression | exclusive_or_expression

logical_and_expression := inclusive_or_expression
logical_and_expression := logical_and_expression && inclusive_or_expression

logical_xor_expression := logical_and_expression
logical_xor_expression := logical_xor_expression ^^ logical_and_expression

logical_or_expression := logical_xor_expression
logical_or_expression := logical_or_expression || logical_xor_expression

conditional_expression := logical_or_expression
conditional_expression := logical_or_expression ? expression : assignment_expression

assignment_expression := conditional_expression
assignment_expression := unary_expression assignment_operator assignment_expression

assignment_operator := =
assignment_operator := *=
assignment_operator := /=
assignment_operator := %=
assignment_operator := +=
assignment_operator := -=
assignment_operator := <<=
assignment_operator := >>=
assignment_operator := &=
assignment_operator := ^=
assignment_operator := |=

expression := assignment_expression
expression := expression , assignment_expression

constant_expression := conditional_expression

declaration := function_prototype ;
declaration := init_declarator_list ;
declaration := function_definition

function_call := function_call_or_method

function_call_or_method := function_call_generic
function_call_or_method := postfix_expression . function_call_generic

function_call_generic := function_call_header_with_parameters )
function_call_generic := function_call_header_no_parameters )

function_call_header_no_parameters := function_call_header VOID
function_call_header_no_parameters := function_call_header

function_call_header_with_parameters := function_call_header assignment_expression
function_call_header_with_parameters := function_call_header_with_parameters , assignment_expression

function_call_header := function_identifier (

function_identifier := type_specifier
function_identifier := IDENTIFIER
function_identifier := FIELD_SELECTION

function_prototype := function_declarator )

function_declarator := function_header
function_declarator := function_header_with_parameters

function_header_with_parameters := function_header parameter_declaration
function_header_with_parameters := function_header_with_parameters , parameter_declaration

function_header := fully_specified_type IDENTIFIER (

function_definition := function_prototype compound_statement_no_new_scope

parameter_declarator := type_specifier IDENTIFIER
parameter_declarator := type_specifier IDENTIFIER [ constant_expression ]

parameter_declaration := type_qualifier parameter_qualifier parameter_declarator
parameter_declaration := parameter_qualifier parameter_declarator
parameter_declaration := parameter_declarator
parameter_declaration := type_qualifier parameter_qualifier parameter_type_specifier
parameter_declaration := parameter_qualifier parameter_type_specifier
parameter_declaration := parameter_type_specifier

parameter_qualifier := IN
parameter_qualifier := OUT
parameter_qualifier := INOUT

parameter_type_specifier := type_specifier

init_declarator_list := single_declaration
init_declarator_list := init_declarator_list , IDENTIFIER
init_declarator_list := init_declarator_list , IDENTIFIER [ ]
init_declarator_list := init_declarator_list , IDENTIFIER [ constant_expression ]
init_declarator_list := init_declarator_list , IDENTIFIER [ ] = initializer
init_declarator_list := init_declarator_list , IDENTIFIER [ constant_expression ] = initializer
init_declarator_list := init_declarator_list , IDENTIFIER = initializer

single_declaration := fully_specified_type
single_declaration := fully_specified_type IDENTIFIER
single_declaration := fully_specified_type IDENTIFIER [ ]
single_declaration := fully_specified_type IDENTIFIER [ constant_expression ]
single_declaration := fully_specified_type IDENTIFIER [ ] = initializer
single_declaration := fully_specified_type IDENTIFIER [ constant_expression ] = initializer
single_declaration := fully_specified_type IDENTIFIER = initializer

fully_specified_type := type_specifier
fully_specified_type := type_qualifier type_specifier

type_qualifier := CONST
type_qualifier := VARYING
type_qualifier := CENTROID VARYING
type_qualifier := INVARIANT VARYING
type_qualifier := INVARIANT CENTROID VARYING
type_qualifier := UNIFORM
type_specifier := type_specifier_nonarray
type_specifier := type_specifier_nonarray [ ]
type_specifier := type_specifier_nonarray < type_args >
type_specifier := type_specifier_nonarray [ constant_expression ]

type_args := type_specifier
type_args := type_args , type_specifier

type_specifier_nonarray := struct_specifier
type_specifier_nonarray := IDENTIFIER

struct_specifier := STRUCT IDENTIFIER { struct_declaration_list }
struct_specifier := STRUCT { struct_declaration_list }

struct_declaration_list := struct_declaration
struct_declaration_list := struct_declaration_list struct_declaration
struct_declaration := type_specifier struct_declarator_list ;
struct_declarator_list := struct_declarator
struct_declarator_list := struct_declarator_list , struct_declarator
struct_declarator := IDENTIFIER
struct_declarator := IDENTIFIER [ constant_expression ]

initializer := assignment_expression

declaration_statement := declaration

statement := compound_statement
statement := simple_statement

simple_statement := declaration_statement
simple_statement := expression_statement
simple_statement := selection_statement
simple_statement := iteration_statement
simple_statement := jump_statement

compound_statement := { }
compound_statement := { statement_list }

statement_no_new_scope := compound_statement_no_new_scope
statement_no_new_scope := simple_statement

compound_statement_no_new_scope := { }
compound_statement_no_new_scope := { statement_list }

statement_list := statement
statement_list := statement_list statement

expression_statement := ;
expression_statement := expression ;

selection_statement := IF ( expression ) selection_rest_statement

selection_rest_statement := statement ELSE statement
selection_rest_statement := statement

condition := expression
condition := fully_specified_type IDENTIFIER = initializer

iteration_statement := WHILE ( condition ) statement_no_new_scope
iteration_statement := DO statement WHILE ( expression ) ;
iteration_statement := FOR ( for_init_statement for_rest_statement ) statement_no_new_scope

for_init_statement := expression_statement
for_init_statement := declaration_statement

conditionopt := condition

for_rest_statement := conditionopt ;
for_rest_statement := conditionopt ; expression

jump_statement := CONTINUE ;
jump_statement := BREAK ;
jump_statement := RETURN ;
jump_statement := RETURN expression ;
jump_statement := DISCARD ;

translation_unit := external_declaration
translation_unit := translation_unit external_declaration
`


var grammar = new Grammar(...grammar_s
	.trim()
	.split(/\n+/g)
	.filter(i=>i.length)
	.map(i=>i.split(':='))
	.map(
		i=>new ParseRule(
			i[0].trim(),
			...(j=>j[0] == 'ε' ? [] : j)(i[1].trim().split(/ +/g))
		)
	)
)

var parser = new Parser(grammar);



function evalAST(ast){
	
}





{	//Testing
	/**@type {HTMLInputElement}*/
	let input = document.getElementById("input")
	input.value=`
vec2 sqr(vec2 a){
    return vec2(a.x * a.x - a.y * a.y,2.0 * a.x * a.y);
}
float mag(vec2 a){
    return a.x * a.x + a.y * a.y;
}

void main()
{
    float c = 1.0;
    vec2 v = vec2(0,0);
    for(int i = 0; i < 128; i++){
        if(mag(v) > 8.0) break;
        v = sqr(v) + 2.0 * fragCoord;
        c *= 0.95;
    }
    c = 1.0 - c;
    vec3 col = vec3(0,0,0.5) * (1.0 - c) + vec3(1.0,1.0,1.0) * c;
    gl_FragColor = vec4(col,1.0);
}
	`.trim();
	input.oninput = input.onchange = _=>{
		input.style.setProperty("outline", `1px solid ${parser.parse(tokenizer.tokenize(input.value))[1] ? "limegreen" : "red"}`);
	}
	input.oninput();
	//input.onkeypress = 
	input.onkeydown = e => {
		if(e.code == "Backquote"){
			// let tokens = tokenizer.tokenize(input.value)
			// let [chart, success] = parser.parse(tokens);
			// console.log(`[${tokens.join(' ')}]\n${chart}`);

            let tokens = tokenizer.tokenize(input.value);
            let ast = parser.toAST(tokens);

			console.log(ast.toString());
            
			e.preventDefault();
		}
		if(e.key == "/" && e.ctrlKey){
			
            let {value, selectionStart, selectionEnd, selectionDirection} = input;
			value = "\n" + value + "\n";
			selectionStart++;selectionEnd++;
			let prevNewLinePos = value.substring(0, selectionStart).lastIndexOf("\n");
			if(prevNewLinePos == -1) prevNewLinePos = 0;
			let nextNewLinePos = value.substring(selectionEnd).indexOf("\n") + selectionEnd;
			if(nextNewLinePos == -1) nextNewLinePos = value.length;
			let changes = "";
			let lines = value.substring(prevNewLinePos, nextNewLinePos).split("\n").slice(1);

			if(lines.map(i=>i.length==0||i.match(/^\s*\/\//)&&true).reduce((a,b)=>a&&b,true)){
				selectionStart -= lines[0].length ? lines[0].match(/(?<=^\s*)\/\/ ?/)[0].length : 0;
				selectionEnd -= lines.map(i=>i.length?i.match(/(?<=^\s*)\/\/ ?/)[0].length:0).reduce((a,b)=>a+b);
				changes = "\n" + lines.map(i=>i.replace(/^(\s*)\/\/ ?/, "$1")).join("\n");
			}
			else
			{
				selectionStart += lines[0].length ? 3 : 0;
				selectionEnd += lines.map(i=>i.length?3:0).reduce((a,b)=>a+b);
				changes = "\n" + lines.map(i=>i.replace(/^((    )*)(.)/, "$1// $3")).join("\n");
			}
			value = value.substring(0, prevNewLinePos) + changes + value.substring(nextNewLinePos);
			input.value = value.substring(1, value.length - 1);
			e.preventDefault();
			
			input.selectionStart = selectionStart - 1;
			input.selectionEnd = selectionEnd - 1;
			input.selectionDirection = selectionDirection;

			input.oninput();
		}
		if(e.code == "Tab"){
			let {value, selectionStart, selectionEnd, selectionDirection} = input;
			let prevNewLinePos = value.substring(0, selectionStart).lastIndexOf("\n");
			if(prevNewLinePos == -1) prevNewLinePos = 0;
			let nextNewLinePos = value.substring(selectionEnd).indexOf("\n") + selectionEnd;
			if(nextNewLinePos == -1) nextNewLinePos = value.length;
			let changes = "";
			if(selectionStart == selectionEnd){
				let before = value.substring(prevNewLinePos, selectionEnd);
				let after = value.substring(selectionEnd, nextNewLinePos);
                if(e.shiftKey == false){
    				let padding_length = 4 - ((before.length + 3) % 4);
    				changes = before + " ".repeat(padding_length) + after;
    				selectionEnd += padding_length;
    				selectionStart += padding_length;
                }
                else{
                    let unpad_length = (before.match(/^\n {0,4}/)[0] || "\n").length - 1;
                    changes = before.replace(/^\n {0,4}/, "\n") + after;
                    selectionEnd -= unpad_length;
                    selectionStart -= unpad_length;
                }
			}
			else{
				let lines = value.substring(prevNewLinePos, nextNewLinePos).split("\n").slice(1);
				if(e.shiftKey == false){
					changes = "\n" + lines.map(i=>"    " + i).join("\n");
					selectionStart += 4;
					selectionEnd += lines.length * 4;
				}
				else{
					selectionEnd -= lines.map(i=>(i.match(/^ {0,4}/)[0] || []).length).reduce((a,b)=>a+b);
					selectionStart -= (lines[0].match(/^ {0,4}/)[0] || []).length;
					changes = "\n" + lines.map(i=>i.replace(/^ {0,4}/,"")).join("\n");
				}
			}
			input.value = value.substring(0, prevNewLinePos) + changes + value.substring(nextNewLinePos);
			e.preventDefault();
			
			input.selectionStart = selectionStart;
			input.selectionEnd = selectionEnd;
			input.selectionDirection = selectionDirection;
		}
	}
}