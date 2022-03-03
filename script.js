var allowed_tokens = [
		//Comments
		["COMMENT", /^(\/\/[^\n]*)/],
		["MULTILINE_COMMENT", /^\/\\*.*\\*\//s],

		//Symbols and operators
		...`
			>>= <<= && || ^^ ++ -- += *= -= /= %= == <= >= != << >> -> :: ; ! = < > ( ) [ ] { } , . + - * / % ? :
			| & ^ \\$ # @ \\\\
		`.trim().split(/ |\n/g).map(i=>i.trim()).map(i => [i, new RegExp(`^(${i.replace(/\||\+|\*|\(|\)|\[|]|\.|\?|\^/g,"\\$&")})`)]),

		//Keywords
		...[
			"const",

			"class", "func", "var", 

			"break", "continue",
			"do", "while", "for", 
			"if", "else", 
			"switch", "test", "case", "default", 
			
			"return",

			"public", "private", "protected", 

			"static",

			"input",
			"output",
		].map(i=>[i.toUpperCase(), new RegExp(`^${i}\\b`)]),

		//Literals
		["BOOL_CONST", /^(true|false)\b/],
		["STRING_CONST", /^"([^\\"]|\\.|\\")*"/],
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

// var grammar_s = `
// __START__ := statements
// statements := statement_list
// statements := ε

// variable_identifier := IDENTIFIER

// number_literal := NUMBER_CONST
// number_literal := HEX_CONST
// number_literal := OCTAL_CONST
// number_literal := BINARY_CONST

// primary_expression  := variable_identifier
// primary_expression  := number_literal
// primary_expression  := BOOL_CONST
// primary_expression  := STRING_CONST
// primary_expression  := ( expression )

// postfix_expression := primary_expression
// postfix_expression := postfix_expression [ expression ]
// postfix_expression := function_call
// postfix_expression := postfix_expression . IDENTIFIER
// postfix_expression := postfix_expression -> IDENTIFIER
// postfix_expression := postfix_expression ++
// postfix_expression := postfix_expression --

// unary_expression := postfix_expression
// unary_expression := unary_operator unary_expression

// unary_operator := -
// unary_operator := !
// unary_operator := ~
// unary_operator := *
// unary_operator := &
// unary_operator := ++
// unary_operator := --

// multiplicative_expression := unary_expression
// multiplicative_expression := multiplicative_expression * unary_expression
// multiplicative_expression := multiplicative_expression / unary_expression
// multiplicative_expression := multiplicative_expression % unary_expression

// additive_expression := multiplicative_expression
// additive_expression := additive_expression + multiplicative_expression
// additive_expression := additive_expression - multiplicative_expression

// shift_expression := additive_expression
// shift_expression := shift_expression << additive_expression
// shift_expression := shift_expression >> additive_expression

// relational_expression := shift_expression
// relational_expression := relational_expression < shift_expression
// relational_expression := relational_expression > shift_expression
// relational_expression := relational_expression <= shift_expression
// relational_expression := relational_expression >= shift_expression

// equality_expression := relational_expression
// equality_expression := equality_expression == relational_expression
// equality_expression := equality_expression != relational_expression

// binary_and_expression := equality_expression
// binary_and_expression := binary_and_expression & equality_expression

// binary_xor_expression := binary_and_expression
// binary_xor_expression := binary_xor_expression ^ binary_and_expression
// binary_or_expression := binary_xor_expression
// binary_or_expression := binary_or_expression | binary_xor_expression

// logical_and_expression := binary_or_expression
// logical_and_expression := logical_and_expression && binary_or_expression

// logical_xor_expression := logical_and_expression
// logical_xor_expression := logical_xor_expression ^^ logical_and_expression

// logical_or_expression := logical_xor_expression
// logical_or_expression := logical_or_expression || logical_xor_expression

// conditional_expression := logical_or_expression
// conditional_expression := logical_or_expression ? expression : assignment_expression

// assignment_expression := conditional_expression
// assignment_expression := unary_expression assignment_operator assignment_expression

// assignment_operator := =
// assignment_operator := *=
// assignment_operator := /=
// assignment_operator := %=
// assignment_operator := +=
// assignment_operator := -=
// assignment_operator := <<=
// assignment_operator := >>=
// assignment_operator := &=
// assignment_operator := ^=
// assignment_operator := |=

// expression := assignment_expression
// expression := expression , assignment_expression

// lambda_expression := func ( func_params ) func_body

// declaration := function_prototype ;
// declaration := init_declarator_list ;
// declaration := function_definition

// function_call := function_call_or_method

// function_call_or_method := function_call_generic
// function_call_or_method := postfix_expression . function_call_generic

// function_call_generic := function_call_header_with_parameters )
// function_call_generic := function_call_header_no_parameters )

// function_call_header_no_parameters := function_call_header VOID
// function_call_header_no_parameters := function_call_header

// function_call_header_with_parameters := function_call_header assignment_expression
// function_call_header_with_parameters := function_call_header_with_parameters , assignment_expression

// function_call_header := function_identifier (

// function_identifier := type_specifier
// function_identifier := IDENTIFIER
// function_identifier := FIELD_SELECTION

// function_prototype := function_declarator )

// function_declarator := function_header
// function_declarator := function_header_with_parameters

// function_header_with_parameters := function_header parameter_declaration
// function_header_with_parameters := function_header_with_parameters , parameter_declaration

// function_header := fully_specified_type IDENTIFIER (

// function_definition := function_prototype compound_statement_no_new_scope

// parameter_declarator := type_specifier IDENTIFIER
// parameter_declarator := type_specifier IDENTIFIER [ constant_expression ]

// parameter_declaration := type_qualifier parameter_qualifier parameter_declarator
// parameter_declaration := parameter_qualifier parameter_declarator
// parameter_declaration := parameter_declarator
// parameter_declaration := type_qualifier parameter_qualifier parameter_type_specifier
// parameter_declaration := parameter_qualifier parameter_type_specifier
// parameter_declaration := parameter_type_specifier

// parameter_qualifier := IN
// parameter_qualifier := OUT
// parameter_qualifier := INOUT

// parameter_type_specifier := type_specifier

// init_declarator_list := single_declaration
// init_declarator_list := init_declarator_list , IDENTIFIER
// init_declarator_list := init_declarator_list , IDENTIFIER [ ]
// init_declarator_list := init_declarator_list , IDENTIFIER [ constant_expression ]
// init_declarator_list := init_declarator_list , IDENTIFIER [ ] = initializer
// init_declarator_list := init_declarator_list , IDENTIFIER [ constant_expression ] = initializer
// init_declarator_list := init_declarator_list , IDENTIFIER = initializer

// single_declaration := fully_specified_type
// single_declaration := fully_specified_type IDENTIFIER
// single_declaration := fully_specified_type IDENTIFIER [ ]
// single_declaration := fully_specified_type IDENTIFIER [ constant_expression ]
// single_declaration := fully_specified_type IDENTIFIER [ ] = initializer
// single_declaration := fully_specified_type IDENTIFIER [ constant_expression ] = initializer
// single_declaration := fully_specified_type IDENTIFIER = initializer

// fully_specified_type := type_specifier
// fully_specified_type := type_qualifier type_specifier

// type_qualifier := CONST
// type_qualifier := VARYING
// type_qualifier := CENTROID VARYING
// type_qualifier := INVARIANT VARYING
// type_qualifier := INVARIANT CENTROID VARYING
// type_qualifier := UNIFORM
// type_specifier := type_specifier_nonarray
// type_specifier := type_specifier_nonarray [ ]
// type_specifier := type_specifier_nonarray < type_args >
// type_specifier := type_specifier_nonarray [ constant_expression ]

// type_args := type_specifier
// type_args := type_args , type_specifier

// type_specifier_nonarray := struct_specifier
// type_specifier_nonarray := IDENTIFIER

// struct_specifier := STRUCT IDENTIFIER { struct_declaration_list }
// struct_specifier := STRUCT { struct_declaration_list }

// struct_declaration_list := struct_declaration
// struct_declaration_list := struct_declaration_list struct_declaration
// struct_declaration := type_specifier struct_declarator_list ;
// struct_declarator_list := struct_declarator
// struct_declarator_list := struct_declarator_list , struct_declarator
// struct_declarator := IDENTIFIER
// struct_declarator := IDENTIFIER [ constant_expression ]

// initializer := assignment_expression

// declaration_statement := declaration

// statement := compound_statement
// statement := simple_statement

// simple_statement := declaration_statement
// simple_statement := expression_statement
// simple_statement := selection_statement
// simple_statement := iteration_statement
// simple_statement := jump_statement

// compound_statement := { }
// compound_statement := { statement_list }

// statement_no_new_scope := compound_statement_no_new_scope
// statement_no_new_scope := simple_statement

// compound_statement_no_new_scope := { }
// compound_statement_no_new_scope := { statement_list }

// statement_list := statement
// statement_list := statement_list statement

// expression_statement := ;
// expression_statement := expression ;

// selection_statement := IF ( expression ) selection_rest_statement

// selection_rest_statement := statement ELSE statement
// selection_rest_statement := statement

// condition := expression
// condition := fully_specified_type IDENTIFIER = initializer

// iteration_statement := WHILE ( condition ) statement_no_new_scope
// iteration_statement := DO statement WHILE ( expression ) ;
// iteration_statement := FOR ( for_init_statement for_rest_statement ) statement_no_new_scope

// for_init_statement := expression_statement
// for_init_statement := declaration_statement

// conditionopt := condition

// for_rest_statement := conditionopt ;
// for_rest_statement := conditionopt ; expression

// output_statement := OUTPUT expression ;
// input_statement := INPUT variable ;

// jump_statement := CONTINUE ;
// jump_statement := BREAK ;
// jump_statement := RETURN ;
// jump_statement := RETURN expression ;
// jump_statement := GOTO IDENTIFIER ;
// `

var grammar_s = `
__START__ := statements

statements := ε
statements := statement_list

statement_list := statement
statement_list := statement_list statement

statement := expression ;
statement := block_statements
statement := variable_decls
statement := function_decl
statement := output_statement
statement := input_statement
statement := while_loop
statement := do_while_loop
statement := for_loop
statement := if_statement
statement := else_statement
statement := test_statement
statement := switch_statement
statement := jump_statement

primary_expression := ( expression )

primary_expression := HEX_CONST
primary_expression := OCTAL_CONST
primary_expression := BINARY_CONST
primary_expression := NUMBER_CONST

primary_expression := CHAR_CONST
primary_expression := STRING_CONST

primary_expression := IDENTIFIER

postfix_expression := primary_expression
postfix_expression := postfix_expression [ expression ]
postfix_expression := function_call
postfix_expression := postfix_expression . IDENTIFIER
postfix_expression := postfix_expression -> IDENTIFIER
postfix_expression := postfix_expression ++
postfix_expression := postfix_expression --

unary_expression := postfix_expression
unary_expression := unary_operator unary_expression

unary_operator := -
unary_operator := !
unary_operator := ~
unary_operator := *
unary_operator := &
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

binary_and_expression := equality_expression
binary_and_expression := binary_and_expression & equality_expression

binary_xor_expression := binary_and_expression
binary_xor_expression := binary_xor_expression ^ binary_and_expression
binary_or_expression := binary_xor_expression
binary_or_expression := binary_or_expression | binary_xor_expression

logical_and_expression := binary_or_expression
logical_and_expression := logical_and_expression && binary_or_expression

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

type := IDENTIFIER
type := IDENTIFIER :: type
type := type *
type := type &
type := type < template_params >

template_param := expression
template_param := type

template_params := template_param
template_params := template_params , template_param

variable_decls := type VAR variable_inits ;

variable_inits := variable_name_and_assign
variable_inits := variable_inits , variable_name_and_assign

variable_name_and_assign := IDENTIFIER
variable_name_and_assign := IDENTIFIER = expression

function_decl := type FUNC IDENTIFIER ( ) block_statements
function_decl := type FUNC IDENTIFIER ( func_args ) block_statements

func_args := single_variable_decl
func_args := func_args , single_variable_decl

do_while_loop := DO block_statements WHILE ( expression ) ;
while_loop := WHILE ( expression ) statement
for_loop := for ( variable_decls ; expression ; expression ) statement

if_statement := IF ( expression ) statement
else_statement := ELSE statement

test_case_statement := CASE ( expression ) statement
test_case_statement := DEFAULT statement
test_case_statements := ε
test_case_statements := test_case_statements test_case_statement

test_statement := TEST ( expression ) { test_case_statements }

switch_case_statement := CASE expression : statements
switch_case_statements := ε
switch_case_statements := switch_case_statements switch_case_statement

switch_statement := SWITCH ( expression ) { switch_case_statements }

block_statements := { statements }

output_statement := OUTPUT expression ;
input_statement := INPUT expression ;

jump_statement := CONTINUE ;
jump_statement := BREAK ;
jump_statement := RETURN ;
jump_statement := RETURN expression ;
jump_statement := GOTO IDENTIFIER ;
`;


var grammar = new Grammar(...grammar_s
	.trim()
	.split(/\n+/g)
	.filter(i=>i.length && !i.match(/^\/\//))
	.map(i=>i.split(':='))
	.map(
		i=>new ParseRule(
			i[0].trim(),
			...(j=>j[0] == 'ε' ? [] : j)(i[1].trim().split(/ +/g))
		)
	)
)

var parser = new Parser(grammar);

{	//Testing
	/**@type {HTMLInputElement}*/
	let input = document.getElementById("input")
	input.value=`
/* A simple program demonstrating simple control structures and I/O */
void func main(){
	int var a;
	input a;
	while(a != 0){
		test(a){
			case (a == 1){
				output "You said one";
			}
			case (a == 2){
				output "You said two";
			}
			case (a == 3) {
				output "You said three";
			}
			case (a % 2 == 0) {
				output "You gave me an even number_literal";
			}
			default {
				output "IDK what you gave me";
			}
		}
	}
	output "breh.";
}
	`.trim().replaceAll("\t","    ");
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


let textarea = document.querySelector("textarea");
let output = document.getElementById("highlighted");
textarea.onscroll = /**@param e*/ e=>{
	output.scrollTop = textarea.scrollTop;
	output.scrollLeft = textarea.scrollLeft;
}
textarea.addEventListener("input",(f=>(f(),f))(_=>{
  let input = textarea.value;
  try{
  let ast = parser.toAST(tokenizer.tokenize(input));
  highlight(ast, input, output)
  }
  catch{
    output.innerHTML = input;
  }
}))