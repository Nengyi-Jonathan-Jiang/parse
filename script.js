/*

TODO: optimize
- closure in Parser : 1526.3 ms
- isTerminal in Grammar : 1297.3 ms
- addState in ParseChart : 346.1 ms
- reduce in Parser : 248.3 ms
- equals in ParseState : 106.2 ms
- get lookahead in ParseState : 101.1 ms

*/




class Token{
	constructor(type, value, begin, end){
		this.type = type;
		this.value = value;
		this.stringBeginPos = begin;
		this.stringEndPos = end;
	}
	toString(){
		return this.type.length ? this.value.length ? `${this.type}<${this.value}>` : this.type : this.value
	}
}

/**
 * @typedef {{
 *     regex: RegExp,
 *     func: string=>any
 * }} TokenRule
 */
class Tokenizer{
	/**
     * @param {[string, TokenRule][]} tokenRules
     */
	constructor(tokenRules){
		this.tokenRules = tokenRules;
	}
	tokenize(input="") {
	    let tokens = [];
	    for (let i = 0; i < input.length;) {
			if(input.charAt(i).match(/\s/)){i++; continue;}
			let token = null;
			for (let [name, {regex, func}] of this.tokenRules) {
		        let result = input.slice(i).match(regex);
		        if (result !== null) {
		            let text = result[0];
					token = new Token(name, func(text), i, i + text.length);
		            i = i + text.length;
					break;
		        }
		    }
			if(!token){
				return [new Token("ERROR")];
			}
			
	        tokens.push(token);
	    }
	    return tokens;
	}
}


class ParseRule{
	/**
	 * @param {string} lhs
	 * @param {string[]} tkns
	 */
	constructor(lhs, ...tkns){
		this.lhs = lhs;
		this.tkns = tkns || [];
		this.isEpsilon = !(tkns && tkns.length);
	}

	toString(){
		return `${this.lhs.padEnd(36)} -> ${
			this.isEpsilon ? 'ε' : this.tkns.join(' ')
		}`;
	}
}

class ParseState{
	/**
	 * @param {ParseRule} rule
	 * @param {number} from
	 * @param {number} pos
	 * @param {ParseState} prevState
	 * @param {"shift"|"reduce"|"closure"} ruleUsed
	 */
	constructor(rule, from, pos, prevState, ruleUsed){
		this.rule = rule;
		this.from = from;
		this.pos = pos;
        this.prevState = prevState;
		this.ruleUsed = ruleUsed;
	}

	get lhs(){return this.rule.lhs}
	get tkns(){return this.rule.tkns}

	get lookahead(){
		return this.tkns[this.pos];
	}
	get finished(){
		return this.pos >= this.tkns.length;
	}

	/**@param {ParseState} state */
	equals(state){
		return this.pos == state.pos && this.from == state.from && this.rule == state.rule;
	}

	toString(){
		return `${this.lhs.padEnd(36)} -> ${[
			...this.tkns.slice(0, this.pos), '•', ...this.tkns.slice(this.pos)
		].join(' ')} from ${this.from}`;
	}
}

class Grammar{
	/**
	 * @param {ParseRule[]} rules
	 */
	constructor(...rules){
		this.rules = rules;
		this.start = this.rules.filter(i => i.lhs == "__START__")[0];
        
        /** @type {Set<string>} */
        this.allTokens = new Set([].concat(...this.rules.map(i=>i.tkns.concat([i.lhs]))));
        /** @type {Set<string>} */
        this.nonTerminals = new Set(this.rules.map(i=>i.lhs));
        /** @type {Set<string>} */
        this.terminals = new Set(this.allTokens);
        for(let nonTerminal of this.nonTerminals) this.terminals.delete(nonTerminal);
	}
	/**
	 * @param {string} tkn
	 */
	isTerminal(tkn){
        return this.terminals.has(tkn);
	}
	toString(){
		return `Grammar{\n  ${this.rules.join("\n  ")}\n}\n`;
	}
}

class ParseChart{
	/**
	 * @param {Grammar} grammar
	 * @param {number} length
	 */
	constructor(grammar, length){
		this.grammar = grammar;
		/** @type {ParseState[][]} */
		this.chart = [
			[new ParseState(grammar.start, 0, 0, null)], 
			...new Array(length).fill().map(i=>[])
		];
	}
	addState(idx, state){
		for(const i of this.chart[idx]) if(i.equals(state)) return false;
		return this.chart[idx].push(state), true;
	}

	/** @param {number} idx */
	row(idx){
		return this.chart[idx]
	}

	toString(){
		let res = ""
        for(let i = 0; i < this.chart.length; i++){
            res += `== Row ${i} ==\n    ${this.chart[i].join("\n    ")}\n`
		}
        return res
	}
}


class ASTNode{
    /** @param {string} name @param {string|ASTNode[]} value */
    constructor(name, value){
        this.name = name;
        this.value = value;
    }

	toString(){
		return JSON.stringify(this, null, 1)
			.replaceAll(/"name": "(.*)",\n\s*"value":/g,"$1")
			.replaceAll(/\s*(\{|\},?)/g,"")
			.replaceAll(/(.+) "\1"/g,"\"$1\"");
	}
}


class Parser{
	/**
	 * @param {Grammar} grammar 
	 */
	constructor(grammar){
		this.grammar = grammar;

        /** @type {Map<string, Set<ParseRule>>} */
        this.closures = new Map();
        for(let token of this.grammar.allTokens){
            this.closures.set(token, this.grammar.rules.filter(i => i.lhs == token));
        }
	}
	/**
	 * @param {Token[]} tkns 
     * @returns {[ParseChart, ParseState]}
	 */
	parse(tkns){
		let chart = new ParseChart(this.grammar, tkns.length);

		for(let i = 0; i < tkns.length; i++){
			let changed = true;
			for(let j = 0; j < chart.row(i).length; j++){
				const prevState = chart.row(i)[j];
				if(prevState.finished){
					for(let state of this.reduce(chart, prevState)){
						chart.addState(i, state);
					}
				}
				else if(!this.grammar.isTerminal(prevState.lookahead)){
					for(let state of this.closure(i, prevState)){
						chart.addState(i, state);
					}
				}
			}
			for(let prevState of chart.row(i)){
				if(!prevState.finished && this.grammar.isTerminal(prevState.lookahead)){
					if(prevState.rule.isEpsilon || tkns[i].type == prevState.lookahead){
						chart.addState(i + 1, this.shift(prevState));
					}
				}
			}
		}

		let i = tkns.length;
		for(let j = 0; j < chart.row(i).length; j++){
			const prevState = chart.row(i)[j];
			if(prevState.finished){
				for(let state of this.reduce(chart, prevState)){
					chart.addState(i, state);
				}
			}
			else if(!this.grammar.isTerminal(prevState.lookahead)){
				for(let state of this.closure(i, prevState)){
					chart.addState(i, state);
				}
			}
		}

		for(let j = 0; j < chart.row(i).length; j++){
			const prevState = chart.row(i)[j];
			if(prevState.finished && prevState.lhs == "__START__"){
                return [chart, prevState];
            }
		}

		return [chart, null];
	}

    /** @param {Token[]} tokens */
    toAST(tokens){
		let tkns = tokens.map(i=>i);
		
        let [chart, last] = this.parse(tokens);
        if(!last) return null;
        let states = [last];
        while(last.prevState) states.unshift(last = last.prevState);
		
        let astStk = [];
		let stateStk = [];

		let prev = null;
        while(states.length){
            let state = states.shift();
            switch(state.ruleUsed){
				case "shift":
					let token = tkns.shift();
					astStk.push(new ASTNode(token.type, token.value));
					break;
				case "reduce":
					if(prev.rule.tkns.length == 1) break;
					let t = [];
					for(let i = 0; i < prev.rule.tkns.length; i++)
						t.unshift(astStk.pop());
					astStk.push(new ASTNode(prev.rule.lhs, t));
					break;
			}
			prev = state;
        }

		return astStk[0];
    }

	/**
	 * @param {number} from
	 * @param {ParseState} state
	 */
    closure(from, state){
        return this.closures.get(state.lookahead).map(i => new ParseState(i, from, 0, state, "closure"));
	}

	/**
	 * @param {ParseState} state 
	 */
    shift(state){
        return new ParseState(state.rule, state.from, state.pos + 1, state, "shift");
	}

	/**
	 * @param {ParseChart} chart 
	 * @param {ParseState} state 
	 */
    reduce(chart, state){
        return chart.row(state.from)
			.filter(rState => !rState.finished && rState.lookahead == state.lhs)
			.map(rState => new ParseState(rState.rule, rState.from, rState.pos + 1, state, "reduce"));
	}
}

const symbols_and_operators = (
	"&& || ^^ ++ -- += *= -= /= %= == <= >= != << >> ; ! = < > ( ) [ ] { } , . + - * / % ? : "
  + "| & \\^ \\$ # @ ` ~ \\\\"
).split(" ")


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
];
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
		["COMMENT", /^(\/\/[^\n]*)/],
		...symbols_and_operators.map(i => [i, new RegExp(`^(${i.replace(/\||\+|\*|\(|\)|\[|]|\.|\?|\^/g,"\\$&")})`)]),
		...keywords.map(i=>[i.toUpperCase(), new RegExp(`^(${i})\\b`)]),
		["BOOL_CONST", /^(true|false)\b/],
		["STRING_CONST", /^("(\\\\|\\"|[^"])*")/],
		["IDENTIFIER", /^([a-zA-Z_][a-zA-Z0-9_]*)\b/],
		["NUMBER_CONST", /^(\d+\.\d*|\.\d+|\d+)/]
]

var tokenizer = new Tokenizer(allowed_tokens.map(i=>[i[0],{regex:i[1],func:s=>s}]));

var grammar_s = `
__START__ := statements
statements := statement_list
statements := ε

variable_identifier := IDENTIFIER

primary_expression  := variable_identifier
primary_expression  := NUMBER_CONST
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

// grammar_s = `
// __START__ := s
// s := NUMBER_CONST
// s := s + NUMBER_CONST
// `;

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