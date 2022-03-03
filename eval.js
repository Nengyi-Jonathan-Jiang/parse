/* Example code

func void main(){
    var int a;
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
                output "You gave me an even number";
            }
            default {
                output "IDK what you gave me";
            }
        }
    }
    output "breh.";
}
*/



/** @param {any[]} a @param {any[]} b */
function arrayEquals(a, b){
    return a == b || a.length == b.length && a.map((s,i)=>s === b[i]).reduce((a,b)=>a&&b,true)
}

class Scope{
    /** @param {Scope} parent */
    constructor(parent){
        this.parentScope = parent;
        /** @type {Map<string, Variable>} */
        this.vars = new Map();
    }
    /** @param {string} name @param {Variable} v */
    registerVar(name, v){
        if(this.vars.has(name)) throw new Error("VARIABLE " + name + "(" + v.type + ") ALREADY DECLARED IN THIS SCOPE");
        this.vars.set(name, v);
    }
    getVar(name){
        if(this.vars.has(name)) return this.vars.get(name);
        if(!this.parentScope) throw new Error("IDENTIFIER " + name + " NOT FOUND");
        return this.parentScope.getVar(name);
    }
}

class Variable{
    /** @param {Type} type @param {any} value */
    constructor(type, value){
        this.type = type;
        this.value = value;
    }
}

class Literal extends Variable{
    /** @param {type} type @param {any} value */
    constructor(type, value){
        super(type, value);
    }
}

class Type{
    /** @param {string} name @param {Map<FunctionSignature, Function>} methods @param {Map<string, Type>} properties*/
    constructor(name, methods, properties){
        this.name = name;
        this.methods = methods;
        this.properties = properties;
    }
}

class Function{
    /** @param {FunctionSignature} signature @param {Type} returns @param {(...args:Variable[])=>Variable} apply*/
    constructor(signature, returns, apply){
        this.signature = signature;
        this.returns = returns;
        this.apply = apply;
    }
}

class FunctionSignature{
    /** @param {string} name @param {Type[]} signature*/
    constructor(name, ...signature){
        this.name = name;
        this.signature = signature;
    }
    /** @param {FunctionSignature} other */
    equals(other){
        return this.name == other.name && this.matches(other.signature);
    }
    /** @param {Type[]} signature */
    matches(...signature){
        return arrayEquals(this.signature, signature);
    }
}

const NUMBER_T = new Type("number", new Map([
    ["operator+", (a, b)=>a+b]
]))

/** @param {ASTNode} ast */
function evalAST(ast){
    let scopeStack = [new Scope(null)];
	/** @param {ASTNode} node */
	function f(node){
		let {name, value} = node;
		switch(name){
			case "NUMBER_CONST":
            case "OCTAL_CONST":
			case "BOOL_CONST":
            case "HEX_CONST":
				return new Literal("number", +value);
            case "IDENTIFIER":
                return currScope
            
            case "additive_expression":
                switch(value[1].name){
                    case "+":
                        return evalAST(value[0]) + evalAST(value[2]);
                    case "0":
                        return evalAST(value[0]) - evalAST(value[2]);
                }
		}
	}
}