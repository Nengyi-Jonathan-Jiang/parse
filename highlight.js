/** @param {ASTNode} ast @returns {Token[]} */
function getTokens(ast){
    if(Array.isArray(ast.value)){
        return [].concat(...ast.value.map(getTokens));
    }
    return [ast.value];
}

/** @param {ASTNode} ast @param {string} originaltext @param {HTMLDivElement} targetDiv */
function highlight(ast, originaltext, targetDiv){
    let tokens = getTokens(ast);
    /** @type {Map<Token,string>} */
    let m = basicMap(tokens);
    let stateStack = [];

    (/**@param {ASTNode} node*/function traverse(node){
        switch(node.name){
            // case "function_decl":
                
            //     break;
            default:
                if(Array.isArray(node.value)) node.value.forEach(traverse);
        }
    })(ast);

    // console.log(m);

    let res = originaltext;

    for(let tk of [...tokens].reverse()){
        let {stringBeginPos, stringEndPos} = tk;
        res = res.substring(0, stringBeginPos) + "\0span class=\"" + (m.get(tk) || "unknown") + "\"\1" + res.substring(stringBeginPos, stringEndPos) + "\0/span\1" + res.substring(stringEndPos);
    }

    targetDiv.innerHTML = res.replaceAll("<","&lt;").replaceAll(">","&gt;").replaceAll("\0","<").replaceAll("\1",">");	
}

/** @param {Token[]} tokens */
function basicMap(tokens){
    /** @type {Map<Token,string>} */
    let m = new Map();
    for(let token of tokens){
        switch(token.type){
            case "NUMBER_CONST":
            case "BINARY_CONST":
            case "OCTAL_CONST":
            case "HEX_CONST":
                m.set(token, "number-literal");
                break;
            
            case "STRING_CONST":
            case "CHAR_CONST":
                m.set(token, "string-literal");
                break;
            
            case "IF": case "ELSE":
            case "TEST": case "SWITCH": case "CASE": case "DEFAULT":
            case "DO": case "WHILE": case "FOR":
                m.set(token, "control-keyword");
                break;
			
			case "INPUT": case "OUTPUT":
			case "VAR": case "FUNC":
			case "RETURN":
			case "BREAK": case "CONTINUE":
			case "GOTO":
                m.set(token, "keyword");
                break;
            case ">>=": case "<<=": case "+=":
            case "*=":  case "-=":  case "/=": 
            case "%=":  case "==":  case "&&":
            case "||":  case "^^":  case "++":
            case "--":  case "<=":  case ">=":
            case "!=":  case "<<":  case ">>":
            case "<-":  case "!":   case "=": 
            case "<":   case ">":   case "+":
            case "-":   case "*":   case "/":
            case "%":   case "?":   case ":": 
            case "|":   case "&":   case "^":
                m.set(token, "operator");
                break;
            default:
                m.set(token, "unknown");
        }
    }
    return m;
}

/** @param {Token[]} tokens @param {string} originaltext @param {HTMLDivElement} targetDiv */
function highlightBasic(tokens, originaltext, targetDiv){
    let m = basicMap(tokens);
    let res = originaltext;

    for(let tk of [...tokens].reverse()){
        let {stringBeginPos, stringEndPos} = tk;
        res = res.substring(0, stringBeginPos) + "\0span class=\"" + (m.get(tk) || "unknown") + "\"\1" + res.substring(stringBeginPos, stringEndPos) + "\0span\1" + res.substring(stringEndPos);
    }

    targetDiv.innerHTML = res.replaceAll("<","&lt;").replaceAll(">","&gt;").replaceAll("\0","<").replaceAll("\1",">");
}