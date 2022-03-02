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
    let m = new Map();

    (/**@param {ASTNode} node*/function traverse(node){
        switch(node.name){
            case "NUMBER_CONST":
            case "BINARY_CONST":
            case "OCTAL_CONST":
            case "HEX_CONST":
                m.set(node.value, "number-literal");
                break;
            
            case "STRING_CONST":
            case "CHAR_CONST":
                m.set(node.value, "string-literal");
                break;
            
            case "IF": case "ELSE":
            case "TEST": case "SWITCH": case "CASE":
            case "DO": case "WHILE": case "FOR":
                m.set(node.value, "control-keyword");
                break;
            default:
                if(Array.isArray(node.value)) node.value.forEach(traverse);
                else m.set(node.value, "unknown");
        }
    })(ast);

    console.log(m);

    let res = originaltext;

    for(let tk of tokens.reverse()){
        let {stringBeginPos, stringEndPos} = tk;
        res = res.substring(0, stringBeginPos) + "<span class=\"" + (m.get(tk) || "unknown") + "\">" + res.substring(stringBeginPos, stringEndPos) + "</span>" + res.substring(stringEndPos);
    }

    targetDiv.innerHTML = res;
}