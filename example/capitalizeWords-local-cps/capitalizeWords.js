function main() {
    console.log(capitalizeWords("hello world"));
}



function LambdaFn(name) {
    this.name = name;
}

const __lambdas__ = {
    capitalizeWord: new LambdaFn('capitalizeWord')
}

const __Array_map__ = Array.prototype.map;
Array.prototype.map = function(mapFn) {
    if (mapFn instanceof LambdaFn) {
        throw new Error('not implemented');
    } else {
        return __Array_map__.apply(this, arguments);
    }
}

const __lambda_capitalizeWord__ = new LambdaFn();

function original_capitalizeWords(sentence) {
    return sentence.split(' ').map(original_capitalizeWord).join(' ');
}

function original_capitalizeWord(word) {
    return word.charAt(0).toUpperCase() + word.slice(1);
}

function varAssign_capitalizeWords(sentence) {
    const var1 = sentence.split(' ');
    const var2 = var1.map(original_capitalizeWord); // member call (with lambda arg) THIS ONE IS INTERESTING
    const var3 = var2.join(' ');
    return var3;
}

function varAssign_capitalizeWord(word) {
    const var1 = word.charAt(0); // member call (with args)
    const var2 = var1.toUpperCase(); // member call (with no args)
    const var3 = word.slice(1); // member call (with args)
    const var4 = var2 + var3; // binary operation
    return var4;
}

function capitalizeWords(sentence) {
    const words = capitalizeWords_split(sentence);;
    const result = mapReduce(words, capitalizeWord, capitalizeWords_join)
    return result;
}

function capitalizeWord(word) {
    return word.charAt(0).toUpperCase() + word.slice(1);
}

function capitalizeWords_split(sentence) {
    return sentence.split(' ');
}

function capitalizeWords_join(words) {
    return words.join(' ');
}

function mapReduce(xs, mapFn, reduceFn) {
    const mapped = xs.map(mapFn);
    const reduced = reduceFn(mapped);
    return reduced;
}

main();

