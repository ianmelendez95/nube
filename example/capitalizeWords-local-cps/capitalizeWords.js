function main() {
    console.log(capitalizeWords("hello world"));
}

function capitalizeWords(sentence) {
    const words = sentence.split(' ');
    return words.map(capitalizeWord).join(' ');
}

function capitalizeWord(word) {
    return word.charAt(0).toUpperCase() + word.slice(1);
}

main();

