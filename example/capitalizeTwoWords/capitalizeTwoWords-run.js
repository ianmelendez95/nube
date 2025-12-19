function capitalizeTwoWords(string) {
    const words = string.split(' ');
    const word1 = words[0];
    const word2 = words[1];
    const capitalizedWord1 = capitalizeWord(word1);
    const capitalizedWord2 = capitalizeWord(word2);
    return capitalizedWord1 + ' ' + capitalizedWord2;
}

function capitalizeWord(word) {
    return word[0].toUpperCase() + word.slice(1);
}

console.log(capitalizeTwoWords("hello world"))
