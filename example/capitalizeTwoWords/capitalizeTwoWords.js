async function capitalizeTwoWords(string) {
    const words = string.split(' ');
    const word1 = words[0];
    const word2 = words[1];
    const capitalizedWord1 = await capitalizeWord(word1);
    const capitalizedWord2 = await capitalizeWord(word2);
    return capitalizedWord1 + ' ' + capitalizedWord2;
}

async function capitalizeWord(word) {
    return word[0].toUpperCase() + word.slice(1);
}