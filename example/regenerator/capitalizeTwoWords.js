function* capitalizeTwoWords(string) {
  const words = string.split(' ');
  const cw1 = yield capitalizeWord(words[0]);
  const cw2 = yield capitalizeWord(words[1]);
  return cw1 + ' ' + cw2;
}

function* capitalizeWord(word) {
  return word[0].toUpperCase() + word.slice(1);
}
