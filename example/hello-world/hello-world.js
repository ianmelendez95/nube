function main() {
  return capitalizeWords("hello, world!")
}

function capitalizeWords(string) {
  return string.split(' ').map(capitalizeWord)
}

function capitalizeWord(string) {
  return s[0].toUpperCase() + s.slice(1)
}