async function capitalizeWords(string) {
  const words = await Promise.all(string.split(' ').map(capitalizeWord))
  return words.join(' ')
}

async function capitalizeWord(word) {
  return word[0].toUpperCase() + word.slice(1)
}