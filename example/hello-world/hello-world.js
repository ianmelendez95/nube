async function main() {
  return capitalizeWords("hello, world!")
}

async function capitalizeWords(string) {
  // const caps = await Promise.all(string.split(' ').map(capitalizeWord))
  // console.log("Caps: ", caps)
  return (await Promise.all(string.split(' ').map(capitalizeWord))).join(' ')
}

async function capitalizeWord(string) {
  return string[0].toUpperCase() + string.slice(1)
}