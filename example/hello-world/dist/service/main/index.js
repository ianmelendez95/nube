const express = require('express')
const app = express()
const name = "main"
const port = 3000

app.get('/', (req, res) => {
  const result = main()
  res.send(result)
})

app.listen(port, () => {
  console.log(`${name}: ${port}`)
})

function main() {
  return capitalizeWords("hello, world!")
}

function capitalizeWords(string) {
  return string.split(' ').map(capitalizeWord).join(' ')
}

function capitalizeWord(word) {
  return word[0].toUpperCase() + word.slice(1)
}