const express = require('express')
const fetch = require('node-fetch')

const app = express()
const port = 3000

// main

async function main() {
  const response = await fetch(
    `http://localhost:3000/main`, 
    { method: "POST", body: JSON.stringify(arguments) }
  )

  return await response.json()
}

async function main_impl() {
  return capitalizeWords("hello, world!")
}

function main_setup() {
  const app = express()
  app.post('/main', (req, res) => {
    const args = req.body ? JSON.parse(req.body) : []
    const resultP = main_impl.apply(null, args)
    resultP
      .then(result => JSON.stringify(result))
      .then(result => res.send(result))
  })
  app.listen(3000)
}

main_setup()

async function capitalizeWords(string) {
  // const caps = await Promise.all(string.split(' ').map(capitalizeWord))
  // console.log("Caps: ", caps)
  return (await Promise.all(string.split(' ').map(capitalizeWord))).join(' ')
}

async function capitalizeWord(string) {
  return string[0].toUpperCase() + string.slice(1)
}