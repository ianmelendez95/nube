const express = require('express')
const fetch = require('node-fetch')

// main

async function main() {
  const response = await fetch(
    "http://localhost:3000/main", 
    { 
      method: "post", 
      body: JSON.stringify([]),
      headers: { 'Content-Type': 'application/json' } 
    }
  )

  return await response.json()
}

function main_setup(app) {
  app.post('/main', (req, res) => {
    const args = req.body
    const resultP = main_impl.apply(null, args)
    resultP.then(result => res.send(JSON.stringify(result)))
  })
}

async function main_impl() {
  return capitalizeWords("hello, world!")
}

// capitalizeWords

async function capitalizeWords(string) {
  const response = await fetch(
    `http://localhost:3000/capitalizeWords`, 
    { 
      method: "post", 
      body: JSON.stringify([ string ]), 
      headers: { 'Content-Type': 'application/json' } 
    }
  )

  return await response.json()
}

function capitalizeWords_setup(app) {
  app.post('/capitalizeWords', (req, res) => {
    const args = req.body
    const resultP = capitalizeWords_impl.apply(null, args)
    resultP.then(result => { res.send(JSON.stringify(result)) })
  })
}

async function capitalizeWords_impl(string) {
  return (await Promise.all(string.split(' ').map(capitalizeWord))).join(' ')
}

// capitalizeWord

async function capitalizeWord(string) {
  return string[0].toUpperCase() + string.slice(1)
}

// runtime setup

const _setup_functions = [
  main_setup,
  capitalizeWords_setup
]

const app = express()

app.use(express.json())

for (let setup_function of _setup_functions) {
  setup_function(app)
}

app.listen(3000, async () => {
  const result = await main() 
  if (typeof result != 'undefined') {
    console.log(result)
  }
})