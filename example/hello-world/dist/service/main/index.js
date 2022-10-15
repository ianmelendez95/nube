const express = require('express')
const common = require('./lib/common')

const app = express()
const port = 3000
const handler = main

common.startService(app, port, handler)

function main() {
  return capitalizeWords("hello, world!")
}

function capitalizeWords(string) {
  return string.split(' ').map(capitalizeWord).join(' ')
}

function capitalizeWord(word) {
  return word[0].toUpperCase() + word.slice(1)
}