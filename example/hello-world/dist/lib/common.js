const express = require('express')
const app = express()
const name = "main"
const port = 3000

function startService(port, handler) {
  app.get('/', (req, res) => {
    res.send(handler(req))
  })

  app.listen(port, () => {
    console.log(`${name}: ${port}`)
  })
}
