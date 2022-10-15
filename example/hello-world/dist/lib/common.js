function startService(app, port, handler) {
  app.get('/', (req, res) => {
    res.send(handler(req))
  })

  app.listen(port, () => {
    console.log(`Started service at ${port}`)
  })
}

module.exports = {
  startService
}
