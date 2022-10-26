const https = require('https')
const { Buffer } = require('node:buffer')

_gateway_host = "3fry8db278.execute-api.us-east-2.amazonaws.com"

exports.handler = async function(event) {
  try {
    return {
      statusCode: 200,
      body: JSON.stringify(await capitalizeWords.apply(null, JSON.parse(event.body)))
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function capitalizeWords(string) {
  const words = await Promise.all(string.split(' ').map(capitalizeWord))
  return words.join(' ')
}

async function capitalizeWord(word) {
  const argsString = JSON.stringify(
    Array.from(arguments).slice(0, capitalizeWord.length))

  const options = {
    hostname: _gateway_host,
    port: 443,
    path: "/capitalizeWord",
    method: 'POST'
  };

  return new Promise((resolve, reject) => {
    const req = https.request(options, (res) => {
      let data = ""

      res.on('data', (chunk) => {
        data += chunk
      })

      res.on('end', () => {
        resolve(JSON.parse(data))
      })

      res.on('error', (e) => {
        console.error("Response Error: ", e)
        reject(e)
      })
    })

    req.setHeader('Content-Type', 'application/json')
    req.setHeader('Content-Length', Buffer.byteLength(argsString))

    req.on('error', (e) => {
      console.error("Request Error: ", e)
      reject(e)
    })

    console.log("Writing: '" + argsString + "'")
    req.end(argsString)
  })
}