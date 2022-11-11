const https = require('https')
const { Buffer } = require('node:buffer')

exports.handler = async function(event) {
  try {
    let args = (typeof event.body === 'undefined' || event.body.trim().length === 0) 
      ? [] 
      : JSON.parse(event.body) 

    if (!Array.isArray(args)) {
      args = [args]
    }

    return {
      statusCode: 200,
      body: JSON.stringify(await makeBobAndAliceFriends.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function makeBobAndAliceFriends()  {
  return makeFriends(await getBob(), await getAlice())
}

async function getBob()  {
  const argsString = JSON.stringify(
    Array.from(arguments).slice(0, getBob.length))

  const options = {
    hostname: process.env.AWS_GATEWAY_HOST,
    port: 443,
    path: "/getBob",
    method: 'POST'
  };

  return new Promise((resolve, reject) => {
    const req = https.request(options, (res) => {
      let data = ''

      res.on('data', (chunk) => {
        data += chunk
      })

      res.on('end', () => {
        resolve(JSON.parse(data))
      })

      res.on('error', (e) => {
        console.error('Response Error: ', e)
        reject(e)
      })
    })

    req.setHeader('Content-Type', 'application/json')
    req.setHeader('Content-Length', Buffer.byteLength(argsString))

    req.on('error', (e) => {
      console.error('Request Error: ', e)
      reject(e)
    })

    req.end(argsString)
  })
}


async function getAlice()  {
  const argsString = JSON.stringify(
    Array.from(arguments).slice(0, getAlice.length))

  const options = {
    hostname: process.env.AWS_GATEWAY_HOST,
    port: 443,
    path: "/getAlice",
    method: 'POST'
  };

  return new Promise((resolve, reject) => {
    const req = https.request(options, (res) => {
      let data = ''

      res.on('data', (chunk) => {
        data += chunk
      })

      res.on('end', () => {
        resolve(JSON.parse(data))
      })

      res.on('error', (e) => {
        console.error('Response Error: ', e)
        reject(e)
      })
    })

    req.setHeader('Content-Type', 'application/json')
    req.setHeader('Content-Length', Buffer.byteLength(argsString))

    req.on('error', (e) => {
      console.error('Request Error: ', e)
      reject(e)
    })

    req.end(argsString)
  })
}


async function makeFriends(person1, person2)  {
  const argsString = JSON.stringify(
    Array.from(arguments).slice(0, makeFriends.length))

  const options = {
    hostname: process.env.AWS_GATEWAY_HOST,
    port: 443,
    path: "/makeFriends",
    method: 'POST'
  };

  return new Promise((resolve, reject) => {
    const req = https.request(options, (res) => {
      let data = ''

      res.on('data', (chunk) => {
        data += chunk
      })

      res.on('end', () => {
        resolve(JSON.parse(data))
      })

      res.on('error', (e) => {
        console.error('Response Error: ', e)
        reject(e)
      })
    })

    req.setHeader('Content-Type', 'application/json')
    req.setHeader('Content-Length', Buffer.byteLength(argsString))

    req.on('error', (e) => {
      console.error('Request Error: ', e)
      reject(e)
    })

    req.end(argsString)
  })
}


