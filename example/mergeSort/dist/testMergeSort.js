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
      body: JSON.stringify(await testMergeSort.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function testMergeSort(len, min, max)  {
  len = typeof len === 'undefined' ? 8 : len
  min = typeof min === 'undefined' ? 0 : min
  max = typeof max === 'undefined' ? 64 : max

  return mergeSort(await getRandomIntArray(len, min, max))
}

async function getRandomIntArray(len, min, max)  {
  const argsString = JSON.stringify(
    Array.from(arguments).slice(0, getRandomIntArray.length))

  const options = {
    hostname: process.env.AWS_GATEWAY_HOST,
    port: 443,
    path: "/getRandomIntArray",
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


async function getRandomInt(min, max)  {
  const argsString = JSON.stringify(
    Array.from(arguments).slice(0, getRandomInt.length))

  const options = {
    hostname: process.env.AWS_GATEWAY_HOST,
    port: 443,
    path: "/getRandomInt",
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


async function mergeSort(arr)  {
  const argsString = JSON.stringify(
    Array.from(arguments).slice(0, mergeSort.length))

  const options = {
    hostname: process.env.AWS_GATEWAY_HOST,
    port: 443,
    path: "/mergeSort",
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


async function mergeSortPair(arr1, arr2)  {
  const argsString = JSON.stringify(
    Array.from(arguments).slice(0, mergeSortPair.length))

  const options = {
    hostname: process.env.AWS_GATEWAY_HOST,
    port: 443,
    path: "/mergeSortPair",
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


