const {
  testMergeSort,
  getRandomInt,
  mergeSort,
  mergeSortPair
} = require('proxies')

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
      body: JSON.stringify(await getRandomIntArray.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function getRandomIntArray(len, min, max)  {
  let res = []

  for (let i = 0; i < len; i++) {
    res.push(getRandomInt(min, max))
  }

  return Promise.all(res)
}