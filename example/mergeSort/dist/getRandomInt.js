const {
  getRandomIntArray,
  testMergeSort,
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
      body: JSON.stringify(await getRandomInt.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function getRandomInt(min, max)  {
  return Math.floor(Math.random() * (max - min) + min); // The maximum is exclusive and the minimum is inclusive
}