const {
  getRandomIntArray,
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