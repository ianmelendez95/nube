const {
  getRandomInt,
  getRandomIntArray,
  testMergeSort,
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
      body: JSON.stringify(await mergeSort.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function mergeSort(arr)  {
  if (arr.length <= 1) {
    return arr
  } 

  let mid = Math.floor(arr.length / 2)

  return mergeSortPair(
    arr.slice(0, mid),
    arr.slice(mid, arr.length)
  )
}