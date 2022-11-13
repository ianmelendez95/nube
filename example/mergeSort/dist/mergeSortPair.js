const {
  mergeSort,
  getRandomInt,
  getRandomIntArray,
  testMergeSort
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
      body: JSON.stringify(await mergeSortPair.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function mergeSortPair(arr1, arr2)  {
  const arr1Sorted = await mergeSort(arr1)
  const arr2Sorted = await mergeSort(arr2)

  let i1 = 0
  let i2 = 0

  let res = []

  while (i1 < arr1Sorted.length && i2 < arr2Sorted.length) {
    if (arr1Sorted[i1] <= arr2Sorted[i2]) {
      res.push(arr1Sorted[i1])
      i1++
    } else {
      res.push(arr2Sorted[i2])
      i2++
    }
  }

  if (i1 < arr1Sorted.length) {
    return res.concat(arr1Sorted.slice(i1))
  } else {
    return res.concat(arr2Sorted.slice(i2))
  }
}