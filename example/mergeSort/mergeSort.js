async function testMergeSort(len, min, max) {
  len = typeof len === 'undefined' ? 8 : len
  min = typeof min === 'undefined' ? 0 : min
  max = typeof max === 'undefined' ? 64 : max

  return mergeSort(await getRandomIntArray(len, min, max))
}

async function getRandomIntArray(len, min, max) {
  let res = []

  for (let i = 0; i < len; i++) {
    res.push(getRandomInt(min, max))
  }

  return Promise.all(res)
}

async function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min) + min); // The maximum is exclusive and the minimum is inclusive
}

async function mergeSort(arr) {
  if (arr.length <= 1) {
    return arr
  } 

  let mid = Math.floor(arr.length / 2)

  return mergeSortPair(
    arr.slice(0, mid),
    arr.slice(mid, arr.length)
  )
}

async function mergeSortPair(arr1, arr2) {
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