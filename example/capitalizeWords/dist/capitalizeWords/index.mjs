import {
  capitalizeWord
} from 'proxies'

export const handler = async (event) => {
  try {
    let args = (typeof event.body === 'undefined' || event.body.trim().length === 0) 
      ? [] 
      : JSON.parse(event.body) 

    if (!Array.isArray(args)) {
      args = [args]
    }

    return {
      statusCode: 200,
      body: JSON.stringify(await capitalizeWords.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function capitalizeWords(string)  {
  const words = await Promise.all(string.split(' ').map(capitalizeWord))
  return words.join(' ')
}