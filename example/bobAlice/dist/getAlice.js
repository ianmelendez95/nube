const {
  getBob,
  makeBobAndAliceFriends,
  makeFriends
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
      body: JSON.stringify(await getAlice.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function getAlice()  {
  return {
    name: "Alice",
    age: 27,
    friends: ["Kathy"]
  }
}