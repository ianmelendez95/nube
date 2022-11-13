const {
  getAlice,
  getBob,
  makeBobAndAliceFriends
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
      body: JSON.stringify(await makeFriends.apply(null, args), null, 2)
    }
  } catch (e) {
    console.log(e)
    return e
  }
}

async function makeFriends(person1, person2)  {
  person1.friends.push(person2.name)
  person2.friends.push(person1.name)
  return [person1, person2]
}