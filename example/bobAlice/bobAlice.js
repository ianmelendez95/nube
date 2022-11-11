async function makeBobAndAliceFriends() {
  return makeFriends(await getBob(), await getAlice())
}

async function getBob() {
  return {
    name: "Bob",
    age: 26,
    friends: []  // Bob has no friends :(
  }
}

async function getAlice() {
  return {
    name: "Alice",
    age: 27,
    friends: ["Kathy"]
  }
}

async function makeFriends(person1, person2) {
  person1.friends.push(person2.name)
  person2.friends.push(person1.name)
  return [person1, person2]
}