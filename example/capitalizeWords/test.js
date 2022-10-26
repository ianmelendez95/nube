const { handler } = require('./capitalizeWords.js')

handler({ body: JSON.stringify(["hello there world"]) })
  .then(x => console.log("Result: ", x))
