const { handler } = require('./dist/capitalizeWords.js')

handler({ body: JSON.stringify(["hello there world"]) })
  .then(x => console.log("Result: ", x))
