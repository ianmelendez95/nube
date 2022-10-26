const index = require('./index.js')

index.handler({ body: JSON.stringify(["hello there world"]) })
  .then(x => console.log("Result: ", x))
