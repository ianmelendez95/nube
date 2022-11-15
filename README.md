# nube

## Summary

`nube` is your new favorite, _absurdly_ cloud native, JavaScript compiler.

- Every single function becomes a new API service.
- Every single function call becomes an HTTP request.
- Every unit of your program becomes infinitely scalable.

`nube` provides this for free! [1] Turn your JavaScript file into a complete AWS CloudFormation stage, with each function as a standalone ready-to-use API endpoint, and all of your functions talking to each other over the WWW, just as Tim Berners-Lee intended!

[1] Fine Print: So long as you write in soberingly limited subset of JS, can deal with poor debugging support, and 
have a personal favor from Jeff Bezos to give your account the concurrency quota for an entire region. But if you have that,
it's practically a free lunch!

## Usage

Start with a JavaScript file that contains only asynchronous functions [(why async?)](https://github.com/ianmelendez95/nube/edit/master/README.md#why-only-asynchronous-functions).

    $ cat capitalizeWords.js
    async function capitalizeWords(string) {
      const words = await Promise.all(string.split(' ').map(capitalizeWord))
      return words.join(' ')
    }

    async function capitalizeWord(word) {
      return word[0].toUpperCase() + word.slice(1)
    }
    
Compile this file with the nube compiler.

    $ nube capitalizeWords.js

Next to the source file will be a new `/dist` directory containing the contents of your new cloud native platform.

    $ tree .
    .
    ├── capitalizeWords.js
    └── dist
        ├── capitalizeWord.js               # the lambda function handler for the 'capitalizeWord' function
        ├── capitalizeWords-template.json   # the CloudFormation template file for the entire stack
        ├── capitalizeWords.js              # the lambda function handler for the 'capitalizeWords' function
        ├── deploy.sh                       # the deployment script to create the CloudFormation stack
        └── nodejs                          # the shared layer contents between the lambda function (just contains proxies.js module)
            └── node_modules
                └── proxies.js                # the proxies.js module, which contains the proxy functions that call all the lambda functions
                
Change directory into the `/dist` directory. Read the `deploy.sh` file, verify you are comfortable with its commands, and if so enable execute permissions and run the file.

    $ cd dist
    $ chmod 700 deploy.sh
    $ ./deploy.sh 
    upload: ./capitalizeWords-template.json to s3://capitalizewords-bucket/capitalizeWords-template.json
    packaging: capitalizeWords-layer.zip
      adding: nodejs/ (stored 0%)
    ...
    uploading: capitalizeWord-code.zip s3://capitalizewords-bucket
    upload: ./capitalizeWord-code.zip to s3://capitalizewords-bucket/capitalizeWord-code.zip
    creating stack: capitalizeWords-template.json

After deployment, check your AWS CloudFormation dashboard, and wait until your new `capitalizeWords-stack` is setup and ready to go!
You will now be able to use each of these functions as a standalone API endpoint! Be sure to replace your generated API Gateway API ID 
where you see `${API_ID}`, and your default region where you see `${AWS_REGION}` in the following `curl` examples. To get your default region
either identify it on your AWS console or run `aws configure get region` on the command line.

    $ curl -d '["hello there world!"]' \ 
    >   -H 'Content-Type: application/json' \
    >   https://${API_ID}.execute-api.${AWS_REGION}.amazonaws.com/capitalizeWords
    "Hello There World!"
    
    $ curl -d '["hello"]' \ 
    >   -H 'Content-Type: application/json' \
    >   https://${API_ID}.execute-api.${AWS_REGION}.amazonaws.com/capitalizeWord
    "Hello"
    
## Installation

Ok, you've made it this far, it's time for a disclaimer:

This is lowbrow parody. You should not use it for anything other 
than entertainment. It goes without saying you deploy to your account
at your own risk!

It very much works (when it works, ${READER_DEITY} help you if you have any errors).
However, it was built as a cheeky parody of the microservices craze, 
illustrating the obsession taken to an extreme.

Here is where I say I'm not just some luddite that doesn't appreciate the benefits of the 
cloud. I am well aware of the days when _function calls_ were considered an abhorrent 
disrespect of the CPU, to be avoided at all costs.

## FAQ



### Please Describe, in Excruciating Detail, how nube works?

_Excruciating detail_? Wow, I don't know how you all have time for that but I don't, 
so here's the quick tour.

Here's a JavaScript function.

    async function capitalizeWord(word) {
      return word[0].toUpperCase() + word.slice(1)
    }
    
Here's what it could look like as a NodeJS AWS Lambda 'handler', 
as an example. This handler receives what will be the POST body
as `event.body`. We assume the POST body is a valid JSON array of 
arguments to our `capitalizeWord` function.

    exports.handler = async function(event) {
      // Parse the arguments from the request body as a JSON array
      const args = JSON.parse(event.body)
      
      // Call the function with these arguments
      const functionResult = await capitalizeWord.apply(null, args)
      
      // Stringify the result
      const functionResultString = JSON.stringify(functionResult)
      
      // Return the result as a minimal HTTP response
      return { statusCode: 200, body: functionResultString }
    }
    
And there you have it. A fully working AWS Lambda handler file ready to be turned into a
Lambda function!

"But wait!" you say. "What if my function calls other functions?" I swear, you people
are never content... Well if we must, we need a way to replace our functions with the equivalent
HTTP call to the Lambda function. 

[TODO - continue proxy description]

### Why? Just, why?

This is lowbrow parody. You should not use it for anything other 
than entertainment. It goes without saying you deploy to your account
at your own risk!

It very much works (when it works, ${READER_DEITY} help you if you have any errors).
However, it started as a cheeky parody of the microservices craze, 
illustrating the obsession taken to an extreme. 
As I worked on the concept I found it to be surprisingly fun to interact with AWS 
(hats off to the Amazon teams that make it all a reality!).
One thing led to another, and I found myself fleshing out a full blown MVP, 
despite being fully prepared to let it stay a goofy idea pitched to a group of 
friends on what must have been an otherwise uneventful Tuesday afternoon.

Here is where I say I'm not just a luddite that doesn't appreciate the benefits of the 
cloud. I am well aware of the days when _function calls_ were considered an abhorrent 
disrespect of the CPU, to be avoided at all costs. Noone is suggesting 
each source function should be a microservice (nanoservice?), though should they exist I will 
gladly encourage them to use `nube` to see why that is a thoroughly painful idea.

### Why only Asynchronous Functions?

[TODO]

## Room for Improvement

- [x] proxies in a shared layer
- [x] implement retry in proxy functions
- [ ] add random string to bucket name (help avoid conflicts)
- [ ] error handling 
  - [ ] javascript errors could be caught and line numbers returned instead of a generic 500
  - [ ] source line number mappings
  - [ ] request handlers check argument count
- [ ] support for synchronous functions - one of the following:
  - [ ] included inline in each lambda function (could be in the shared layer)
  - [ ] OR converted to asynchronous form (major undertaking compilation to [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style))
