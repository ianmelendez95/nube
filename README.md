# nube

## Usage

Start with a JavaScript file that contains only asynchronous functions. [(why?)](https://github.com/ianmelendez95/nube/edit/master/README.md#why-only-asynchronous-functions)

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
    upload: ./mergeSort-template.json to s3://mergesort-bucket/mergeSort-template.json
    packaging: mergeSort-layer.zip
      adding: nodejs/ (stored 0%)
    ...
    uploading: mergeSortPair-code.zip s3://mergesort-bucket
    upload: ./mergeSortPair-code.zip to s3://mergesort-bucket/mergeSortPair-code.zip
    creating stack: mergeSort-template.json

After deployment, check your AWS CloudFormation dashboard, and wait until your new `capitalizeWords-stack` is setup and ready to go!
You will now be able to use each of these functions as a standalone API endpoint! (Be sure to replace your generated API Gateway API ID 
where you see `${API_ID}` in the following `curl` examples)

    $ curl -d '["hello there world!"]' \ 
    >   -H 'Content-Type: application/json' \
    >   https://${API_ID}.execute-api.us-east-2.amazonaws.com/capitalizeWords
    "Hello There World!"
    
    $ curl -d '["hello"]' \ 
    >   -H 'Content-Type: application/json' \
    >   https://${API_ID}.execute-api.us-east-2.amazonaws.com/capitalizeWord
    "Hello"

## FAQ

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
