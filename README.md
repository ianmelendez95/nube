# nube

## Summary

`nube` is your new favorite, _absurdly_ cloud native, JavaScript compiler.

- Every single function an API service.
- Every single function call an HTTP request.
- Every unit of your program infinitely scalable.

`nube` provides this for free! [1] Turn your JavaScript file into a complete AWS CloudFormation stage, with each function as a standalone ready-to-use API endpoint, and all of your functions talking to each other over the WWW, just as Tim Berners-Lee intended!

[1] Fine Print: So long as you write in soberingly limited subset of JS, can deal with poor debugging support, and 
have a personal favor from Jeff Bezos to give your account the concurrency quota for an entire region. But if you have that,
it's practically a free lunch!

## Usage

Start with a JavaScript file that contains only asynchronous functions [(why async?)](https://github.com/ianmelendez95/nube/blob/master/README.md#why-only-asynchronous-functions).

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

The AWS CDK is only _marginally_ better than my one man side project, but it 
is undeniably the better choice. Go use it. It's great. The devs did an excellent job 
making my project irrelevant before it ever begun.

### Install Haskell Stack and Build From Source

Obtain the Haskell `stack` build system. Either through 
your preferred OS package manager (recommended), or by direct installation [here](https://docs.haskellstack.org/en/stable/#how-to-install-stack) if not available.

With `stack` installed simply go into the project and run `stack build`.

    $ git clone git@github.com:ianmelendez95/nube.git
    $ cd nube
    $ stack build
    
You can then run the compiler from within the project using `stack exec`

    $ stack exec nube -- capitalizeWords.js
    
You could also install the executable into your `.local/bin` file
so that it is available as a command.

    $ stack install
    Copying from .stack-work/install/aarch64-osx/aa684d49679b5194babb03d60b5f393f45d0337ee2e131e678431f8ccc74050b/9.0.2/bin/nube to /Users/ianmelendez/.local/bin/nube

    Copied executables to ~/.local/bin:
    - nube
    
    $ nube capitalizeWords.js

### Install AWS CLI 

In order to actually use the compiler output and spin up on the cloud, 
you'll need a working AWS CLI available on your machine. 
Follow instructions [here](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html) 
to get that set up.

It goes without saying that you'll need an AWS account to setup the CLI with.
The good news is an AWS account is (frighteningly) easy to set up.

## How it Works

### The Handler

Here's a JavaScript function.

    async function capitalizeWord(word) {
      return word[0].toUpperCase() + word.slice(1)
    }
    
Here's what it could look like with a NodeJS AWS Lambda 'handler', 
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
Lambda function! Check out the actual compiler output of such a handler [here](https://github.com/ianmelendez95/nube/blob/master/example/capitalizeWords/dist/capitalizeWord.js).

### The Proxy

"But wait!" you say. "What if my function calls other functions?" I swear, you people
are never content... Well if we must, we need a way to replace our functions with the equivalent
HTTP call to the Lambda function. We'll call this replacement the 'proxy' to the actual function
in _the cloud_.

Lets say you had the following function that calls your `capitalizeWord` function to 
capitalize all the words in a sentence.

    async function capitalizeWords(string) {
      const words = await Promise.all(string.split(' ').map(capitalizeWord)) // note the call to capitalizeWord here
      return words.join(' ')
    }
    
Further, let's say you deployed your `capitalizeWord` handler script as an AWS Lambda function,
integrated with an AWS HTTP API Gateway, available at the url `https://a1b2c3d4.execute-api.us-east-2.amazonaws.com/capitalizeWord`.
Your `capitalizeWord` function could look like this.

    async function capitalizeWord(string) {
      // Stringify the function arguments to an arguments array
      const argsString = JSON.stringify([string])

      // Send the POST HTTP request on the endpoint, with the stringified arguments in the body
      const response = await fetch('https://a1b2c3d4.execute-api.us-east-2.amazonaws.com/capitalizeWord', {
        method: 'POST',
        body: argsString,
        headers: {'Content-Type': 'application/json'}
      })

      // Retrieve the response as JSON and return the result
      const data = await response.json()
      return data
    }
    
Your `capitalizeWords` handler now has a proxy to `capitalizeWord` ready to use, and 
the handler is none the wiser that the function is actually an HTTP call, since the 
function signature was left intact. For examples of these proxies, see 
[here](https://github.com/ianmelendez95/nube/blob/master/example/capitalizeWords/dist/nodejs/node_modules/proxies.js).
Their implementation is fairly different (uses Node's builtin `https` package, has primitive retry logic, etc.) 
but at the core they are doing the same thing, acting as the 'real' function but delegating 
to the API.

### The CloudFormation

Ok so now what, we have the ingredients, it's all ready to bake right? How does this actually get 
deployed to _the cloud_?

Well we could just use the AWS CLI to create our infrastructure piecemeal, but the benevolent
powers that be at Amazon gave us the power of CloudFormation templates.

For the sake of brevity, there will just be a listing of the resources involved in the template,
and their relationships. For a full example of such a template see [here](https://github.com/ianmelendez95/nube/blob/master/example/capitalizeWords/dist/capitalizeWords-template.json).

| Resource Name                  | Resource Type           | Description                                                                         |
| ------------------------------ | ----------------------- | ----------------------------------------------------------------------------------- |
| `CapitalizeWordsBucket`        | Parameter               | Technically not a resource, but the bucket containing our code, template, and layer |
| `CapitalizeWord(s)Lambda`      | Lambda Function         | The Lambda function handlers for our `capitalizeWord(s)` functions |
| `CapitalizeWordsLayer`         | Lambda LayerVersion     | The shared layer containing our `proxies` module used by our Lambda function handlers | 
| `CapitalizeWordsApi`           | API Gateway             | The API Gateway that will forward HTTP requests to our Lambda functions |
| `CapitalizeWordsApiStage`      | API Gateway Stage       | The API Gateway stage that is configured to auto deploy our API routes | 
| `CapitalizeWord(s)Integration` | API Gateway Integration | The integrations that inform our API about the lambda functions |
| `CapitalizeWord(s)Route`       | API Gateway Route       | The routes that direct POST requests to our API integrated Lambdas by path |
| `CapitalizeWord(s)Permission`  | Lambda Permission       | The permission that authorizes our API Gateway to forward requests to the lambda functions | `CapitalizeWordsRole`          | IAM Role                | The role assigned to the Lambda functions authorizing the basic use of the service |

### The Deployment Script

Finally you need to kick off the whole process. Essentially all we need to do 
is setup the bucket the CloudFormation file is expecting with all of the relevant
files. For our `capitalizeWords.js` example, the following files are uploaded to 
the S3 `capitalizewords-bucket` bucket.

* `capitalizeWord(s)-code.zip`    - The ZIP files containing the Lambda handler scripts respectively.
* `capitalizeWords-layer.zip`     - The ZIP file containing the shared Lambda layer proxies module.
* `capitalizeWords-template.json` - The CloudFormation template file to setup the whole thing.

With the S3 bucket loaded up with all of the relevant files, a painless call to 
`aws cloudformation create-stack` pointing to the `capitalizeWords-template.json` 
bucket file, and less than a minute later a fully functional cloud platform 
pops into existence, all from a simple JavaScript file!

An example of such a deployment script can be seen [here](https://github.com/ianmelendez95/nube/blob/master/example/capitalizeWords/dist/deploy.sh)

## FAQ

### Why only Asynchronous Functions?

Because they are _much_ easier to convert to a proxy function.
A proxy function is where we replace a function in the script, such as the following

    async function capitalizeWord(word) {
      return word[0].toUpperCase() + word.slice(1)
    }
    
With one that makes an HTTP request to the same function that is available via 
API like so

    async function capitalizeWord(word) {
      const response = await fetch('https://21691ba1lf.execute-api.us-east-2.amazonaws.com/capitalizeWord', {
        method: 'POST',
        body: JSON.stringify([word]),
        headers: {'Content-Type': 'application/json'}
      })

      return response.json()
    }
    
Notice how the function signature `async function capitalizeWord(word)` didn't change.
It would be far more difficult to make synchronous functions work as asynchronous HTTP proxies.
Take for example the following script.

    function capitalizeWords(string) {
      return string.split(' ').map(capitalizeWord).join(' ')
    }

    function capitalizeWord(word) {
      return word[0].toUpperCase() + word.slice(1)
    }

If we simply make `capitalizeWord` async, `capitalizeWords` would fail as we would be calling `.join(' ')` on an 
array of `Promises`. We would need to parse the body as an AST and identify all function calls, and convert them to their
async/await equivalent. Notice that our compiler would have to be smart enough to identify that 
`arr.map(capitalizeWord)` is equivalent to `Promise.all(arr.map(capitalizeWord))` and not just `await arr.map(capitalizeWord)`.
Further, notice that we assume all calls are to async functions, since discriminating would be an extra complication.

    async function capitalizeWords(string) {
      const res1 = await string.split(' ')
      const res2 = await Promise.all(res1.map(capitalizeWord))
      const res3 = await res2.join(' ')
      return res3
    }

Ultimately, compiling synchronous functions to asynchronous functions would be a worthwhile project all on its own, 
and proved to be a significant undertaking that wouldn't highlight the process of compiling a simple JS file to a fully 
'cloud native' API platform.

### Why? Just, why?

This is basically parody purely for my own entertainment and a learning vehicle for 
AWS.

It very much works (_when_ it works, `${READER_DEITY}` help you if you have any errors).
Starting as a cheeky dig at the microservices craze, it's an illustration of the obsession taken to an extreme. 
As I worked on the concept it turned out to be surprisingly fun to interact with AWS 
(hats off to the Amazon teams that make it all a reality!).
One thing led to another, and I found myself fleshing out a full blown MVP.
This is despite being fully prepared to let it stay a goofy idea, pitched to a group of 
friends on what must have been an uneventful Tuesday afternoon.

Here is where I say I'm not just a luddite that doesn't appreciate the benefits of the 
cloud. I am well aware of the days when _function calls_ were considered an abhorrent 
disrespect of the CPU, to be avoided at all costs. Noone is suggesting 
each source function should be a microservice (nanoservice?), though should such people exist they are encouraged
to use `nube` to see why that is a thoroughly painful idea.

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
  - [ ] OR converted to asynchronous form (major undertaking)
