# nube

## Room for Improvement

- [x] proxies in a shared layer
- [ ] implement retry in proxy functions
- [ ] add random string to bucket name (help avoid conflicts)
- [ ] error handling 
  - [ ] javascript errors could be caught and line numbers returned instead of a generic 500
  - [ ] source line number mappings
  - [ ] request handlers check argument count
- [ ] support for synchronous functions - one of the following:
  - [ ] included inline in each lambda function (could be in the shared layer)
  - [ ] OR converted to asynchronous form (major undertaking compilation to [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style))
