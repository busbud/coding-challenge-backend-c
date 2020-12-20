# Heroku
GET Endpoint on Heroku: https://serene-tor-02821.herokuapp.com/suggestions

Sample request: https://serene-tor-02821.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

# Functional Tests
Inside of root folder, execute `npm test` to run the functional tests.

The Last test was changed because of the return format. The Regex was requiring the whole name "Montreal" while the wished request includes State and Country name.

# Project Design
With the purpose of isolating data manipulation from the business rules, the source code was divides in the following sections:

## Domain
Contains the business logic, like the score calc, and definition of input and output data like the query inputed via API and the Suggestions received on response.

## Repository
Contains the modules of data manipulation logic, loading the file and filtering the data according to parameters.
The suggestion-repo.js module also contains a canadaStatesDict, which contains a translation from the code contained in the file to the Canadias State codes.

## Error (domain)
A single module containing classes which extend the Error JavaScript class. The purpose is to create centralized custom errors, so the caller, in this case the API, will be able to properly handle those errors.

## Error Handling (API)
A single module containing a dictionary with the translation from error types and http error codes.
This module also contains a method which translates a custom error (defined on the error.js module), returning a the proper response with the corresponding http code.