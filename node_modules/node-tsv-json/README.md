# node-tsv-json

[![Build Status](https://travis-ci.org/DataGarage/node-tsv-json.png?branch=master)](https://travis-ci.org/DataGarage/node-tsv-json)

convert tsv to json format.

## Install

```
  npm install node-tsv-json
```

## Usage

```javascript
  tsv = require("node-tsv-json");
  tsv({
    input: "sample.tsv", 
    output: "output.json"
    //array of arrays, 1st array is column names
    ,parseRows: true
  }, function(err, result) {
    if(err) {
      console.error(err);
    }else {
      console.log(result);

      //    [
      //      { 'date, 'close'},
      //      { '31-Jul-07', '131.76' },
      //      { '30-Jul-07', '141.43' },
      //      { '27-Jul-07', '143.85' },
      //      { '26-Jul-07', '146.00' },
      //      { '25-Jul-07', '137.26' },
      //      { '24-Jul-07', '134.89' },
      //      { '23-Jul-07', '143.70' },
      //      { '20-Jul-07', '143.75' }
      //    ]
    }
  });
```

In config object, you have to enter an input path. But If you don't want to output any file you can set to `null`.

Add `parseRows: true (optional)` to the config object to output an array of arrays where the first element is an array of column names followed by arrays of values. Like sample below:

```
[
    [ 'date', 'close'],
    [ '31-Jul-07', '131.76' ],
    [ '30-Jul-07', '141.43' ],
    [ '27-Jul-07', '143.85' ],
    [ '26-Jul-07', '146.00' ],
    [ '25-Jul-07', '137.26' ],
    [ '24-Jul-07', '134.89' ],
    [ '23-Jul-07', '143.70' ],
    [ '20-Jul-07', '143.75' ]
]
```

## License 

MIT [@chilijung](http://github.com/chilijung)
