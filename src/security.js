const fs = require('fs')

// write an array with object elements to file, use with readArray
const writeArray = (arr, filePath) => {
  const serialized = arr.reduce((accumulated, current) => {
    return accumulated += JSON.stringify(current) + '\n'
  }, "");

  fs.writeFileSync(filePath, serialized);
};

// read a file and return an array of objects, use with writeArray
// if file cannot be found, return an empty array
const readArray = (filePath) => {
  try {
    const str = fs.readFileSync(filePath, {encoding: 'utf-8'});
    const arr = str.split("\n")
      .filter(el => el !== '')
      .map(el => JSON.parse(el));
    return arr;
  } catch (err) {
    return [];
  }
};

// check if user has an jwt access cookie set
module.exports.validateUser = () => {

};