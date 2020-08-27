const fs = require('fs')

// write an array with object elements to file
const writeArray = (arr, filePath) => {
  const serialized = arr.reduce((accumulated, current) => {
    return accumulated += JSON.stringify(current) + '\n'
  }, "");

  fs.writeFileSync(filePath, serialized);
};

// test
writeArray([{user: "usr1", password: "pw1"}, {user: "usr2", password:"pw2"}], "./data/users.txt");

// check if user has an jwt access cookie set
module.exports.validateUser = () => {

};