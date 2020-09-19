const security = require('../src/security');

// upload data
(async () => {
  let users = [
    {'username':'one', 'password':'one'},
    {'username':'two', 'password':'two'}
  ];

  let data = security.serializeArray(users);
  console.log(data);

  await security.uploadArray(data, 'test.txt');
})();

// download data
let users = null;
const getUsers  = async (users) => {
  // download from s3
  let data = await security.downloadArray('test.txt');
  console.log(data);

  users = security.parseArray(data);
};

getUsers(users);
console.log(users);