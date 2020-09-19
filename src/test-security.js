const security = require('./security');

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
(async () => {
  // download from s3
  let data = await security.downloadArray('test.txt');
  console.log(data);

  let users = security.parseArray(data);
  console.log(users);
})();