const security = require('./security');

(async () => {
  // download from s3
  let data1 = await security.downloadFile('users.txt');
  let data2 = await security.downloadFile('ips.txt');

  let users = security.parseArray(data1);
  let ips = security.parseArray(data2);

  console.log(users);
  console.log(ips);
})()