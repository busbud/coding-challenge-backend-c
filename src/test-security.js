const security = require('./security');

(async () => {
  // download from s3
  let data1 = await security.downloadFile('users.txt');
  let data2 = await security.downloadFile('ips.txt');

  console.log(data1.Body.toString('binary'));
  console.log(data2);
})()