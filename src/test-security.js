const security = require('./security');

(async () =>{
  arr = [
    {'username':'one', 'password': 'one'},
    {'username':'two', 'password': 'two'}
  ];
  
  let serialized = security.serializeArray(arr);
  console.log(serialized);
  
  security.uploadArray(serialized, 'test.txt');
})()
