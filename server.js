const path = require('path');
const fs = require('fs');
const express = require('express');


// instantiate express app
const app = express();
const port = process.env.port || 3000;

app.use(express.static(path.join(__dirname, 'client')));

// app.get('/', (req, res) => {
//   res.send('Hello World!')
// })

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})