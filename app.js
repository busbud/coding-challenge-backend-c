"use strict";

var http = require('http');
var port = process.env.PORT || 2345;
const host = process.env.HOST || '127.0.0.1'
const fs = require('fs');
const DB = require('./DB')
const Score = require('./Score');
const data = parseTsv('./data/cities_canada-usa.tsv')
DB.prototype.score = new Score
const db = new DB(data)


function parseTsv(path){
  const tsv = fs.readFileSync(path,'utf8');
  const rows = tsv.split('\n')
  const head = rows.reverse().pop().split('\t').map(i=>i.replace('\r',''))
  const data = []
  for(let row of rows){
    const parsed = row.split('\t')
    
    if(parsed.length == head.length){
      
      const organizedRow = {}
      for(let index in parsed){


        organizedRow[head[index]] = parsed[index].replace('\r','')

      }
    
      data.push(organizedRow)
    }
    
  }

  return data

}

function parseQueryString(str){
  const parts = str.split('&')
  const result = {}
  for(let part of parts){
    const [index,val] = part.split('=')
    result[index] = val
  }

  return result
}

const kmForPenalty = 100
const penalty = 0.01
module.exports = http.createServer(function (req, res) {

  if (req.url.indexOf('/suggestions') === 0) {
    const parsedUrl = req.url.split('?')
    const queryString = decodeURI(parsedUrl[1])
    if(parsedUrl.length < 2){
      res.writeHead(404, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({
        suggestions: []
      }));
      return
      
    }else{
      const query = parseQueryString(queryString)
      const suggestions = db.search(query)
      for(let index in suggestions){
            const row = suggestions[index]

            if(row.distance >= kmForPenalty){
              const newScore = row.score - (row.distance / kmForPenalty) * penalty

              row.score = newScore > 0 ? newScore : 0 
            }
      }
      res.writeHead(suggestions.length ? 200 : 404, {'Content-Type': 'application/json'});

      res.end(JSON.stringify({
        suggestions
      }));
    }

    
  } else {
    res.end();
  }
}).listen(port, host);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);