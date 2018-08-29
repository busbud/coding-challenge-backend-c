# Tsvert
Tsvert is an Open Source Software Library to convert TSV to various data formats which is JSON and SQL insert statement and popular programming languages(PHP, Python, Ruby) array associative written in JavaScript.
Tsvert is supposed to work in both Node.js and browser environments.

[![npm version](https://badge.fury.io/js/tsvert.svg)](https://badge.fury.io/js/tsvert)

## Demo
* [Tsvert(web service)](http://tsvert.high5.science/)

## Installation
```
npm install tsvert --save
```

## Usage (just one example)

sample tsv data which copied from google spreadsheet(or excel)　
[data.tsv](https://docs.google.com/spreadsheets/d/18hi71QjLI1_Odpi1bB3HpeMAo6LgvKluLBafBZJRINc/edit#gid=0)
```
id	name	type	price
1001	egg	food	200
1002	hook	dvd	999
1003	hammer	tool	300
1004	すし	食べ物	900
```
※Header in the first row is what you have specified the key of each line data.

Use the tsvert, if you specify the json as the conversion type, it is possible to convert the tsv data of the target to json.

Minimal usage:
```JavaScript
var fs = require('fs')
  , tsvert = require('tsvert');
var tsv = fs.readFileSync('data.tsv', 'utf8');

tsvert.setOptions({
  "indent":true  
});

var json = tsvert(tsv, 'json');
console.log(json);
```
result:
```JavaScript
[
  {
    "id": 1001,
    "name": "egg",
    "type": "food",
    "price": 200
  },
  {
    "id": 1002,
    "name": "hook",
    "type": "dvd",
    "price": 999
  },
  {
    "id": 1003,
    "name": "hammer",
    "type": "tool",
    "price": 300
  },
  {
    "id": 1004,
    "name": "すし",
    "type": "食べ物",
    "price": 999
  }
];
```


## Options
You can specify a number of options in tsvert.  
Example setting options with default values:
```JavaScript
tsvert.defaults = {
  html: false,
  indent: false,
  sqlBulkInsert: false,
  sqlTableName: '',
  rubySymbolKey: true,
  useRowNumberKey: false,
  header: ''
};
```

The explanation for each of the options.

### html
This option uses the `<br>` tag without using the `\ n` to the new line code for output of browser.    
By default it is false.     
Following is a result of html option(setting true):
```JavaScript
[<br>
  {"id": 1001, "name": "egg", "type": "food", "price": 200},<br>
  {"id": 1002, "name": "hook", "type": "dvd", "price": 999}<br>
];<br>
```

### indent
This option outputs the indent the data of the target to the deeper layers.      
By default it is false.      
Following is a result of indent option(setting false):
```JavaScript
[
  {"id": 1001, "name": "egg", "type": "food", "price": 200},
  {"id": 1002, "name": "hook", "type": "dvd", "price": 999}
];
```
Following is a result of indent option(setting true):
```JavaScript
[
  {
"id": 1001,
    "name": "egg",
    "type": "food",
    "price": 200
  },
  {
"id": 1002,
    "name": "hook",
    "type": "dvd",
    "price": 999
  }
];
```

### sqlBulkInsert
sqlBulkInsert option is an option to output together with the sql insert statement to bulk insert format to one of the sql statement.    
It can be used only in SQL.
By default it is false.  
Following is a result of sqlBulkInsert option(setting false):
```sql
INSERT INTO table(id, name, type, price) VALUES(1001, 'egg', 'food', 200);
INSERT INTO table(id, name, type, price) VALUES(1002, 'hook', 'dvd', 999);
```
Following is a result of sqlBulkInsert option(setting true):
```sql
INSERT INTO table(id, name, type, price) VALUES(1001, 'egg', 'food', 200)
,(header1, header2, header3, header4, header5, header6:str) VALUES(8882, 8882, 8882, '8882', 8882, 8882);
```

### sqlTableName
In sqlTableName option it allows you to specify the table name when output the sql insert statement.  
It can be used only in SQL.  
How to specify the table name
```JavaScript
var tsvert = require('tsvert');

tsvert.setOptions({
  "sqlTableName": "products"  
});
```
If you specify the "products" as the table name, following is a result;
```sql
INSERT INTO products(id, name, type, price) VALUES(1001, 'egg', 'food', 200);
INSERT INTO products(id, name, type, price) VALUES(1002, 'hook', 'dvd', 999);
```

### rubySymbolKey
If you want to output an associative array of ruby, the key of the associative array has become a symbol.  
By specifying the rubySymbolKey option to false, it can be a string key.  
It can be used only in Ruby.  
By default it is true.  
Following is a result of rubySymbolKey option(setting true):
```ruby
[
  {:id => 1001, :name => "egg", :type => "food", :price => 200},
  {:id => 1002, :name => "hook", :type => "dvd", :price => 900}
]
```
Following is a result of rubySymbolKey option(setting false):
```ruby
  {"id" => 1001, "name" => "egg", "type" => "food", "price" => 200},
  {"id" => 1002, "name" => "hook", "type" => "dvd", "price" => 999}
]
```
### useRowNumberKey
useRowNumberKey option is an option to put a number key to the associative array of each line.  
By default it is false.  
Following is a result of useRowNumberKey option(setting true):
```JavaScript
[
  "1": {"id": 1001, "name": "egg", "type": "food", "price": 200},
  "2": {"id": 1002, "name": "hook", "type": "dvd", "price": 999}
];
```

### header
header option is the option to specify from the program rather than the data the row header of the tsv data to be specified as a key for each column data.
For example, let's say there is a following like tsv data.
```
1001	egg	food	200
1002	hook	dvd	999
```

In the following manner to specify the header.
Tab if it is specified in the string must be a "\t".
```JavaScript
tsvert.setOptions({
  "header": "id\tname\ttype\tprice"
});
```
Following is a result of header option:
```JavaScript
[
  {"id": 1001, "name": "egg", "type": "food", "price": 200},
  {"id": 1002, "name": "hook", "type": "dvd", "price": 999}
];
```

### Option correspondence table
Depending on the type of data format to be converted, different options that you can use.  
The following is option correspondence table.

|       | html |indent|header|useRowNumberKey|sqlBulkInsert|sqlTableName|rubySymbolKey|header|
| :---  | :--: |:---: |  :-: | :------------:|:-----------:|:----------:|:-----------:|:----:|
| JSON  |  〇  |  〇  |  〇  |      〇        | ×           |×           |×            |×     |
| SQL   |  〇  |  〇  |  〇  |×               |〇           | 〇         |×            |×     |
| PHP   |  〇  |  〇  |  〇  |〇              |×            |×           |×            |×     |
| Python|  〇  |  〇  |  〇  |〇              |×            |×           |×            |×     |
| Ruby  |  〇  |  〇  |  〇  |〇              |×            |×           |〇           | ×    |


## :type
:type trees line header - which specifies the format of the data by adding to.
It determines whether enclosed in quotes by the contents of the normal data in tsvert.  
It is not enclosed in quotation marks if a number, do not enclose it in quotes If it is not numeric.  If the type of the specified, you can control it and whether enclosed in quotes.   
Type can be specified by adding, separated by a colon (:) at the end of the key.   
In the following example, name to str, and specifies the num in price.  
```
id	name:str	type	price:num
1001	egg	food	200
1002	hook	dvd	999
1003	8214	tool	null
```

The following results by the above specified I obtained.
```JavaScript
[
  {"id": 1001, "name": "egg", "type": "food", "price": 200},
  {"id": 1002, "name": "hook", "type": "dvd", "price": 999},
  {"id": 1003, "name": "8214", "type": "tool", "price": 0}
];
```

Line 3 (id: 1003) name in 8214 trough data has been specified, but: It is surrounded by quotation marks in that it has added a str to the key.  
Also, towards the price is a string that null was specified here has become 0.  
Because it is a string it becomes normal "null", but: it has become a result 0, which has been cast as a number by specifying the num.  
To specify the type There are the following types.  

### :str (or string)
str must be enclosed in quotation be any value specified.
Or single quotes or double quotes and more to the format of the data to be converted.
For example, you will double quotes if JSON if SQL in single quotes.

### :num (or number)
num is not enclosed in quotation marks when the numerical value is specified, then all if non-numerical data has been specified 0.
The numerical value contains an integer (including minus) or decimal.

### :const (or constant)
const is not enclosed in quotation marks any value. If null is addressed, UNDEFINED is specified as data.
This is useful, for example, if you want to set a variable that is not enclosed in quotation marks in the data.

### :flex (flexible)
flex is not enclosed in quotation marks if the null, true, is false has been set to the value.
Others are the same as normal.
The value of the object is together even in uppercase or lowercase.


### escape of a colon (:) in the header row
Colon as a string in the header line: If you want to use, you will need to escape the "\".

In the example below we have "\" escape the colon of the price (:).
And it is this example: the "price \ num": num is: not interpreted as a type, the key: it appears in the form of a "price num".
Example:
```
id	name:str	type	price\:num
1001	egg	food	200
1002	hook	dvd	999
```

Following is a result:
```JavaScript
[
  {"id": 1001, "name": "egg", "type": "food", "price:num": 200},
  {"id": 1002, "name": "hook", "type": "dvd", "price:num": 999}
];
```

### :type correspondence table
Of each in the following table: by the specified type, you can Yes whether data is converted copper as summarized in Table.    
※ leftmost column becomes the original data, the data is set in the header row, respectively: how data according to type will have been able to verify and converted.
※ The default header line: If you do not specify the type

|     |default| :str | :num | :const | :flex |
|:--- | :---: | :--: | :--: | :----: | :---: |
|0→   |0      |"0"   |0     |0       |0      |
|8882→|8882   |"8882"|8882  |8882    |8882   |
|-8882→|-8882|"-8882"|-8882|-8882|-8882|
|Lionel Messi→|"Lionel Messi"|"Lionel Messi"|0|Lionel Messi|"Lionel Messi"|
|→|""|""|0|UNDEFINED|""|
|TRUE→|"TRUE"|"TRUE"|0|TRUE|TRUE|
|FALSE→|"FALSE"|"FALSE"|0|FALSE|FALSE|
|NULL→|"NULL"|"NULL"|0|NULL|NULL|

※ place that does not contain anything to the left is an empty string.


### Browser

```html
<!doctype html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>tsvert in browser</title>
  <script src="tsvert.min.js"></script>
</head>
<body>
  <textarea id="tsv-input" rows="8" cols="100">id	name	type	price
  1001	egg	food	200
  1002	hook	dvd	999
  1003	hammer	tool	300
  1004	すし	食べ物	900</textarea>
  <br />
  <input id="test-button" type="button" value="test" onclick=""></input>
  <br />
  <textarea id="output" rows="8" cols="100"></textarea>
  <div id="content"></div>
  <script type="text/javascript">
    window.onload = function(){
    	var myButton = document.getElementById( "test-button" );
    	myButton.onclick = function()
    	{
        var tsv = document.getElementById('tsv-input').value;
        document.getElementById('output').innerHTML = tsvert(tsv, "sql");
    	};
    }
  </script>
</body>
</html>
```

If the above example, the output is converted tsv data obtained from textarea is the sql to <div id="content"></div>.

## Running Tests

To run the tests:

```
$ cd tsvert/
$ node test
```

## License

This project is released under the terms of the [MIT license](http://opensource.org/licenses/mit-license.php).
