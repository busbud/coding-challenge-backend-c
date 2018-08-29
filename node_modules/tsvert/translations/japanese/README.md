# tsvert
「tsvert 」はタブ区切りテキストを様々なデータ形式に変換するために書かれた小さなライブラリです。
JavaScriptで書かれていて サーバー(node.js)、クライアント(ブラウザ)の両方で使用することが可能です。
変換できるにデータにはsqlのインサート文やjsonやphp, ruby, pythonなどの配列、連想配列に対応しています。

## インストール
```
npm install tsvert --save
```

## 使用例
例えば次のような、スプレッドシートからコピーしたタブ区切りのテキストデータがあったとしましょう。

[data.tsv](https://docs.google.com/spreadsheets/d/18hi71QjLI1_Odpi1bB3HpeMAo6LgvKluLBafBZJRINc/edit#gid=0)
```
id	name	type	price
1001	egg	food	200
1002	hook	dvd	999
1003	hammer	tool	300
1004	すし	食べ物	900
```
※1行目のヘッダーは各行データのキーを指定したものです。　

tsvertを使い,変換typeとしてjsonを指定すれば、対象のtsvデータをjsonに変換することが可能です。

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
上記のスクリプトでえられる結果は以下のとおりです。
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


## オプションの指定
「tsvert」 はいくつかのオプションを指定することができます。
「tsvert」のオプションのデフォルト設定は以下のようになってます。

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

それぞれのオプションについて解説します。

### html
html オプションはブラウザでの出力時に改行して表示されるようにするためのオプションで改行コードに"\n"を用いずに"<br>"を用います。
デフォルトで falseになっています。
html オプションをtrueにする以下のような結果がえられます。
```JavaScript
[<br>
  {"id": 1001, "name": "egg", "type": "food", "price": 200},<br>
  {"id": 1002, "name": "hook", "type": "dvd", "price": 999}<br>
];<br>
```

### indent
indent オプションは対象のデータをより深い層までインデントして出力します。
indent オプションはデフォルトの状態でfalseとなっており、通常は次のように表示されます。
```JavaScript
[
  {"id": 1001, "name": "egg", "type": "food", "price": 200},
  {"id": 1002, "name": "hook", "type": "dvd", "price": 999}
];
```
indent オプションをtrueにすると、次のように出力されます。
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
sqlBulkInsert オプションはsql insert文を bulk insert形式にして一つのsql文にまとめて出力するためのオプションです。
sqlでしか使うことができません。
デフォルトでfalseになっています。

デフォルトの状態では次のようなsql文が出力されます。
```sql
INSERT INTO table(id, name, type, price) VALUES(1001, 'egg', 'food', 200);
INSERT INTO table(id, name, type, price) VALUES(1002, 'hook', 'dvd', 999);
```
sqlBulkInsert オプションをtrueにすると次のようなsql文が出力されます。

```sql
INSERT INTO table(id, name, type, price) VALUES(1001, 'egg', 'food', 200)
,(header1, header2, header3, header4, header5, header6:str) VALUES(8882, 8882, 8882, '8882', 8882, 8882);
```

### sqlTableName
sqlTableName オプションではsql insert文を出力時のテーブル名を指定することができます。

テーブル名の指定方法
```JavaScript
var tsvert = require('tsvert');

tsvert.setOptions({
  "sqlTableName": "products"  
});
```

テーブル名として「products」を指定した場合は以下のような出力結果がえられる。
```sql
INSERT INTO products(id, name, type, price) VALUES(1001, 'egg', 'food', 200);
INSERT INTO products(id, name, type, price) VALUES(1002, 'hook', 'dvd', 999);
```

### rubySymbolKey
ruby の連想配列を出力する場合、連想配列のキーはシンボルになっています。
rubySymbolKey オプションをfalse に指定することで、文字列のキーにすることができます。

rubySymbolKeyがデフォルトのtrueの状態では以下の結果になります。
```ruby
[
  {:id => 1001, :name => "egg", :type => "food", :price => 200},
  {:id => 1002, :name => "hook", :type => "dvd", :price => 900}
]
```
rubySymbolKeyをfalseにすると以下の結果になります。
```ruby
  {"id" => 1001, "name" => "egg", "type" => "food", "price" => 200},
  {"id" => 1002, "name" => "hook", "type" => "dvd", "price" => 999}
]
```
### useRowNumberKey
useRowNumberKey　オプションは各行の連想配列に番号キーをつけるためのオプションです。
デフォルトでfalseになっています。
useRowNumberKey　オプションをtrueにすることで次のような結果になります。
```JavaScript
[
  "1": {"id": 1001, "name": "egg", "type": "food", "price": 200},
  "2": {"id": 1002, "name": "hook", "type": "dvd", "price": 999}
];
```

### header
header オプションは　各列データのキーとして指定するtsvデータの行ヘッダーをデータではなくプログラムから指定するためのオプションです。
例えば、次のようなtsvデータがあったとしましょう。
```
1001	egg	food	200
1002	hook	dvd	999
```

以下のようにしてheaderを指定します。
タブは文字列で指定する場合は"\t"とする必要があります。
```JavaScript
tsvert.setOptions({
  "header": "id\tname\ttype\tprice"
});      
```
headerをオプションで指定することで次のような結果をえることができます。
```JavaScript
[
  {"id": 1001, "name": "egg", "type": "food", "price": 200},
  {"id": 1002, "name": "hook", "type": "dvd", "price": 999}
];
```

### オプション対応表
変換するデータ形式のタイプによって、使用できるオプションが異なります。
以下はオプション対応表になります。

|       | html |indent|header|useRowNumberKey|sqlBulkInsert|sqlTableName|rubySymbolKey|header|
| :---  | :--: |:---: |  :-: | :------------:|:-----------:|:----------:|:-----------:|:----:|
| JSON  |  〇  |  〇  |  〇  |      〇        | ×           |×           |×            |×     |
| SQL   |  〇  |  〇  |  〇  |×               |〇           | 〇         |×            |×     |
| PHP   |  〇  |  〇  |  〇  |〇              |×            |×           |×            |×     |
| Python|  〇  |  〇  |  〇  |〇              |×            |×           |×            |×     |
| Ruby  |  〇  |  〇  |  〇  |〇              |×            |×           |〇           | ×    |


## :タイプの指定
:タイプは行ヘッダーのキ-に付加することでデータの形式を指定するものです。
tsvertでは通常データの内容によってクォーテーションで囲むかどうか判定します。
数値であればクォーテーションで囲まず、数値でなければクォーテーションで囲みません。
タイプの指定をすれば、クォーテーションで囲むかどうかなどをコントロールできます。
タイプはキーの末尾にコロン（:）で区切って付加することで指定できます。

以下の例では、nameに str 、priceに num を指定しています。
```
id	name:str	type	price:num
1001	egg	food	200
1002	hook	dvd	999
1003	8214	tool	null
```
上記の指定により次のような結果がえられます。
```JavaScript
[
  {"id": 1001, "name": "egg", "type": "food", "price": 200},
  {"id": 1002, "name": "hook", "type": "dvd", "price": 999},
  {"id": 1003, "name": "8214", "type": "tool", "price": 0}
];
```

3行目(id: 1003)の nameに8214といデータが指定されていますが、:str をキーに付加したことでクォーテーションで囲まれています。
また、priceのほうにはnullという文字列が指定されてましたがこちらは0になっています。
文字列なので通常 "null" となりますが、:numを指定することにより数値としてキャストされた結果0になってます。

タイプの指定には以下の種類があります。

### :str (or string)
str はどんな値が指定されていても必ずクォーテーションで囲みます。
シングルクォーテーションかダブルクォーテーションかは変換するデータの形式によります。
例えば、SQL であればシングルクォーテーションでJSONであればダブルクォーテーションになります。

### :num (or number)
numは数値が指定されている時はクォーテーションで囲まず、数値以外のデータが指定されていた場合は全て0にします。
数値には整数（マイナス含む）や小数が含まれます。

### :const (or constant)
constはどんな値でもクォーテーションで囲みません。空文字が対処の場合は、UNDEFINEDがデータとして指定されます。
データに　クォーテーションで囲まない変数をセットする場合などに有効です。

### :flex (flexible)
flexは値に null, true, falseがセットされていた場合にクォーテーションで囲みません。
その他は通常と同じです。
対象の値が小文字でも大文字でも一緒です。


### header行でのコロン（:）のエスケープ
header行で文字列としてコロン : を使用したい場合、「\」でエスケープする必要があります。

下記の例ではpriceのコロン (:)を「\」エスケープしています。
この例だと「price\:num」の:numは :type　として解釈されず、キーには「price:num」の形で表示されます。
例.
```
id	name:str	type	price\:num
1001	egg	food	200
1002	hook	dvd	999
```

取得結果は以下のようになります。
```JavaScript
[
  {"id": 1001, "name": "egg", "type": "food", "price:num": 200},
  {"id": 1002, "name": "hook", "type": "dvd", "price:num": 999}
];
```

### :type 対応表
次の表にそれぞれの :type の指定によって、データがどうように変換されるかを表にまとめてあります。
※左端の列が元のデータになり、そのデータがそれぞれヘッダー行に設定されている:type に合わせてどのようにデータが変換されるか確認できるようになっています。
※ヘッダー行のdefault は:typeを指定しない場合

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

※左端になにも入っていない箇所は空文字です。


### ブラウザ

tsvert　をブラウザで使う場合は以下のようにします。
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
上記の例であれば、<div id="content"></div>にtextareaに表示されているtsvデータがsqlに変換されて出力されます。

### テスト

テストは以下のコマンドで実行できます。

```
$ cd tsvert/
$ node test
```


## ライセンス

This project is released under the terms of the [MIT license](http://opensource.org/licenses/mit-license.php).
