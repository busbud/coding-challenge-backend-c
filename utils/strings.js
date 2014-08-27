/*
String utilities
*/

exports.prefixesFor = function(string) {
  var prefixes = [];
  string = string.toLowerCase();
  var words = string.split(' ');
  words.push(string);

  words.forEach(function(word) {
    for (var i = 2; i < word.length+1; i++)
    {
      prefixes.push(word.slice(0, i));
    }
  });
  return prefixes;
}
