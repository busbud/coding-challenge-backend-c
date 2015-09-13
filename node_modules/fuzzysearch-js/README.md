FuzzySearchJS
=============

Prime Implementation for fuzzy searching.

FuzzySearchJS provides an easy and modular way for fuzzy searching in JS for multiple purposes.

Special thanks to Arian Stolwijk and Dimitar Christoff for some early reviews and some ideas.

## Quick Start: ##

Install:
```js
npm install fuzzysearch-js
```

Usage:
```js
var FuzzySearch = require('fuzzysearch-js');
var levenshteinFS = require('fuzzysearch-js/js/modules/LevenshteinFS');
var indexOfFS = require('fuzzysearch-js/js/modules/IndexOfFS');
var wordCountFS = require('fuzzysearch-js/js/modules/WordCountFS');

var data = ['Hydrogen','Helium','Lithium','Beryllium'];

var fuzzySearch = new FuzzySearch(data, {'minimumScore': 300});
fuzzySearch.addModule(levenshteinFS({'maxDistanceTolerance': 3, 'factor': 3}));
fuzzySearch.addModule(indexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
fuzzySearch.addModule(wordCountFS({'maxWordTolerance': 3, 'factor': 1}));

var result = fuzzySearch.search('Hlium');
console.log(result);
```

## Todo: ##

- Add tests for Modules
- Implement own Levenshtein algorithm for improving performance. It's not neccessary to calculate the whole number everytime. You can calculate the first rows and everytime a new letter is added you can just calculate the next row. Also if your tolerance is reached you can stop calculate the other rows.
- Play around with factors for best results. Maybe some presets for different situations. It's a big difference if you make a live search or if you make a search with the full term.
- Take a closer look at Sift3 algorithm. Maybe there also is some potential for performance optimizations.
- Implement caching for results
- Implement that the user choice can be transfered to server, so maybe this also could be a source.
- Implement that caching for the same resultset can be preloaded from server.
- Make better examples and better readme

## About / Idea behind this project / How does it work? ##

I build FuzzySearchJS because I needed something more or less accurate for live searching through categories and stuff like that.
At the beginning i've done something very simple like just get all the available data and make an indexOf call, if it doesn't return -1 I got a hit and added this into my result array.
This works not that bad because you get results even if it is not exactly right or just matches some letters in your data set. For example:

Data:
```js
var dataSet = ['Hydrogen','Helium','Lithium','Beryllium','Boron','Carbon'];
```

If you type in 'lium' for example your results will be Helium and Beryllium which is not that bad for a user who just want to get less results to choose from.
But surely this will bring up some problems. What is if the user doesn't know how things are spelled. Or if he types 'lieum' which is a little mistake but he won't get any results and this sucks.
That's why I came up with a bit more complicated solution. I began writing a small algorithm which is based on the Levenshtein Algorithm which returns an integer of how many differences in words are. Or easier said "how many mistakes is the word away from being correct".
This algorithm works well if you just compare one word to another, but if you want to filter the results live as the user types you get some problems.
Because if your search term is just 1, 2 or 3 letters big the Levenshtein algorithm returns a large number of differences.
I.E. The user just typed: "hyd", the first value in your dataset is "Hydrogen" and the levenshtein algorithm returns 6 because h is lowercase, and there are 5 missing letters (rogen). Even if you are not case sensitive you still get back 5 which is way too much even if you could probably say the user is searching for "hydrogen".
So you have to combine some different methods to get good and accurate results. And as more the user types and specifies the search you get more to worry about. Because you always want to display the closest result on top.
That's why I introduced modules. Because for every purpose you need different criteria to decide which results are the best. If you want to get good live typing results, indexOf works pretty well, but if a word is completed, levenshtein will work much better.
If the user just searches with full terms a simple indexOf search is useless because it just would match if everything is right. What is if You have 3 words?
I.E.
```js
var value = "JavaScript DOM Framework";
```
Now the user types in "dom framework javascript" you got a very close match because all the words are matched. But you have to compare each word by word, also the order of the keywords are relevant. If you have something in your dataset like "dom javascript framework" you want to display this before "Javascript dom Framework" because it is closer at the user search.
That's why i created some modules for the first release of this project. You can easily write your own module and so get better results for your purpose.
Every module returns a number between 0 and 100. 100 means a perfect match, 0 is not relevant. For every module you can define a factor, because some things are only important if there is a small difference (i.E. the word count, it's just relevant if everything else matches perfectly).
So as more modules you add as higher your combinedScore will be for a match. If you add 2 modules with factor 1, your highest score can be 200. If you change the factor for one module to 3, your highest score will be 400. And so on. There is no limit.
If you added all the modules, you can ask for the maximum score:
```js
fuzzySearch.getMaximumScore();
```
You can also set a minimum score which should be reached that the data will included in the resultset.

## How to (just some thoughts / mindstorming) ##
```js
var dataSet = ['Hydrogen','Helium','Lithium','Beryllium','Boron','Carbon','Nitrogen','Oxygen','Fluorine','Neon','Sodium','Magnesium','Aluminum, Aluminium','Silicon','Phosphorus','Sulfur','Chlorine','Argon','Potassium','Calcium','Scandium','Titanium','Vanadium','Chromium','Manganese','Iron','Cobalt','Nickel','Copper','Zinc','Gallium','Germanium','Arsenic','Selenium','Bromine','Krypton','Rubidium','Strontium','Yttrium','Zirconium','Niobium','Molybdenum','Technetium','Ruthenium','Rhodium','Palladium','Silver','Cadmium','Indium','Tin','Antimony','Tellurium','Iodine','Xenon','Cesium','Barium','Lanthanum','Cerium','Praseodymium','Neodymium','Promethium','Samarium','Europium','Gadolinium','Terbium','Dysprosium','Holmium','Erbium','Thulium','Ytterbium','Lutetium','Hafnium','Tantalum','Tungsten','Rhenium','Osmium','Iridium','Platinum','Gold','Mercury','Thallium','Lead','Bismuth','Polonium','Astatine','Radon','Francium','Radium','Actinium','Thorium','Protactinium','Uranium','Neptunium','Plutonium','Americium','Curium','Berkelium','Californium','Einsteinium','Fermium','Mendelevium','Nobelium','Lawrencium','Rutherfordium','Dubnium','Seaborgium','Bohrium','Hassium','Meitnerium','Darmstadtium','Roentgenium','Copernicium','Ununtrium','Flerovium','Ununpentium','Livermorium','Ununseptium','Ununoctium'];
```


First you have to create a new FuzzySearch instance:

available options + defaults:
```js
'caseSensitive': false, //if set to true, all the comparisons will be done lowerCase
'termPath': '', //if you have objects in your data set you can set the path to the value you are searching through
'minimumScore': 0,//the minimum combinedScore which must be reached that the value is included in the result set
'returnEmptyArray': false //false: if no results are found it returns null. true: if no results are found it returns an empty array
```

```js
var FuzzySearch = require('./FuzzySearch');
var fuzzySearch = new FuzzySearch(dataSet, {'caseSensitive': false});
```

Now you should add some modules you like to use. At the moment the following are available:

- IndexOf Search
- Levenshtein
- Sift3 – similar to levenshtein
- word count

```js
var LevenshteinFS = require('./modules/LevenshteinFS');
var IndexOfFS = require('./modules/IndexOfFS');
var WordCountFS = require('./modules/WordCountFS');

fuzzySearch.addModule(LevenshteinFS({'maxDistanceTolerance': 3, 'factor': 3}));
fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
fuzzySearch.addModule(WordCountFS({'maxWordTolerance': 3, 'factor': 1}));
```

Now you are ready to go:

```js
fuzzySearch.search('Hlium');
```

Returns an array like this:

```js
[
    {"score":590,
     "details":[
        {"name":"LevenshteinFS","score":83.33333333333334,"factor":3},
        {"name":"IndexOfFS","score":80,"factor":3},
        {"name":"WordCountFS","score":100,"factor":1}
     ],
     "value":"Helium"
     },
     {"score":490,
      "details":[
        {"name":"LevenshteinFS","score":50,"factor":3},
        {"name":"IndexOfFS","score":80,"factor":3},
        {"name":"WordCountFS","score":100,"factor":1}
      ],
      "value":"Gallium"},
      ...
```

Like you see you get an overall score which indicates how close the match is.
You also get an array with more details, you see in there the score of every module and the factor by which the score is multiplied.
And for sure, in the "value" field you will find the value from the dataset.

## Objects ##

You can also search through objects. Just pass an array with objects as your dataset and set a "termPath" as option when instanciation FuzzySearch:

```js
var dataSet = [{'id': 1, 'demo': {'element': 'Hydrogen'}}, {'id': 2, 'demo': {'element': 'Helium'}}, {'id': 3, 'demo': {'element': 'Lithium'}}];
var fuzzySearch = new FuzzySearch(dataSet, {'caseSensitive': false, 'termPath': 'demo.element'});
```

Now your result would look something like this:

```js
[
    {"score":590,
     "details":[
        {"name":"LevenshteinFS","score":83.33333333333334,"factor":3},
        {"name":"IndexOfFS","score":80,"factor":3},
        {"name":"WordCountFS","score":100,"factor":1}
     ],
     "value": {'id': 1, 'demo': {'element': 'Hydrogen'}}
     },
     {"score":490,
      "details":[
        {"name":"LevenshteinFS","score":50,"factor":3},
        {"name":"IndexOfFS","score":80,"factor":3},
        {"name":"WordCountFS","score":100,"factor":1}
      ],
      "value":{'id': 2, 'demo': {'element': 'Helium'}}
     },
     ...
```

## Testing ##

Mac OS X Terminal or Linux Terminal.

```
cd FuzzySearchJS
make test
```

# Modules #

## LevenshteinFS ##

```js
var LevenshteinFS = require('./modules/LevenshteinFS');
```

options:
```js
'maxDistanceTolerance': 3, //if the distance is higher than 3 this module returns 0
'factor': 1
```

## Sift3FS ###

```js
var Sift3FS = require('./modules/Sift3FS');
```

options:
```js
'maxDistanceTolerance': 3, //if the distance is higher than 3 this module returns 0
'factor': 1
```

## IndexOfFS ##

Even if it sounds like an easy indexOf this is much more. It is a bit mistake tolerant and also dynamic in the minimum term length.
Please take a look at the source code until I find more time to explain it in detail. Or feel free to write one :)

```js
var IndexOfFS = require('./modules/IndexOfFS');
```

options:
```js
'minTermLength': 3,
'maxIterations': 500,
'factor': 1
```

## WordCountFS ##

```js
var WordCountFS = require('./modules/WordCountFS');
```

options:
```js
'maxWordTolerance': 3,
'factor': 1
```

