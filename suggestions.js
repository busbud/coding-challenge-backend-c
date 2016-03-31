var distance = require('jaro-winkler');
var nGram = require('n-gram');
var removeDiacritics = require('diacritics').remove;
var ngramSize = 3;
var jaroWinklerMinimalDistance = 0.85;

var index = null;
var charDict = {};

module.exports = {
    find: function(q, latitude, longitude) {
        var location = latitude != null && longitude != null ? {lat: Number(latitude), lon: Number(longitude)} : null;
        q = removeDiacritics(q).toLowerCase();

        if(index) {
            var results = find(q, location, 10);

            var formattedResults = results.map(function(result) {
                var city = result.city;

                return {
                    name: city.indexString,
                    latitude: city.location.lat.toString(),
                    longitude: city.location.lon.toString(),
                    score: Math.round(result.score * 1000) / 1000
                };
            });

            return formattedResults;
        }
        else {
            return [];
        }
    },
    loadData: loadData
};

var countryNames = {
    'US': 'USA',
    'CA': 'Canada'
}

var admin1codeCanada = {
    '00': "",
    '01': "AB",
    '02': "BC",
    '03': "MB",
    '04': "NB",
    '05': "NL",
    '07': "NS",
    '08': "ON",
    '09': "PE",
    '10': "QC",
    '11': "SK",
    '12': "YT",
    '13': "NT",
    '14': "NU"
}

function loadData() {
    initChars();

    var fs = require('fs');
    var lines = fs.readFileSync('data/cities_canada-usa.tsv').toString().split("\n");

    var cities = [];

    for(var i in lines) {
        var values = lines[i].split('\t');

        if(values.length > 1) {
            cities.push(formatData(values));
        }
    }

    indexData(cities);
}

function formatData(values) {
    var city = {
        name: values[1],
        nameForIndex: sanatizeStringForIndex(values[2]),
        alternateNames: values[3].split(',').map(function(name) { return name.trim(); }),
        admin1Code: admin1codeCanada[values[10]] ? admin1codeCanada[values[10]] : values[10],
        location: {lat: Number(values[4]), lon:  Number(values[5])},
        countryCode: values[8],
        countryName: countryNames[values[8]]
    };

    city.encodedNameForIndex = encodeStringToNumber(city.nameForIndex);

    city.indexString = removeDiacritics(city.name + ", " + city.admin1Code + ", " + city.countryName);

    return city;
}

function sanatizeStringForIndex(s) {
    return removeDiacritics(s.toLowerCase());
}

function indexData(cities) {
    index = {};

    cities.forEach(function(city) {
        nGram(ngramSize)(city.nameForIndex).forEach(function(ngram) {
            if(!index[ngram]) index[ngram] = [];
            index[ngram].push(city);
        });
    });
}

function find(query, location, limit) {
    if(!limit) limit = 5;

    var candidates = findNGram(query);

    var scoredCandidates = score(query, candidates);

    var scoredWithLocation = scoreWithLocation(scoredCandidates, location)

    var sortedCandidates = scoredWithLocation
        .filter(function(e) { return e.score >= jaroWinklerMinimalDistance; })
        .sort(function(a, b) { return Number(b.score) - Number(a.score) })
        .slice(0, limit - 1);

    return sortedCandidates;
}

function scoreWithLocation(candidates, location) {
    var maxVariation = 0.1;

    if(location) {
        var distances = [];

        for(var i = 0; i < candidates.length; ++i) {
            var candidate = candidates[i];
            candidate.squaredDistance = equirectangularApproximation(location, candidate.city.location);
            distances.push(candidate.squaredDistance);
        }

        var maxDistance = Math.max.apply(null, distances);

        for(var i = 0; i < candidates.length; ++i) {
            var candidate = candidates[i];
            candidate.score = maxVariation * candidate.score * (1 - candidate.squaredDistance / maxDistance) + (1 - maxVariation) * candidate.score;
        }

        return candidates;
    }
    else {
        return candidates;
    }
}

function equirectangularApproximation(location1, location2) {
    var x = deg2rad(location2.lon - location1.lon) * Math.cos(deg2rad(location2.lat + location1.lat));
    var y = deg2rad(location2.lat - location1.lat);

    return x*x + y*y;
}

function deg2rad(degree) {
    return degree * (Math.PI / 180);
}

function filterBitMap(query, encodedQuery, city) {
    var a = encodedQuery;
    var b = city.encodedNameForIndex;

    if(query.length > city.nameForIndex.length) {
        var t = a;
        a = b;
        b = t;
    }

    var u = Math.min(query.length, city.nameForIndex.length) - bitcount(a & ~b);

    // Estimation of the best possible match without transpositions for the Jaro-Winkler distance
    var maximumDistance = 1/3 * (u / query.length + u / city.nameForIndex.length + 1);

    return maximumDistance > jaroWinklerMinimalDistance - 0.05;
}

function score(query, candidates) {
    var encodedQuery = encodeStringToNumber(query);
    var filteredCandidates = candidates.filter(function(city) { return filterBitMap(query, encodedQuery, city); });

    var candidatesWithDistance = [];

    for(var i in filteredCandidates) {
        var city = filteredCandidates[i]

        var score = distance(query, city.nameForIndex);

        candidatesWithDistance.push({score: score, city: city});
    }

    return candidatesWithDistance;
}

function findNGram(query) {
    var candidates = [];
    var frequencies = {};

    var ngrams = nGram(ngramSize)(query);

    ngrams.forEach(function(ngram) {
        if(index[ngram]) {
            index[ngram].forEach(function(city) {
                if(!frequencies[city.nameForIndex]) frequencies[city.nameForIndex] = 0;
                frequencies[city.nameForIndex] += 1;

                candidates.push(city);
            });
        }
    });

    var filteredByFrenquenciesCandidates = candidates.filter(function(city) {
        var numberOfMissingNGramAllowed = 2;
        var shortestTermLength = Math.min(query.length, city.nameForIndex.length);
        var minimumMatchingNgrams = shortestTermLength - (ngramSize - 1) - numberOfMissingNGramAllowed;

        return frequencies[city.nameForIndex] >= minimumMatchingNgrams;
    });

    var unduplicatedCandidates = removeDuplicates(filteredByFrenquenciesCandidates);

    return unduplicatedCandidates;
}

function removeDuplicates(arr) {
    var keys = {};

    for(var i = 0; i < arr.length; i++) {
        keys[arr[i].indexString] = arr[i];
    }

    arr = [];
    for(var k in keys) {
        arr.push(keys[k]);
    }

    return arr;
}

function initChars() {
    var alphabet = "abcdefghijklmnopqrstuvwxyz".split("");

    for(var i in alphabet) {
        charDict[alphabet[i]] = i;
    }
}

function encodeStringToNumber(s) {
    var int = 0;

    for(var i = 0; i < s.length; ++i) {
        var c = charDict[s.charAt(i)];

        if(c) {
            int |= 1 << c;
        }
    }

    return int;
}

function bitcount(n) {
    var bitCount = 0;

    while (n !== 0) {
        bitCount += n & 1;
        n = n >> 1;
    }

    return bitCount;
}
