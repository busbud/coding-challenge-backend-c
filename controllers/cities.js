var fs = require('fs');
var crypto = require('crypto');
var fileIndex = {};
var fileIndexHash = undefined;

exports.getCities = function(search, latitude, longitude, callback){
    fs.readFile('./data/cities_canada-usa.tsv', 'utf8', function (err, data) {
        if (err) {
            return console.log(err);
        }
        var cities = parseTSVAndSearch(data, search, latitude, longitude);
        return callback(cities);
    });
}

var parseTSVAndSearch = function(tsv, search, latitude, longitude) {
    var x = tsv.split('\n');
    var result = [];
    var j = 0;
    var high_score = 0;

    if(typeof latitude !== "undefined" && typeof longitude !== "undefined"){
        var geo = true;
    } else {
        var geo = false;
    }

    /* Adding capability to change .tsv file if needed */
    if(typeof fileIndex.name === 'undefined'){
        getFileIndex(x[0]);
        fileIndexHash = calculateIndexHash(x[0]);
    } else if(typeof fileIndexHash !== 'undefined'){
        var tmp_hash = calculateIndexHash(x[0]);
        if(tmp_hash !== fileIndexHash){
            getFileIndex(x[0]);
            fileIndexHash = tmp_hash;
        }
    }

    for (var i=1; i < x.length - 1; i++) {
        y = x[i].split('\t');

        if(parseInt(y[fileIndex.population]) > 5000) {
            var score = 1 - (sift4(search.toLowerCase(), y[fileIndex.name].substring(0,search.length).toLowerCase(), y[fileIndex.name].length) / 10);

            result[j] = {
                "name": y[fileIndex.name] + ", " + provinceCode(y[fileIndex.adm1]) + ", " + countryCode(y[fileIndex.country]),
                "latitude": y[fileIndex.lat],
                "longitude": y[fileIndex.long],
                "score": score
            };
            if(high_score < score){
                high_score = score;
            }
            j++;
        }
    }

    /* filter and sort */
    return result.filter(function(obj) {
        if('score' in obj && parseFloat(obj.score) === parseFloat(high_score)) {
            if(geo === true){
                var location_score = getDistanceScore(parseFloat(latitude), parseFloat(longitude), parseFloat(obj.latitude), parseFloat(obj.longitude));
                obj.score = obj.score - location_score;
            }
            return true;
        } else {
            return false;
        }
    }).sort(function(a, b) {
        return parseFloat(b.score) - parseFloat(a.score);
    });;
}

var getFileIndex = function(str){
    row = str.split('\t');
    row.forEach(function(elem, index){
        switch(elem.trim()) {
            case 'ascii':
                fileIndex.name = index;
                break;
            case 'population':
                fileIndex.population = index;
                break;
            case 'lat':
                fileIndex.lat = index;
                break;
            case 'long':
                fileIndex.long = index;
                break;
            case 'country':
                fileIndex.country = index;
                break;
            case 'admin1':
                fileIndex.adm1 = index;
                break;
        }
    });
};

var calculateIndexHash = function(str){
    var shasum = crypto.createHash('sha1');
    shasum.update(str);
    return shasum.digest('hex');
};

var countryCode = function(code){
    if(code === 'US'){
        return 'USA';
    } else if(code === 'CA'){
        return 'Canada';
    } else {
        return 'Unknown';
    }
};

/* Canada Province Code */
var provinceCode = function(code){
    var province_code = code;
    switch(code.trim()) {
        case '01':
            province_code = 'AB';
            break;
        case '02':
            province_code = 'BC';
            break;
        case '03':
            province_code = 'MB';
            break;
        case '04':
            province_code = 'NB';
            break;
        case '05':
            province_code = 'NL';
            break;
        case '06':
            province_code = 'Unknown';
            break;
        case '07':
            province_code = 'NS';
            break;
        case '08':
            province_code = 'ON';
            break;
        case '09':
            province_code = 'PE';
            break;
        case '10':
            province_code = 'QC';
            break;
        case '11':
            province_code = 'SK';
            break;
        case '12':
            province_code = 'YT';
            break;
        case '13':
            province_code = 'NT';
            break;
        case '14':
            province_code = 'NU';
            break;
    }
    return province_code;
};

/* toRad() function */
if (typeof(Number.prototype.toRad) === "undefined") {
    Number.prototype.toRad = function() {
        return this * Math.PI / 180;
    }
}

var getDistanceScore = function(lat1, lon1, lat2, lon2) {
    var R = 6378.1; // Radius of the earth in km
    var dLat = (lat2 - lat1).toRad();
    var dLon = (lon2 - lon1).toRad();
    var a = Math.sin(dLat/2) * Math.sin(dLat/2) +
        Math.cos(lat1.toRad()) * Math.cos(lat2.toRad()) *
        Math.sin(dLon/2) * Math.sin(dLon/2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    var d = R * c;
    var score = 0.5;
    if(d <= 10){
        score = 0;
    } else if (d > 10 && d <= 100) {
        score = 0.1;
    } else if(d > 100 && d <= 200) {
        score = 0.2;
    } else if(d > 200 && d <= 600) {
        score = 0.3;
    } else if (d < 600 && d <= 900){
        score = 0.4;
    } else {
        score = 0.5;
    }
    return score;
    // Return Distance Score : < 10km score = 0
}

/* sift4 function */
var sift4 = function(s1, s2, maxOffset) {
    if (!s1||!s1.length) {
        if (!s2) {
            return 0;
        }
        return s2.length;
    }

    if (!s2||!s2.length) {
        return s1.length;
    }

    var l1=s1.length;
    var l2=s2.length;

    var c1 = 0;  //cursor for string 1
    var c2 = 0;  //cursor for string 2
    var lcss = 0;  //largest common subsequence
    var local_cs = 0; //local common substring

    while ((c1 < l1) && (c2 < l2)) {
        if (s1.charAt(c1) == s2.charAt(c2)) {
            local_cs++;
        } else {
            lcss+=local_cs;
            local_cs=0;
            if (c1!=c2) {
                c1=c2=Math.max(c1,c2);
            }
            for (var i = 0; i < maxOffset && (c1+i<l1 || c2+i<l2); i++) {
                if ((c1 + i < l1) && (s1.charAt(c1 + i) == s2.charAt(c2))) {
                    c1+= i;
                    local_cs++;
                    break;
                }
                if ((c2 + i < l2) && (s1.charAt(c1) == s2.charAt(c2 + i))) {
                    c2+= i;
                    local_cs++;
                    break;
                }
            }
        }
        c1++;
        c2++;
    }
    lcss+=local_cs;
    return Math.round(Math.max(l1,l2)- lcss);
}
