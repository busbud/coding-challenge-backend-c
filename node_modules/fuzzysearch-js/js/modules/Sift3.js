// sift3 - http://siderite.blogspot.com/2007/04/super-fast-and-accurate-string-distance.HTMLElement

"Use Strict";

var prime = require('prime');

var sift3 = prime({

    constructor: function(haystack) {
        this.haystack = haystack;
    },

    getDifference: function(s1, s2) {
        var c = 0,
            offset1 = 0,
            offset2 = 0,
            lcs = 0,
            maxOffset = 5,
            i = 0;

        if (s1 === null || s1.length === 0){
            if (s2 === null || s2.length === 0){
                return 0;
            } else {
                return s2.length;
            }
        }

        if (s2 === null || s2.length === 0){
            return s1.length;
        }


        while ((c + offset1 < s1.length) && (c + offset2 < s2.length)){
            if (s1.charAt(c + offset1) == s2.charAt(c + offset2)){
                lcs++;
            } else {
                offset1 = 0;
                offset2 = 0;
                for (; i < maxOffset; i++){
                    if ((c + i < s1.length) && (s1.charAt(c + i) == s2.charAt(c))){
                        offset1 = i;
                        break;
                    }
                    if ((c + i < s2.length) && (s1.charAt(c) == s2.charAt(c + i))){
                        offset2 = i;
                        break;
                    }
                }
            }
            c++;
        }
        return (s1.length + s2.length) / 2 - lcs;
    },

    setHaystack: function(haystack) {
        this.haystack = haystack;
    }
});

module.exports = sift3;