var Utils = {

  //function to remove the accents
  removeAccents : function (strAccents) {
    var strAccents = strAccents.split('');
    var strAccentsOut = new Array();
    var strAccentsLen = strAccents.length;
    var accents = 'ÀÁÂÃÄÅàáâãäåÒÓÔÕÕÖØòóôõöøÈÉÊËèéêëðÇçÐÌÍÎÏìíîïÙÚÛÜùúûüÑñŠšŸÿýŽž';
    var accentsOut = "AAAAAAaaaaaaOOOOOOOooooooEEEEeeeeeCcDIIIIiiiiUUUUuuuuNnSsYyyZz";
    for (var y = 0; y < strAccentsLen; y++) {
      if (accents.indexOf(strAccents[y]) != -1) {
        strAccentsOut[y] = accentsOut.substr(accents.indexOf(strAccents[y]), 1);
      } else
        strAccentsOut[y] = strAccents[y];
      }
      strAccentsOut = strAccentsOut.join('');
      return strAccentsOut;
  },

  levenshtein : function (str1, str2) {
    var current = [], prev, value;

    for (var i = 0; i <= str2.length; i++)
      for (var j = 0; j <= str1.length; j++) {
        if (i && j)
          if (str1.charAt(j - 1) === str2.charAt(i - 1))
            value = prev;
          else
            value = Math.min(current[j], current[j - 1], prev) + 1;
        else
          value = i + j;

        prev = current[j];
        current[j] = value;
      }

    return current.pop();
  },

  // return an edit distance from 0 to 1
  distance : function(str1, str2) {
    if (str1 === null && str2 === null) throw 'Trying to compare two null values';
    if (str1 === null || str2 === null) return 0;
    str1 = String(str1); str2 = String(str2);

    var distance = this.levenshtein(str1, str2);
    if (str1.length > str2.length) {
      return 1 - distance / str1.length;
    } else {
      return 1 - distance / str2.length;
    }
  },

  compare_score_desc : function(a,b) {
    if (a.score < b.score)
      return 1;
    if (a.score > b.score)
      return -1;
    return 0;
  }
};

if (exports) {
   exports.Utils = Utils;
}
