module.exports = function (a, b) {
  if (a === b) return 0;
  if (!a.length || !b.length) return a.length || b.length;
  if (b.length > a.length) {
    var temp_a = a;
    a = b;
    b = temp_a;
  }

  d_matrix = [];

  for (i = 0; i <= a.length + 1; i++) {
    d_matrix.push([i]);
  }

  for (j = 0; j <= b.length; j++) {
    d_matrix[0].push(j);
  }

  for (i = 1; i <= a.length; i++) {
    for (j = 1; j <= b.length; j++) {
      var indicator = (a[i - 1] === b[j - 1]) ? 0 : 1;
      
      d_matrix[i][j] = Math.min(
        d_matrix[i-1][j] + 1,
        Math.min(
          d_matrix[i][j-1] + 1,
          d_matrix[i-1][j-1] + indicator
        )
      );
    }
  }

  return d_matrix[a.length][b.length];
};