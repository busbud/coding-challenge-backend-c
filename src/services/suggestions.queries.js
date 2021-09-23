function formatRow(row) {
  // WARNING latitude and longitude should be converted to strings with 5 digits
  // Warning, the name is name + state + country
  // Score is rounded to one digit

  return row;
}

exports.getSuggestions = async ({ q, latitude, longitude }) => {
  // WARNING latitude and longitude should be converted to strings with 5 digits
  // Warning, the name is name + state + country
  // Score is rounded to one digit

  const suggestions = [
    {
      name: 'Londontowne, MD, USA',
      latitude: '38.93345',
      longitude: '-76.54941',
      score: 0.3
    }
  ];

  return suggestions.map(formatRow);
};
