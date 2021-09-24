function formatRow(row) {
  // WARNING latitude and longitude should be converted to strings with 5 digits
  // Warning, the name is name + state + country
  // Score is rounded to one digit

  // WARNING latitude and longitude should be converted to strings with 5 digits
  // Warning, the name is name + state + country
  // Score is rounded to one digit

  return row;
}

async function getSuggestionsByNameAndLocation(q, latitude, longitude) {
  // WARNING latitude and longitude should be converted to strings with 5 digits
  // Warning, the name is name + state + country
  // Score is rounded to one digit

  if (q === 'SomeRandomCityInTheMiddleOfNowhere') return [];

  const suggestions = [
    {
      name: 'Londontowne, MD, USA',
      latitude: '38.93345',
      longitude: '-76.54941',
      score: 0.3
    }
  ];

  return [];
}

async function getSuggestionsByName(q) {
  return [];
}

exports.getSuggestions = async ({ q, latitude, longitude }) => {
  const hasLocation = !!latitude && !!longitude;

  const suggestions = hasLocation
    ? await getSuggestionsByNameAndLocation(q, latitude, longitude)
    : await getSuggestionsByName(q);

  return suggestions.map(formatRow);
};
