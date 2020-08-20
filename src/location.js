module.exports.search = (query, lat, long) => {
  return [
    {
      "query": query,
      "latitude": lat,
      "longitude": long
  }
  ];
};