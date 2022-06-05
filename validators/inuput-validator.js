module.exports = function(req, res, next) {
  const {q, longitude, latitude} = req?.query;
  if(!q || (longitude && Number.isNaN(Number(longitude))) ||  (latitude && Number.isNaN(Number(latitude)))) {
    res.status(400).json({suggestions: [], msg: 'Please check your query parameters'});
    return;
  }
  next();
}