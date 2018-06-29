module.exports = (req, res, next) => {
  let { q, longitude, latitude, radius } = req.query;
  if (q === undefined || q.length < 2) {
    return res.status(400).json({ error: "Missing 'q' parameter or 'q' must be at least 2 characters long" });
  }

  if ((longitude === undefined && latitude !== undefined) || (longitude !== undefined && latitude === undefined)) {
    return res.status(400).json({ error: "Missing 'longitude' or 'latitude' parameters" });
  }

  if (radius !== undefined && (isNaN(radius) || radius < 1 || radius > 1000)) {
    return res.status(400).json({ error: "Bad parameter 'radius'. Value must be a number between 1 and 1000 km." });
  }

  next();
};
