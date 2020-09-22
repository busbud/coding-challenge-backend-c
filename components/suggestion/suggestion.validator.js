const validateGet = async (req, res, next) => {
  if (!req.query.q) {
    return res.status(422).json({ error: 'Missing "q" param' });
  }
  return next();
};

export default {
  validateGet,
};
