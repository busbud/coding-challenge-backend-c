module.exports = { normalizeValues };

function normalizeValues(value, minValue, maxValue) {
  const range = maxValue - minValue;
  return (value - minValue) / range;
}