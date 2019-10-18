module.exports = { normalizeValues };

// Brings a value into the 0 - 1 range, depending on the scale the numbers existed in previously.
function normalizeValues(value, minValue, maxValue) {
  const range = maxValue - minValue;
  return (value - minValue) / range;
}