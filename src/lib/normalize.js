const DEFAULT_SCALE = {
  start: 0,
  end: 1
}

const normalize = (value, min, max, scale) => {
  return scale.start + (value - min) * (scale.end - scale.start) / (max - min)
}

export default (inputs, scale = DEFAULT_SCALE) => {
  let minValue
  let maxValue

  if (Array.isArray(inputs)) {
    minValue = Math.min(...inputs)
    maxValue = Math.max(...inputs)
    return inputs.map((value) => {
      return normalize(value, minValue, maxValue, scale)
    })
  }

  minValue = inputs.min || Math.min(inputs.data)
  maxValue = inputs.max || Math.max(inputs.data)
  return normalize(inputs.value, minValue, maxValue, scale)
}
