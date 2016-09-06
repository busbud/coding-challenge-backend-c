export const mean = (inputs) => {
  const total = inputs.reduce((t, v) => {
    t += v
    return t
  }, 0)

  return total / inputs.length
}

export const deviation = (inputs, inputsMean) => {
  const m = inputsMean || mean(inputs)
  const total = inputs.reduce((t, v) => {
    t += Math.pow(v - m, 2)
    return t
  }, 0)

  return Math.sqrt(total / inputs.length)
}

/**
* A z-score less than 0 represents an element less than the mean.
* A z-score greater than 0 represents an element greater than the mean.
* A z-score equal to 0 represents an element equal to the mean.
* A z-score equal to 1 represents an element that is 1 standard deviation greater than the mean; a z-score equal to 2, 2 standard deviations greater than the mean; etc.
* A z-score equal to -1 represents an element that is 1 standard deviation less than the mean; a z-score equal to -2, 2 standard deviations less than the mean; etc.
*/
export default (inputs) => {
  const m = mean(inputs)
  const d = deviation(inputs, m)
  return inputs.map((i) => (i - m) / d)
}
