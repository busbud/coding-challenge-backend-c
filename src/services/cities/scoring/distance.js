export default (distance) => {
  if (!distance) {
    return 0
  }

  return (distance.value / 100) * -1
}
