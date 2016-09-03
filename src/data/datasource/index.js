let connection

export default {
  get redis() {
    return connection || require('./connection').default
  }
}