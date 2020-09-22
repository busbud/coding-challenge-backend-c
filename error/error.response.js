export default (status, body) => ({
  status,
  body,
  stack: new Error().stack,
});
