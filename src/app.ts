import fastify from 'fastify';

const PORT = process.env.PORT || 2345;
export const app = fastify();

app.get('/suggestions', (req, res) => {
  res.send({
    suggestions: [],
  })
});

app.listen(PORT, '127.0.0.1', (err) => {
  if (!!err) {
    console.error(`Failed to start web server on port:`, err);
  } else {
    console.log(`Server is running at http://127.0.0.1:${PORT}`);
  }
})
