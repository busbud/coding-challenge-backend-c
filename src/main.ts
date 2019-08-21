import { init } from './app'

const { PORT = 3000 } = process.env

init().then(app =>
  app.listen(PORT, () => console.log(`http://localhost:${PORT}`))
)
