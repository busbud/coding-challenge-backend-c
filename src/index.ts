import * as dotenv from 'dotenv';

import app from './server';

dotenv.config();

app.listen(process.env.PORT, () => {
  console.log(`hello on http://${process.env.DOMAIN}:${process.env.PORT}`);
});
