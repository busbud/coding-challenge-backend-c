import { readFileSync, existsSync } from 'fs';
import dotenv from 'dotenv';
dotenv.config();
const override = '.env.override';
if (existsSync(override)) {
  const envConfig = dotenv.parse(readFileSync(override));
  for (const k in envConfig) {
    process.env[k] = envConfig[k];
  }
}
