import dotenv from 'dotenv';

export enum Env { production = 'production', development = 'development', test = 'test' }
export enum Path { src = 'src', dist = 'dist' }
export enum Ext { ts = 'ts', js = 'js' }

dotenv.config();

const path = process.env.NODE_PATH;

export default {
  env: process.env.NODE_ENV || Env.development,
  path,
  ext: path === Path.src ? Ext.ts : Ext.js,
};
