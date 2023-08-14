import { Response } from 'express';
import env from '../../config/env';
import { ServerError } from '../errors';

export function sendBadRequest(res: Response, error: Error) {
  res.status(400).send({ error: error.message });
}

export function sendServerError(res: Response, error?: Error) {
  const err = new ServerError(
    env.env !== 'production' ? error?.stack : undefined
  );

  res.status(500).send({ error: err.message, stack: err.stack });
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function sendOk(res: Response, data: any) {
  res.status(200).send(data);
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function sendNotFound(res: Response, data?: any) {
  res.status(404).send(data);
}
