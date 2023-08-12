import { ServerError } from '../errors';
import { Response } from 'express';

export function sendBadRequest(res: Response, error: Error) {
  res.status(400).send(error);
}

export function sendServerError(res: Response, error: Error) {
  res.status(500).send(new ServerError(error.stack));
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function sendOk(res: Response, data: any) {
  res.status(200).send(data);
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function sendNotFound(res: Response, data?: any) {
  res.status(404).send(data);
}
