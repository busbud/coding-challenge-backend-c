import { Request, Response, NextFunction } from 'express';
export default interface RequestContract {
  (request: Request, response: Response, next: NextFunction): void;
}