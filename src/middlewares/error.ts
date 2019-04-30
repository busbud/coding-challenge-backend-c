import express from 'express';

export const errorHandler = (err: any, _req: Express.Request, res: Express.Response & { status: any; json: any }, _next: any): any => {
    console.error(err.stack);
    res.status(500);
    res.json({error: err.name,message: err.message});
};

export class BadRequestError extends Error {
    public constructor(message: string) {
        super(message);
        this.name = 'BadRequestError';
    }
}


export default errorHandler;
