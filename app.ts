import express, { Request, Response, NextFunction } from 'express';



const app = express();

app.use(express.json());
app.use(express.urlencoded({ extended: true }));

 

app.use(require('./routes/main'));

 

app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
    console.error(err.stack);
    res.status(500).send('Something broke!');
});

app.listen(3000, () => {
    console.log('Server started on port 3000');
});
