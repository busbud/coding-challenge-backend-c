import express, { Request, Response, NextFunction } from 'express';
export const app = express();

app.use(express.json());
app.use(express.urlencoded({ extended: true }));

app.use(require('./routes/main'));

 

app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
    console.error(err.stack);
    res.status(500).send('Something broke!');
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => console.log(`Server started on port ${PORT}`));
