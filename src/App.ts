import express from 'express';
import routes from './routes';
import rateLimit from 'express-rate-limit';

class App {
    exp: express.Application;

    constructor() {
        this.exp = express();
        this.middleware();
        this.router()
    }

    middleware() {
        const limiter = rateLimit({
            windowMs: 10000,
            max: 20,
            message: 'Too many requests from this IP, please try again after 10 seconds'

        });
        this.exp.use(limiter);
    }

    router() {
        this.exp.use(express.json());
        this.exp.use(routes);
    }

    get express(): express.Application {
        return this.express;
    }
}

const app = new App().exp;
const port = process.env.PORT || 3333;
app.listen(port, () => {
    console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

export default app;