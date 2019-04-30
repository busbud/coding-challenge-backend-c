require('dotenv').config();

import app from './app';

// Use strict is implied in import with ES6 modules
const instance = app.listen(process.env.PORT);


// Graceful process ending
process.on('SIGTERM', function (): void {
    console.log('SIGTERM caught');
    instance.close(function (): void {
        console.log('Instance closed, exiting process');
        process.exit(0);
    });
});

console.log(`App Started on port ${process.env.PORT}`);
