function uncaughtException(err) {
  console.error('Server unexcpectedly crashed, closing app', err);
  process.exit(1);
}

const App = require('./code/app');

const app = new App();

// Listen to SIGTERM signal to handle graceful shutdown
process.on('SIGTERM', () => {
	try {
		console.warn('Server gracefully shut down, closing app');
		process.exit(0);
	} catch (e) {
		uncaughtException(e);
	}
});

try {
	app.start();
} catch(e) {
	uncaughtException(e);
}
