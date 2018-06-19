const { Main } = require('./src');

async function run() {
  try {
    const main = new Main();
    await main.init();
    main.start();
  } catch(err) {
    console.log('Unexpected error during initialisation', err);
  }
}

module.exports = run();

