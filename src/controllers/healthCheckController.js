const { StatusCodes } = require('http-status-codes');

const appData = require('../../package.json');

module.exports = {
  checkHealth: (req, res) => {
    res
      .status(StatusCodes.OK)
      .json({
        result: 'I am alive!',
        version: appData.version,
        name: appData.name,
      });
  },
};
