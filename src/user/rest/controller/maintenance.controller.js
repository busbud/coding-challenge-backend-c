const config = require('../../../../config');
const suggestionCommand = require('../../../command/suggestions.command');
const maintenanceResponse = require('../response/maintenance.response');
const elasticsearchIndex = require('../../../infrastructure/suggestions/elasticsearch.index');

let currentImporting = false;

const handler = async (req, res) => {
  if (req.url.indexOf('/maintenance') === 0
      && (!('authorization' in req.headers)
          || req.headers.authorization !== `Bearer ${config.maintenanceToken}`
      )
  ) {
    maintenanceResponse.response(res, 'Unauthorized', 401);
    return true;
  }

  if (req.url.indexOf('/maintenance/populate') === 0 && req.method === 'POST') {
    if (!currentImporting) {
      suggestionCommand.importFile(config.suggestionDataSource)
        .then(() => { currentImporting = false; })
        .catch(() => { currentImporting = false; });
      maintenanceResponse.response(res, 'running', 202);
      currentImporting = true;
      return true;
    }
    maintenanceResponse.response(res, 'import is already running', 409);
    return true;
  }

  if (req.url.indexOf('/maintenance/index') === 0 && req.method === 'DELETE') {
    await elasticsearchIndex.deleteIndex();
    maintenanceResponse.response(res, 'deleted', 200);
    return true;
  }

  if (req.url.indexOf('/maintenance/index') === 0 && req.method === 'POST') {
    await elasticsearchIndex.createIndex(`${process.cwd()}/suggestions_index.json`);
    maintenanceResponse.response(res, 'created', 201);
    return true;
  }
  return false;
};

module.exports = handler;
