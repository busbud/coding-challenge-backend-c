class SuggestionsController {
  constructor(db) {
    this.db = db;
  }

  get(req, res) {
    const suggestions = [];
    res.writeHead(404, {'Content-Type': 'application/json'});
    res.end(JSON.stringify({ suggestions }))
  }
}

module.exports = SuggestionsController;