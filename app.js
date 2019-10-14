const port = process.env.PORT || 2345;
const express = require('express');
const app = express();
const Suggestions = require ('./src/suggestions');


app.get('/suggestions', Suggestions.getSuggestions);

app.listen(port, () => {
	console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});