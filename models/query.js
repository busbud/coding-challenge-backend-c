const mongoose = require('mongoose');

const queryModel = mongoose.model('Query', {
    results: {
        type: Array
    }
});

module.exports = queryModel;