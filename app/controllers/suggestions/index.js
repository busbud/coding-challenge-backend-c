const express = require('express');

const search = require('./search');
const {cache} = require('../../utils');

const router = new express.Router();

/**
 * Register cache key as params instead of URL, always in the same order
 * It enables to have all combinations of a query cached
 * e.g /suggestions?q=London&latitude=40&longitude=-80
 *      will result in a cached keyed as 'london:40:-80`, the same way as the following query
 *     /suggestions?latitude=40&q=London&longitude=-80
 * @param {Object} req Request object
 * @return {String} key
 */
const formatKey = (req) => {
    const {
        q,
        latitude,
        longitude,
    } = req.query;

    // Always register query in this order
    // so order of parameter doesn't matter for caching
    return `${q}:${latitude}:${longitude}`.toLowerCase();
};

router.get(
    '/',
    cache(60 * 60, formatKey),
    search
);

module.exports = router;
