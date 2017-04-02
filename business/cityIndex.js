var _ = require("lodash");

/**
 * This class is used to store and find cities by their names.
 * It is actually a search tree.
 * @class
 */
function cityNode(level) {
    // the node's level
    this.level = level || 0;
    // Cities associated to this node
    this.cities = [];
    // Sub nodes hold cities which names start with the letters that brought us to the current node plus 1 letter
    this.subNodes = null;
}

/**
 * Indicates wether the current node is root or not
 */
cityNode.prototype.isRoot = function() {
    return this.level === 0;
}

/**
 * Add a city in the structure
 * @param {Object} city
 */
cityNode.prototype.addCity = function(city) {
    if (!city || !city.name)
        throw "Invalid city";

    var prefix = city.name.charAt(this.level).toLowerCase();
    
    var subNodeForPrefix;
    if (prefix) {
        if (!this.subNodes) {
            this.subNodes = {};
        } else {
            subNodeForPrefix = this.subNodes[prefix];
        }

        if (!subNodeForPrefix) {
            this.subNodes[prefix] = subNodeForPrefix = new cityNode(this.level + 1);
        }
        
        subNodeForPrefix.addCity(city);
    }

    // store all cities on the root node would be useless and memory consuming
    if (!this.isRoot()) {
        this.cities.push(city);
    }
};

/**
 * Find cities that matches a search term
 * @param {string} search - the search term
 */
cityNode.prototype.findCities = function(search) {
    if (!search)
        return [];

    var prefix = search.charAt(this.level).toLowerCase();

    if (!prefix) {
        // we've reached the last character of the search term and consequently the last node for this search
        // so we can return the cities of the current node (we clone it to prevent modifying the original cities)
        return _.clone(this.cities);
    }

    var matchingSubNode = this.subNodes[prefix];

    return matchingSubNode
        ? matchingSubNode.findCities(search) // there is a matching node for the current prefix letter, so we can go deeper in the tree
        : _.clone(this.cities); // no more matches, let's return the cities of the current node (we clone it to prevent modifying the original cities)
};

/**
 * Factory function for creating a city index
 * @export
 */
module.exports.createIndex = function() {
    return new cityNode();
};