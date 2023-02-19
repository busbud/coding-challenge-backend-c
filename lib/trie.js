/**
 * Trie.js
 * Trie Data structure altered to support location objects containing long,lat data.
 */



function normalizeString(str) {
    return str.normalize("NFD").replace(/[\u0300-\u036f]/g, "")
}

function Trie(val) {
    this.val = val;
    this.children = {};
    this.isEndOfWord = false;
    this.location= {};
}

/**
 * Inserts a word into the Trie
 * @param {Location} location 
 */
Trie.prototype.insertWord = function (location) {
    const { name } = location
    const nameNormalized = normalizeString(name).toLowerCase();
    let current = this;
    for (let character of nameNormalized) {
        if (!current.children[character]) {
            current.children[character] = new Trie(character);
        }

        current = current.children[character];
    }

    current.isEndOfWord = true;
    current.location = location;
}

/**
 * Searches for a word in the Trie
 * @param {String} prefix 
 * @returns {Array<Location>} Array of locations that match the prefix
 */
Trie.prototype.findWordsByPrefix = function (prefix) {
    let matchedWords = [];

    /**
     *  Traverse the Trie attempting to find the prefix path
     * @param {Trie} root 
     * @param {String} prefix 
     * @returns {Trie} The node where the prefix ends
     */
    const findPrefix = (root, prefix) => {
        const prefixNormalized = normalizeString(prefix).toLowerCase();
        let current = root;
        for (let character of prefixNormalized) {
            if (!current.children[character]) {
                return false;
            }

            current = current.children[character];
        }

        return current;
    }

    /**
     * Finds all children of given prefix path and adds them to 
     *  the matchedWords array
     * @param {Trie} root 
     * @param {String} wordSoFar 
     */
    const findFullChildrenOfPrefix = (root) => {
        // Base case 1
        if (!root) {
            return;
        }
        // Base case 2
        if (root.isEndOfWord) {
            matchedWords.push(root.location);
            //return;
        }

        const children = Object.keys(root.children);
        for (let character of children) {
            findFullChildrenOfPrefix(root.children[character])
        }
    }

    const lastPrefixNode = findPrefix(this, prefix);
    findFullChildrenOfPrefix(lastPrefixNode);
    return matchedWords;
}

// class Trie {

//     constructor(val) {
//         val,
//         this.children = {};
//         this.isEndOfWord = false;
//         this.location= {};
//     }

//     /**
//      * Insert a word into the Trie
//      * @param {String} word 
//      */
//     insertWord(location) {
//         const { name } = location
//         let current = this;
//         for (let character of name) {
//             if (!current.children[character]) {
//                 current.children[character] = new Trie(character);
//             }

//             current = current.children[character];
//         }

//         current.isEndOfWord = true;
//         current.location = location;
//     }

//     /**
//      * Find all words starting with prefix
//      * @param {String} prefix 
//      * @returns {Array} Matched location objects
//      */
//     findWordsByPrefix(prefix) {
//         let matchedWords = [];

//         /**
//          *  Traverse the Trie attempting to find the prefix path
//          * @param {Trie} root 
//          * @param {String} prefix 
//          * @returns {Trie} The node where the prefix ends
//          */
//         const findPrefix = (root, prefix) => {
//             let current = root;
//             for (let character of prefix) {
//                 if (!current.children[character]) {
//                     return false;
//                 }

//                 current = current.children[character];
//             }

//             return current;
//         }

//         /**
//          * Finds all children of given prefix and adds them to 
//          *  the matchedWords array
//          * @param {Trie} root 
//          * @param {String} wordSoFar 
//          */
//         const findFullChildrenOfPrefix = (root) => {
//             // Base case 1
//             if (!root) {
//                 return;
//             }
//             // Base case 2
//             if (root.isEndOfWord) {
//                 matchedWords.push(root.location);
//                 //return;
//             }

//             const children = Object.keys(root.children);
//             for (let character of children) {
//                 findFullChildrenOfPrefix(root.children[character])
//             }
//         }

//         const lastPrefixNode = findPrefix(this, prefix);
//         findFullChildrenOfPrefix(lastPrefixNode);
//         return matchedWords;
//     }
// }

module.exports = Trie;
