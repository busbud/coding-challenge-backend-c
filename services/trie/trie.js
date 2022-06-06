import { isEndOfTerm } from "../../utils/helpers.js";
import TrieNode from "./node.js";

/**
 * Trie implementation to index all cities to speed search, and provide suggestions
 */
export default class Trie {
  constructor() {
    this.root = new TrieNode(null);
  }

  /**
   *
   * @param {string} word the word to be indexed
   * @param {Object} additionalInformation additional information to be stored as details to the node.
   * @returns Promise
   */
  insert = (word, additionalInformation) => {
    return new Promise((resolve, reject) => {
      if (!word) reject();
      if (!word.length) reject();

      let parent = this.root;

      for (let c = 0; c < word.length; c++) {
        const character = word[c].toLowerCase();

        // check if the character exists in the children
        const exists = parent.children.has(character);
        let newNode = new TrieNode(character);
        newNode.parent = parent;

        if (exists) {
          newNode = parent.children.get(character);
        }

        if (isEndOfTerm(word, c)) {
          newNode.end = true;
          newNode.details.set(additionalInformation.id, {
            id: additionalInformation.id,
            name: additionalInformation.ascii,
            latitude: Number(additionalInformation.lat),
            longitude: Number(additionalInformation.long),
            state: additionalInformation.admin1,
            country: additionalInformation.country,
          });
        }

        if (!exists) {
          parent.children.set(character, newNode);
        }

        parent = newNode;
      }

      resolve();
    });
  };
  /**
   *
   * @param {string} term the search term
   * @returns Promise<Array>
   */
  search = (term) => {
    return new Promise((resolve) => {
      if (!term) resolve([]);
      if (!term.length) resolve([]);

      let result = new Map();
      let parent = this.root;

      // Iterate each word character
      for (let t = 0; t < term.length; t++) {
        const character = term[t].toLowerCase();
        const exists = parent.children.has(character);

        // If the word is not found, it will break the ireration and do the traverse of the remaining tree for that parent.
        if (!exists) {
          if (parent.children.size) {
            this.traverse(parent, result);
          }

          break;
        }

        const node = parent.children.get(character);
        parent = node;

        this.handleCompleteWord(node, result);

        if (isEndOfTerm(term, t)) {
          this.traverse(node, result);
        }
      }

      resolve(result);
    });
  };

  /**
   *
   * @param {TrieNode} node holds a reference to a node being traversed
   * @param {Map} result holds a reference to the list of matched suggestions
   * @void
   */

  traverse = (node, result) => {
    if (!node) return;
    if (!node.children) return;
    if (!node.children.size) return;

    for (const value of node.children.values()) {
      value.accuracy = Math.max(0, node.accuracy - 1);
      this.handleCompleteWord(value, result);

      if (value.children.size) {
        this.traverse(value, result);
      }
    }
  };

  /**
   *
   * @param {TrieNode} node holds a reference to the node that holds the complete word details
   * @param {Map} result holds a reference to the list of matched suggestions
   */

  handleCompleteWord = (node, result) => {
    if (node.end) {
      for (const value of node.details.values()) {
        result.set(value.id, { ...value, accuracy: node.accuracy });
      }
    }
  };
}
