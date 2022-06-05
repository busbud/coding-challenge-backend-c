import { distanceRangeKM } from "../constants.js";
import { getLocationBetweenTwoPoints, isEndOfTerm } from "../helpers.js";
import TrieNode from "./node.js";

export default class Trie {
  constructor() {
    this.root = new TrieNode(null);
  }

  insert = (word, additionalInformation) => {
    return new Promise((resolve) => {
      if (!word) return;
      if (!word.length) return;

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
            name: additionalInformation.name,
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
        resolve();
      }
    });
  };
  /**
   *
   * @param {string} term the search term
   * @returns a Promise that will resolve to the result list
   */
  search = (term) => {
    return new Promise((resolve) => {
      if (!term) return resolve([]);
      if (!term.length) return resolve([]);

      let result = new Map();
      let parent = this.root;

      for (let t = 0; t < term.length; t++) {
        const character = term[t].toLowerCase();
        const exists = parent.children.has(character);

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
   * @param {Map} result holds a reference to the list of cities resulting in similarities
   * @void
   */

  traverse = (node, result) => {
    if (!node) return;
    if (!node.children) return;
    if (!node.children.size) return;

    for (const value of node.children.values()) {
      value.accuracy = Math.max(0, node.accuracy - 5);
      this.handleCompleteWord(value, result);

      if (value.children.size) {
        this.traverse(value, result);
      }
    }
  };

  /**
   *
   * @param {TrieNode} node holds a reference to the node that completed search
   * @param {Map} result holds a reference to the list of cities resulting in similarities
   */

  handleCompleteWord = (node, result) => {
    if (node.end) {
      for (const value of node.details.values()) {
        result.set(value.id, { ...value, accuracy: node.accuracy });
      }
    }
  };
}
