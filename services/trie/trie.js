import { FLAGS, FLAG_VALUES } from "../constants.js";
import { isLastItemFromArray } from "../helpers.js";
import TrieNode from "./node.js";

export default class Trie {
  constructor() {
    this.root = new TrieNode(null);
    this.generalFlags = [];
  }

  insert = (word, additionalInformation) => {
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

      if (isLastItemFromArray(word, c)) {
        newNode.end = true;
        if (!newNode.information) {
          newNode.information = {
            name: additionalInformation.name,
            latitude: additionalInformation.lat,
            longitue: additionalInformation.long,
            city: `${additionalInformation.name}, ${additionalInformation.admin1}, ${additionalInformation.country}`,
          };
        } else {
          newNode.similarItems.push({
            name: additionalInformation.name,
            latitude: additionalInformation.lat,
            longitue: additionalInformation.long,
            city: `${additionalInformation.name}, ${additionalInformation.admin1}, ${additionalInformation.country}`,
          });
        }
      }

      if (!exists) {
        parent.children.set(character, newNode);
      }

      parent = newNode;
    }
  };

  search = (term) => {
    return new Promise((resolve) => {
      if (!term) return resolve([]);
      if (!term.length) return resolve([]);

      let result = [];
      let parent = this.root;

      for (let t = 0; t < term.length; t++) {
        const character = term[t].toLowerCase();
        const exists = parent.children.has(character);

        if (!exists) {
          this.generalFlags.push(FLAGS.CHAR_NOT_FOUND);
          continue;
        }

        const node = parent.children.get(character);
        parent = node;

        this.handleCompleteWord(node, result, term);

        if (isLastItemFromArray(term, t)) {
          this.traverse(node, result, term);
        }
      }

      resolve(result);
    });
  };

  traverse = (node, result, term) => {
    if (!node) return;
    if (!node.children) return;

    for (const value of node.children.values()) {
      value.flags.push(FLAGS.SIMILAR_WORD);

      this.handleCompleteWord(value, result, term);

      if (value.children.size) {
        this.traverse(value, result, term);
      }
    }
  };

  /**
   *
   * @param {TrieNode} node holds a reference to the node that completed search
   * @param {Array} result holds a reference to the list of cities resulting in similarities
   * @param {string} term the search terms
   */

  handleCompleteWord = (node, result, term) => {
    if (node.end) {
      const score = this.calculateScore(node, term);
      result.push({ score, ...node.information });
      node.similarItems.forEach((item) => {
        result.push({ score, ...item });
      });
    }
  };
  /**
   *
   * @param {EVENT_TYPE} event type of the event that requires calculation of the score.
   * @returns number
   */
  calculateScore = (node, term) => {
    const highestScore = 1;
    const score = [...this.generalFlags, ...node.flags].reduce((a, b) => {
      const calc = FLAG_VALUES[b](term, node.information.name);
      return a + calc;
    }, highestScore);

    return score.toPrecision(1);
  };
}
