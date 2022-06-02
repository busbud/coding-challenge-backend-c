import { distanceRangeKM, FLAGS, FLAG_VALUES } from "../constants.js";
import {
  getLocationBetweenTwoPoints,
  isLastItemFromArray,
} from "../helpers.js";
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
            latitude: Number(additionalInformation.lat),
            longitude: Number(additionalInformation.long),
            city: `${additionalInformation.name}, ${additionalInformation.admin1}, ${additionalInformation.country}`,
          };
        } else {
          const similarNode = new TrieNode();
          similarNode.information = {
            name: additionalInformation.name,
            latitude: Number(additionalInformation.lat),
            longitude: Number(additionalInformation.long),
            city: `${additionalInformation.name}, ${additionalInformation.admin1}, ${additionalInformation.country}`,
          };
          newNode.similarItems.push(similarNode);
        }
      }

      if (!exists) {
        parent.children.set(character, newNode);
      }

      parent = newNode;
    }
  };
  /**
   *
   * @param {string} term the search term
   * @returns a Promise that will resolve to the result list
   */
  search = (term, latitude, longitude) => {
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

        this.handleCompleteWord(node, result, term, latitude, longitude);

        if (isLastItemFromArray(term, t)) {
          this.traverse(node, result, term, latitude, longitude);
        }
      }

      resolve(result);
    });
  };

  /**
   *
   * @param {TrieNode} node holds a reference to a node being traversed
   * @param {Array} result holds a reference to the list of cities resulting in similarities
   * @param {string} term the search term
   * @void
   */

  traverse = (node, result, term, latitude, longitude) => {
    if (!node) return;
    if (!node.children) return;

    for (const value of node.children.values()) {
      value.flags.push(FLAGS.SIMILAR_WORD);

      this.handleCompleteWord(value, result, term, latitude, longitude);

      if (value.children.size) {
        this.traverse(value, result, term, latitude, longitude);
      }
    }
  };

  /**
   *
   * @param {TrieNode} node holds a reference to the node that completed search
   * @param {Array} result holds a reference to the list of cities resulting in similarities
   * @param {string} term the search term
   */

  handleCompleteWord = (node, result, term, latitude, longitude) => {
    if (node.end) {
      this.validateLocation(node, latitude, longitude);
      const score = this.calculateScore(node, term);

      result.push({ score, ...node.information });
      node.similarItems.forEach((item) => {
        this.validateLocation(item, latitude, longitude);
        const scoreSimilarItems = this.calculateScore(item, term, score);
        result.push({ score: scoreSimilarItems, ...item.information });
      });
    }
  };
  /**
   *
   * @param {EVENT_TYPE} event type of the event that requires calculation of the score.
   * @returns number
   */
  calculateScore = (node, term, prev) => {
    const highestScore = prev || 1;
    const score = [...this.generalFlags, ...node.flags].reduce((a, b) => {
      const calc = FLAG_VALUES[b](term, node.information.name);
      return a + calc;
    }, highestScore);

    return parseFloat(score.toFixed(1));
  };

  validateLocation = (node, incomingLatitude, incomingLongitude) => {
    if (incomingLatitude && incomingLongitude) {
      const distance = getLocationBetweenTwoPoints(
        node,
        Number(incomingLatitude),
        Number(incomingLongitude)
      );

      const distanceKM = distance / 1000;

      if (distanceKM > distanceRangeKM) {
        node.flags.push(FLAGS.OUTSIDE_LOCATION);
      }
    }

    return 0;
  };
}
