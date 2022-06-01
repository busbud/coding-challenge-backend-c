import TrieNode from "./node.js";

export default class Trie {
  constructor() {
    this.root = new TrieNode(null);
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
      if (word === "London") {
        console.log("london");
      }

      if (c === word.length - 1) {
        newNode.end = true;
        if (!newNode.nodeInformation) {
          newNode.nodeInformation = {
            latitude: additionalInformation.lat,
            longitue: additionalInformation.long,
            city: `${additionalInformation.name}, ${additionalInformation.admin1}, ${additionalInformation.country}`,
          };
        } else {
          newNode.similarItems.push({
            latitude: additionalInformation.lat,
            longitue: additionalInformation.long,
            city: `${additionalInformation.name}, ${additionalInformation.admin1}, ${additionalInformation.country}`,
          });
        }
      }

      if (!exists) {
        // if it's the last character I want to mark it as a complete word.
        // and add the word information
        // TODO: Create a helper function to know if it's last.
        // TODO: Create a helper function to set the node information

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
      let score = 1;

      for (let t = 0; t < term.length; t++) {
        const character = term[t].toLowerCase();
        const exists = parent.children.has(character);

        if (!exists) {
          score -= 0.05;
        }

        if (exists) {
          const node = parent.children.get(character);
          parent = node;

          if (node.end) {
            const scoredSimilarItems = node.similarItems.map((s) => {
              {
                score: score, { ...s };
              }
            });
            result.push([
              { score: score, ...node.nodeInformation },
              ...scoredSimilarItems,
            ]);
          }

          if (t === term.length - 1) {
            const suggestions = this.traverse(node, score, result);
            result.push(...suggestions);
          }
        }
      }

      resolve(result);
    });
  };

  traverse = (node, remainingScore, result) => {
    if (!node) return [];
    if (!node.children) return [];

    let score = remainingScore;

    for (const value of node.children.values()) {
      if (!value.end) {
        score -= 0.05;
      }
      // TODO: fix duplication of results.
      if (value.end) {
        const scoredSimilarItems = value.similarItems.map((s) => {
          return {
            score: score,
            ...s,
          };
        });
        result.push([
          { score: score, ...value.nodeInformation },
          ...scoredSimilarItems,
        ]);
      }

      if (value.children) {
        return this.traverse(value, score, result);
      }
    }

    return result;
  };
}
