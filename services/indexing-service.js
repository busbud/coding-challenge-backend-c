import Trie from "./trie/trie.js";

export default class IndexingService {
  constructor(data) {
    this.data = data;
  }

  index = () => {
    return new Promise((resolve, reject) => {
      if (!this.data) reject("no data to index");
      if (!this.data.length) reject("no data to index");

      const trie = new Trie();

      this.data.forEach((cityInformation) => {
        return trie.insert(cityInformation.name, cityInformation);
      });

      resolve(trie);
    });
  };
}
