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
        trie.insert(cityInformation.ascii, cityInformation);

        // index alt_names
        const altNames = cityInformation.alt_name
          ?.split(",")
          .map((r) => r.replace(/[^a-zA-Z ]/g, " ").trim())
          .filter((p) => p);

        if (altNames?.length) {
          altNames.forEach((alt) => {
            trie.insert(alt, cityInformation);
          });
        }
      });

      resolve(trie);
    });
  };
}
