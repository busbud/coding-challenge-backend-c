/**
 * The TrieNode class will be the representation of a node in the Trie tree.
 * key: string -> The key of the node. In our case this will be each character of the word to index.
 * parent: Node -> holds a referece to the parent node.
 * children: Map -> All following characters of a specific character.
 * end: boolean -> Flag to determine if that node forms a complete word.
 * details: Map -> Stores all the similar words that end at that node. Having a map will not allow repeated items to be added
 * accuracy: number, default = 100 -> this is a value that represents how accurate a word is vs the searched term
 */
export default class TrieNode {
  key = null;
  parent = null;
  children = new Map();
  end = false;
  details = new Map();
  accuracy = 100;

  constructor(key) {
    this.key = key;
  }
}
