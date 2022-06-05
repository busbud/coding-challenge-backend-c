/**
 * The TrieNode class will be the representation of a node in the Trie tree.
 * key: string -> The character of a word
 * parent: Node -> holds a referece to the parent node.
 * children: Map -> will have all the characters that follow.
 * end: boolean -> will let me know if I'm at the end of a word.
 * similarItems -> if words are found to be similar
 *      if I reach the end of it, then I'll add it as a similar item containing the city information (latitude, longitude, province, etc)
 */
class TrieNode {
  key = null;
  parent = null;
  children = new Map();
  end = false;
  accuracy = 100;
  details = new Map();

  constructor(key) {
    this.key = key;
  }
}

export default TrieNode;
