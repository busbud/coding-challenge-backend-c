
//
// A splay tree is a self-balancing binary search tree with the
// additional property that recently accessed elements are quick
// to access again. It performs basic operations such as insertion,
// look-up and removal in O(log(n)) amortized time.
//
// Adapted from the V8 project. Copyright 2011 the V8 project
// authors. All rights reserved. Maintained by http://github.com/hij1nx
//

(function (exports, undefined) {

  function SplayTree() {
  };

  //
  // @type root {SplayTree.Node}
  // @private
  // Pointer to the root node of the tree.
  //
  SplayTree.prototype.root = null;

  //
  // ### @function isEmpty()
  // #### @return {boolean} Whether the tree is empty.
  //
  SplayTree.prototype.isEmpty = function() {
    return !this.root;
  };

  //
  // ### function insert(key, value)
  // #### @param key {number} key Key to insert into the tree.
  // #### @param value {*} value Value to insert into the tree.
  // Inserts a node into the tree with the specified key and value if
  // the tree does not already contain a node with the specified key. If
  // the value is inserted, it becomes the root of the tree.
  //
  SplayTree.prototype.insert = function(key, value) {
    if (this.isEmpty()) {
      this.root = new SplayTree.Node(key, value);
      return;
    }
    // Splay on the key to move the last node on the search path for
    // the key to the root of the tree.
    this.splay(key);
    if (this.root.key == key) {
      return;
    }
    var node = new SplayTree.Node(key, value);
    if (key > this.root.key) {
      node.left = this.root;
      node.right = this.root.right;
      this.root.right = null;
    } else {
      node.right = this.root;
      node.left = this.root.left;
      this.root.left = null;
    }
    this.root = node;
  };

  //
  // ### function remove(key)
  // #### @param {number} key Key to find and remove from the tree.
  // #### @return {SplayTree.Node} The removed node.
  // Removes a node with the specified key from the tree if the tree
  // contains a node with this key. The removed node is returned. If the
  // key is not found, an exception is thrown.
  //
  SplayTree.prototype.remove = function(key) {
    if (this.isEmpty()) {
      throw Error('Key not found: ' + key);
    }
    this.splay(key);
    if (this.root.key != key) {
      throw Error('Key not found: ' + key);
    }
    var removed = this.root;
    if (!this.root.left) {
      this.root = this.root.right;
    } else {
      var right = this.root.right;
      this.root = this.root.left;
      //
      // Splay to make sure that the new root has an empty right child.
      //
      this.splay(key);
      //
      // Insert the original right child as the right child of the new
      // root.
      //
      this.root.right = right;
    }
    return removed;
  };

  //
  // ### function find(key)
  // #### @param {number} key Key to find in the tree.
  // #### @return {SplayTree.Node} Node having the specified key.
  // Returns the node having the specified key or null if the
  // tree doesn't contain a node with the specified key.
  //
  SplayTree.prototype.find = function(key) {
    if (this.isEmpty()) {
      return null;
    }
    this.splay(key);
    return this.root.key == key ? this.root : null;
  };

  //
  // ### function(opt_startNode)
  // #### @return {SplayTree.Node} Node having the maximum key value.
  //
  SplayTree.prototype.findMax = function(opt_startNode) {
    if (this.isEmpty()) {
      return null;
    }
    var current = opt_startNode || this.root;
    while (current.right) {
      current = current.right;
    }
    return current;
  };

  //
  // ### function findGreatestLessThan(key)
  // #### @return {SplayTree.Node} Node having the maximum key value that
  //     is less than the specified key value.
  //
  SplayTree.prototype.findGreatestLessThan = function(key) {
    if (this.isEmpty()) {
      return null;
    }

    //
    // Splay on the key to move the node with the given key or the last
    // node on the search path to the top of the tree.
    //
    this.splay(key);

    //
    // Now the result is either the root node or the greatest node in
    // the left subtree.
    //
    if (this.root.key < key) {
      return this.root;
    } else if (this.root.left) {
      return this.findMax(this.root.left);
    } else {
      return null;
    }
  };

  //
  // ### function exportKeys()
  // #### @return {Array<*>} An array containing all the keys of tree's nodes.
  //
  SplayTree.prototype.exportKeys = function() {
    var result = [];
    if (!this.isEmpty()) {
      this.root.traverse(function(node) { result.push(node.key); });
    }
    return result;
  };

  //
  // This is the simplified top-down splaying algorithm from:
  // * "Self-adjusting Binary Search Trees" by Sleator and Tarjan
  //
  SplayTree.prototype.splay = function(key) {
    if (this.isEmpty()) {
      return;
    }

    var stub, left, right;
    stub = left = right = new SplayTree.Node(null, null);

    var root = this.root;

    while (true) {
      if (key < root.key) {

        if (!root.left) {
          break;
        }

        if (key < root.left.key) {
          // Rotate right.
          var tmp = root.left;
          root.left = tmp.right;
          tmp.right = root;
          root = tmp;
          if (!root.left) {
            break;
          }
        }

        // Link right.
        right.left = root;
        right = root;
        root = root.left;

      } else if (key > root.key) {
        if (!root.right) {
          break;
        }
        if (key > root.right.key) {
          // Rotate left.
          var tmp = root.right;
          root.right = tmp.left;
          tmp.left = root;
          root = tmp;
          if (!root.right) {
            break;
          }
        }
        // Link left.
        left.right = root;
        left = root;
        root = root.right;
      } else {
        break;
      }
    }

    // Assemble.
    left.right = root.left;
    right.left = root.right;
    root.left = stub.right;
    root.right = stub.left;
    this.root = root;
  };

  //
  // ### @Function Node()
  // #### @param {number} key Key.
  // #### @param {*} value Value.
  // Constructs a Splay tree node.
  //
  SplayTree.Node = function(key, value) {
    this.key = key;
    this.value = value;
  };

  //
  // #### @type {SplayTree.Node}
  //
  SplayTree.Node.prototype.left = null;

  //
  // #### @type {SplayTree.Node}
  //
  SplayTree.Node.prototype.right = null;

  //
  // ### function traverse()
  // #### @param {function(SplayTree.Node)} f Visitor function.
  // #### @private
  // Performs an ordered traversal of the subtree starting at
  // this SplayTree.Node.
  //
  SplayTree.Node.prototype.traverse = function(f) {
    var current = this;
    while (current) {
      var left = current.left;
      if (left) left.traverse(f);
      f(current);
      current = current.right;
    }
  };

  module.exports = SplayTree;

}(module));
