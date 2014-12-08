//Trie data structure http://en.wikipedia.org/wiki/Trie
var _=require('lodash');

module.exports = Trie;
function Trie () {
	this._root_node=new TrieNode('');
}
Trie.prototype.addEntry = function(entry_string,entry_obj) {//Traverse trie and add entry_obj at point corresponding to entry_string
	var current_node=this._root_node;
	for (var i=0;i<entry_string.length;i++) {
		current_node=current_node.getChild(entry_string.charAt(i));
	}
	current_node.addEntry(entry_obj);
};
Trie.prototype.retrievePartialMatches = function(search_string) {//Retrieves an array of entries corresponding to search_string and all child nodes (ie all exact matches and matches to initial substring)
	var search_node=this._retrieveNode(search_string);
	if (!search_node) {
		return [];
	}
	return search_node.getDescendantEntries();
};

Trie.prototype._retrieveNode = function(node_string) {//Retrieves node corresponding to position of node_string in tree, returns null if none exists
	var current_node=this._root_node;
	for (var i=0;i<node_string.length;i++) {
		current_node=current_node.getExistingChild(node_string.charAt(i));
		if (!current_node) {
			return null;
		}
	}
	return current_node;
};

function TrieNode(node_char) {
	this.node_char=node_char;
	this.children=[];//Child nodes
	this.entries=[];//Entries corresponding to this Trie node
}
TrieNode.prototype.getChild = function(child_char) {//Returns child of node corresponding to child_char; creates it if it does not exist
	if (!_.has(this.children,child_char)) {
		this._addChild(new TrieNode(child_char),child_char);
	}
	return this.children[child_char];
};
TrieNode.prototype.getExistingChild = function(child_char) {//Returns child of node corresponding to child_char, returns undefined if it does not exist
	return this.children[child_char];
};
TrieNode.prototype.getDescendantEntries = function(results) {//Get entries on this node and all descendant nodes (ie for a string, get all exact matches and matches to initial substring)
	if (!results) {
		results=[];
	}
	Array.prototype.push.apply(results,this.entries);
	for (var key in this.children) {
		var child=this.children[key];
		child.getDescendantEntries(results);
	}
	return results;
};

TrieNode.prototype._addChild = function(child_node, child_char) {
	this.children[child_char]=child_node;
};
TrieNode.prototype.addEntry = function(node_entry) {
	this.entries.push(node_entry);
};