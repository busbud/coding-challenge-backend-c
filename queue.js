/*
* Queue.js
* Queue which contains the timestamp as value / round robin
*/

var POPULARITY_SLOT = 60 * 60; // seconds

var Queue = function() {
    this.first = null;
    this.size = 0;
};

var Node = function(data) {
    this.data = data;
    this.next = null;
};

Queue.prototype.enqueue = function(data) {
    var node = new Node(data);
    if (!this.first){
        this.first = node;
    } else {
        n = this.first;
        while (n.next) {
            var now = Math.round(Date.now() /1000);
            var diff = now - n.data;
            if(diff > POPULARITY_SLOT) {
                this.first = n.next;
                this.size -= 1;
            }
            n = n.next;
        }
        n.next = node;
    }
    this.size += 1;
    return node;
};

Queue.prototype.dequeue = function() {
    temp = this.first;
    this.first = this.first.next;
    this.size -= 1;
    return temp;
};

Queue.prototype.last = function() {
    return this.first;
};

Queue.prototype.size = function() {
    return this.size;
};

if (module && module.exports){
    module.exports = Queue;
}