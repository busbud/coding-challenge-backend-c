/*
* Tree.js
* Ternary tree which contains the lexical
*/

var GeoPoint = require('geopoint');
var sortBy = require('sort-by');
var Queue = require("./queue.js");

var Tree = function (caseSensitive) {
    this.root = undefined;
    this.caseSensitive = Boolean(caseSensitive);
}

Tree.prototype.add = function (str, data) {
    if (this.root === undefined){
        this.root = this.createNode(str[0]);
    }
    this.innerAdd(str, 0, this.root, data);
};

Tree.prototype.innerAdd = function (str, pos, node, data) {
    var currentCar = str[pos];
    var nextCar = str[pos + 1];

    if (!this.caseSensitive){
        currentCar = currentCar.toLowerCase();
        if (nextCar !== undefined){
            nextCar = nextCar.toLowerCase();
        }
    }
    if (currentCar < node.car) {
        node = this.addLeft(node, currentCar);
        this.innerAdd(str, pos, node, data);
    } else if (currentCar > node.car) {
        node = this.addRight(node, currentCar);
        this.innerAdd(str, pos, node, data);
    } else if (nextCar === undefined) {
        node.isEnd = true;
        if (node.data === undefined){
            node.data = [];
        }
        node.data.push(data);
        node.word = str;
    } else {
        node = this.addCenter(node, nextCar);
        this.innerAdd(str, pos+1, node, data);
    }
};

Tree.prototype.search = function(str, position, matcher) {
    if (this.root === undefined) {
        throw new Error("Tree should be initialized.");
    }
    var res = [];
    var _this = this;
    this.traverse(this.root, str, 0, function(node, str, pos, parent){
        if(pos === str.length){
            if (node !== undefined){
                _this.searchNodes(node, position, res, matcher);
            } else {
                _this.searchNodes(parent, position, res, matcher);
            }
        }
    });
    return this.compute(res);
}

Tree.prototype.traverse = function(node, str, pos, fn){
    while(node !== undefined) {
        var currentCar = str[pos];
        var currentNode = node;
        if (!this.caseSensitive && currentCar !== undefined){
            currentCar = currentCar.toLowerCase();
        }
        if (currentCar < node.car) {
            node = node.left;
        } else if (currentCar > node.car) {
            node = node.right;
        } else {
            node = node.center;
            pos++;
        }
        fn(node, str, pos, currentNode);
    }
};

Tree.prototype.searchNodes = function(node, position, res, matcher){
    if (res === undefined){
        res = [];
    }
    if(node !== undefined) {
        if (node.isEnd){
            for(var i = 0; i < node.data.length; i++) {
                if (matcher === undefined || matcher(node.data)){
                    var n = node.data[i];
                    var now = Math.round(Date.now() /1000);

                    if(n.popularity == undefined) {
                        var queue = new Queue();
                        queue.enqueue(now);
                        n.popularity = queue;
                    } else {
                        n.popularity.enqueue(now);
                    }
                    n.count++;
                    if(position !== undefined) {
                        n.distance = this.computeDistance(n.position, position);
                    }
                    res.push(n);
                }
            }
        }
        this.searchNodes(node.left, position, res, matcher);
        this.searchNodes(node.right, position, res, matcher);
        this.searchNodes(node.center, position, res, matcher);
    }
};

Tree.prototype.computeDistance = function(nPosition, position){
    var pointNode = new GeoPoint(Number(nPosition.latitude), Number(nPosition.longitude));
    var pointArgs = new GeoPoint(Number(position.latitude), Number(position.longitude));
    return pointNode.distanceTo(pointArgs, true);
};

Tree.prototype.compute = function(nodes){
    var resNodes = [];
    var maxCount = 0, maxPopularity = 0;
    for(var i = 0; i < nodes.length; i++) {
        var node = nodes[i];
        if(node.count > maxCount) {
            maxCount = node.count;
        }
        if(node.popularity.size > maxPopularity) {
            maxPopularity = node.popularity.size;
        }
    }
    for(var j = 0; j < nodes.length; j++) {
        var node = nodes[j];
        var scoreCount = node.count / maxCount;
        var scorePopularity = node.popularity.size / maxPopularity;
        var score = 0;
        if(node.distance !== -1) {
            var scoreDistance = 0;
            if(node.distance < 500) {
                scoreDistance = 1;
            } else if(node.distance >= 500 && node.distance < 1000){
                scoreDistance = 0.7;
            } else if(node.distance >= 1000){
                scoreDistance = 0.3;
            }
            score = scoreCount * (1/3) + scorePopularity * (1/3) + scorePopularity * (1/3);
        } else {
            score = scoreCount * (1/2) + scorePopularity * (1/2);
        }

        var res = {
            name : node.name,
            latitude : node.position.latitude,
            longitude : node.position.longitude,
            score : score.toFixed(2)
        };
        resNodes.push(res);
    }
    return resNodes.sort(sortBy('-score', 'name'));;
};

Tree.prototype.createNode = function(car){
    if (!this.caseSensitive){
        car = car.toLowerCase();
    }
    return new TreeNode(car);
};


Tree.prototype.addLeft = function(node, car) {
    if (node.left === undefined) {
        node.left = this.createNode(car);
    }
    return node.left;
};

Tree.prototype.addRight = function(node, car) {
    if (node.right === undefined) {
        node.right = this.createNode(car);
    }
    return node.right;
};

Tree.prototype.addCenter = function(node, car) {
    if (node.center === undefined) {
        node.center = this.createNode(car);
    }
    return node.center;
};

var TreeNode = function (car) {
    this.car = car;
    this.isEnd = false;
};

if (module && module.exports){
    module.exports = Tree;
}