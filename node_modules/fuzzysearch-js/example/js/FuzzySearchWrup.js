(function (modules) {
    var cache = {}, require = function (id) {
            var module = cache[id];
            if (!module) {
                module = cache[id] = {};
                var exports = module.exports = {};
                modules[id].call(exports, require, module, exports, window);
            }
            return module.exports;
        };
    require('0');
}({
    '0': function (require, module, exports, global) {
        'Use Strict';
        var prime = require('1');
        var onDomReady = require('2');
        var $$ = require('4');
        require('9');
        require('3');
        require('c');
        require('h');
        require('e');
        require('i');
        var zen = require('j');
        var number = require('k');
        var mixin = require('m');
        var bound = require('n');
        var FuzzySearch = require('q');
        var LevenshteinFS = require('u');
        var Sift3FS = require('x');
        var IndexOfFS = require('z');
        var WordCountFS = require('10');
        var Arr = require('5');
        onDomReady(function () {
            new Main();
        });
        var Main = prime({
                constructor: function () {
                    this.searchField = $$('#searchfield');
                    this.fuzzySearch = new FuzzySearch(fsData, {
                        'caseSensitive': false,
                        'termPath': ''
                    });
                    this.fuzzySearch.addModule(LevenshteinFS({
                        'maxDistanceTolerance': 3,
                        'factor': 3
                    }));
                    this.fuzzySearch.addModule(IndexOfFS({
                        'minTermLength': 3,
                        'maxIterations': 500,
                        'factor': 3
                    }));
                    this.fuzzySearch.addModule(WordCountFS({
                        'maxWordTolerance': 3,
                        'factor': 1
                    }));
                    $$('#maxscore').text(this.fuzzySearch.getMaximumScore());
                    this.loadEvents();
                    this.displayData();
                },
                loadEvents: function () {
                    this.searchField.on('keyup', this.bound('search'));
                },
                displayData: function () {
                    var container = $$('#data');
                    Arr.forEach(fsData, function (data) {
                        zen('li').text(data).insert(container);
                    });
                },
                displayResults: function (results, container) {
                    var container = container;
                    $$(container).empty();
                    Arr.forEach(results, function (result) {
                        zen('li').text(result.value + ' (total: ' + number.round(result.score) + ')').insert(container);
                    });
                },
                search: function () {
                    var term = this.searchField.value();
                    var levResults = this.fuzzySearch.search(term);
                    this.displayResults(levResults, $$('#results'));
                }
            });
        mixin(Main, bound);
    },
    '1': function (require, module, exports, global) {
        'use strict';
        var has = function (self, key) {
            return Object.hasOwnProperty.call(self, key);
        };
        var each = function (object, method, context) {
            for (var key in object)
                if (method.call(context, object[key], key, object) === false)
                    break;
            return object;
        };
        if (!{ valueOf: 0 }.propertyIsEnumerable('valueOf')) {
            var buggy = 'constructor,toString,valueOf,hasOwnProperty,isPrototypeOf,propertyIsEnumerable,toLocaleString'.split(',');
            var proto = Object.prototype;
            each = function (object, method, context) {
                for (var key in object)
                    if (method.call(context, object[key], key, object) === false)
                        return object;
                for (var i = 0; key = buggy[i]; i++) {
                    var value = object[key];
                    if ((value !== proto[key] || has(object, key)) && method.call(context, value, key, object) === false)
                        break;
                }
                return object;
            };
        }
        var create = Object.create || function (self) {
                var constructor = function () {
                };
                constructor.prototype = self;
                return new constructor();
            };
        var getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
        var define = Object.defineProperty;
        try {
            var obj = { a: 1 };
            getOwnPropertyDescriptor(obj, 'a');
            define(obj, 'a', { value: 2 });
        } catch (e) {
            getOwnPropertyDescriptor = function (object, key) {
                return { value: object[key] };
            };
            define = function (object, key, descriptor) {
                object[key] = descriptor.value;
                return object;
            };
        }
        var implement = function (proto) {
            each(proto, function (value, key) {
                if (key !== 'constructor' && key !== 'define' && key !== 'inherits')
                    this.define(key, getOwnPropertyDescriptor(proto, key) || {
                        writable: true,
                        enumerable: true,
                        configurable: true,
                        value: value
                    });
            }, this);
            return this;
        };
        var prime = function (proto) {
            var superprime = proto.inherits;
            var constructor = has(proto, 'constructor') ? proto.constructor : superprime ? function () {
                    return superprime.apply(this, arguments);
                } : function () {
                };
            if (superprime) {
                var superproto = superprime.prototype;
                var cproto = constructor.prototype = create(superproto);
                constructor.parent = superproto;
                cproto.constructor = constructor;
            }
            constructor.define = proto.define || superprime && superprime.define || function (key, descriptor) {
                define(this.prototype, key, descriptor);
                return this;
            };
            constructor.implement = implement;
            return constructor.implement(proto);
        };
        prime.has = has;
        prime.each = each;
        prime.create = create;
        prime.define = define;
        module.exports = prime;
    },
    '2': function (require, module, exports, global) {
        'use strict';
        var $ = require('3');
        var readystatechange = 'onreadystatechange' in document, shouldPoll = false, loaded = false, readys = [], checks = [], ready = null, timer = null, test = document.createElement('div'), doc = $(document), win = $(window);
        var domready = function () {
            if (timer)
                timer = clearTimeout(timer);
            if (!loaded) {
                if (readystatechange)
                    doc.off('readystatechange', check);
                doc.off('DOMContentLoaded', domready);
                win.off('load', domready);
                loaded = true;
                for (var i = 0; ready = readys[i++];)
                    ready();
            }
            return loaded;
        };
        var check = function () {
            for (var i = checks.length; i--;)
                if (checks[i]())
                    return domready();
            return false;
        };
        var poll = function () {
            clearTimeout(timer);
            if (!check())
                timer = setTimeout(poll, 1000 / 60);
        };
        if (document.readyState) {
            var complete = function () {
                return !!/loaded|complete/.test(document.readyState);
            };
            checks.push(complete);
            if (!complete()) {
                if (readystatechange)
                    doc.on('readystatechange', check);
                else
                    shouldPoll = true;
            } else {
                domready();
            }
        }
        if (test.doScroll) {
            var scrolls = function () {
                try {
                    test.doScroll();
                    return true;
                } catch (e) {
                }
                return false;
            };
            if (!scrolls()) {
                checks.push(scrolls);
                shouldPoll = true;
            }
        }
        if (shouldPoll)
            poll();
        doc.on('DOMContentLoaded', domready);
        win.on('load', domready);
        module.exports = function (ready) {
            loaded ? ready() : readys.push(ready);
            return null;
        };
    },
    '3': function (require, module, exports, global) {
        'use strict';
        var $ = require('4'), prime = require('1'), Emitter = require('8');
        var html = document.documentElement;
        var addEventListener = html.addEventListener ? function (node, event, handle) {
                node.addEventListener(event, handle, false);
                return handle;
            } : function (node, event, handle) {
                node.attachEvent('on' + event, handle);
                return handle;
            };
        var removeEventListener = html.removeEventListener ? function (node, event, handle) {
                node.removeEventListener(event, handle, false);
            } : function (node, event, handle) {
                node.detachEvent('on' + event, handle);
            };
        $.implement({
            on: function (event, handle) {
                this.forEach(function (node) {
                    var self = $(node);
                    Emitter.prototype.on.call(self, event, handle);
                    var domListeners = self._domListeners || (self._domListeners = {});
                    if (!domListeners[event])
                        domListeners[event] = addEventListener(node, event, function (e) {
                            self.emit(event, e || window.event);
                        });
                });
                return this;
            },
            off: function (event, handle) {
                this.forEach(function (node) {
                    var self = $(node);
                    var domListeners = self._domListeners, domEvent, listeners = self._listeners, events;
                    if (domListeners && (domEvent = domListeners[event]) && listeners && (events = listeners[event])) {
                        Emitter.prototype.off.call(self, event, handle);
                        var empty = true, k, l;
                        for (k in events) {
                            empty = false;
                            break;
                        }
                        if (empty) {
                            removeEventListener(node, event, domEvent);
                            delete domListeners[event];
                            for (l in domListeners)
                                empty = false;
                            if (empty)
                                delete self._domListeners;
                        }
                    }
                });
                return this;
            },
            emit: function (event) {
                var args = arguments;
                this.forEach(function (node) {
                    Emitter.prototype.emit.apply($(node), args);
                });
                return this;
            }
        });
        module.exports = $;
    },
    '4': function (require, module, exports, global) {
        'use strict';
        var prime = require('1'), array = require('5').prototype;
        var uniqueIndex = 0;
        var uniqueID = function (n) {
            return n === global ? 'global' : n.uniqueNumber || (n.uniqueNumber = 'n:' + (uniqueIndex++).toString(36));
        };
        var instances = {};
        var $ = prime({
                constructor: function $(n, context) {
                    if (n == null)
                        return this && this.constructor === $ ? new elements() : null;
                    var self = n;
                    if (n.constructor !== elements) {
                        self = new elements();
                        var uid;
                        if (typeof n === 'string') {
                            if (!self.search)
                                return null;
                            self[self.length++] = context || document;
                            return self.search(n);
                        }
                        if (n.nodeType || n === global) {
                            self[self.length++] = n;
                        } else if (n.length) {
                            var uniques = {};
                            for (var i = 0, l = n.length; i < l; i++) {
                                var nodes = $(n[i], context);
                                if (nodes && nodes.length)
                                    for (var j = 0, k = nodes.length; j < k; j++) {
                                        var node = nodes[j];
                                        uid = uniqueID(node);
                                        if (!uniques[uid]) {
                                            self[self.length++] = node;
                                            uniques[uid] = true;
                                        }
                                    }
                            }
                        }
                    }
                    if (!self.length)
                        return null;
                    if (self.length === 1) {
                        uid = uniqueID(self[0]);
                        return instances[uid] || (instances[uid] = self);
                    }
                    return self;
                }
            });
        var elements = prime({
                inherits: $,
                constructor: function elements() {
                    this.length = 0;
                },
                unlink: function () {
                    return this.map(function (node, i) {
                        delete instances[uniqueID(node)];
                        return node;
                    });
                },
                forEach: array.forEach,
                map: array.map,
                filter: array.filter,
                every: array.every,
                some: array.some
            });
        module.exports = $;
    },
    '5': function (require, module, exports, global) {
        'use strict';
        var array = require('6')['array'];
        var names = ('pop,push,reverse,shift,sort,splice,unshift,concat,join,slice,toString,indexOf,lastIndexOf,forEach,every,some' + ',filter,map,reduce,reduceRight').split(',');
        for (var methods = {}, i = 0, name, method; name = names[i++];)
            if (method = Array.prototype[name])
                methods[name] = method;
        if (!methods.filter)
            methods.filter = function (fn, context) {
                var results = [];
                for (var i = 0, l = this.length >>> 0; i < l; i++)
                    if (i in this) {
                        var value = this[i];
                        if (fn.call(context, value, i, this))
                            results.push(value);
                    }
                return results;
            };
        if (!methods.indexOf)
            methods.indexOf = function (item, from) {
                for (var l = this.length >>> 0, i = from < 0 ? Math.max(0, l + from) : from || 0; i < l; i++) {
                    if (i in this && this[i] === item)
                        return i;
                }
                return -1;
            };
        if (!methods.map)
            methods.map = function (fn, context) {
                var length = this.length >>> 0, results = Array(length);
                for (var i = 0, l = length; i < l; i++) {
                    if (i in this)
                        results[i] = fn.call(context, this[i], i, this);
                }
                return results;
            };
        if (!methods.every)
            methods.every = function (fn, context) {
                for (var i = 0, l = this.length >>> 0; i < l; i++) {
                    if (i in this && !fn.call(context, this[i], i, this))
                        return false;
                }
                return true;
            };
        if (!methods.some)
            methods.some = function (fn, context) {
                for (var i = 0, l = this.length >>> 0; i < l; i++) {
                    if (i in this && fn.call(context, this[i], i, this))
                        return true;
                }
                return false;
            };
        if (!methods.forEach)
            methods.forEach = function (fn, context) {
                for (var i = 0, l = this.length >>> 0; i < l; i++) {
                    if (i in this)
                        fn.call(context, this[i], i, this);
                }
            };
        var toString = Object.prototype.toString;
        array.isArray = Array.isArray || function (self) {
            return toString.call(self) === '[object Array]';
        };
        module.exports = array.implement(methods);
    },
    '6': function (require, module, exports, global) {
        'use strict';
        var prime = require('1'), type = require('7');
        var slice = Array.prototype.slice;
        var ghost = prime({
                constructor: function ghost(self) {
                    this.valueOf = function () {
                        return self;
                    };
                    this.toString = function () {
                        return self + '';
                    };
                    this.is = function (object) {
                        return self === object;
                    };
                }
            });
        var shell = function (self) {
            if (self == null || self instanceof ghost)
                return self;
            var g = shell[type(self)];
            return g ? new g(self) : self;
        };
        var register = function () {
            var g = prime({ inherits: ghost });
            return prime({
                constructor: function (self) {
                    return new g(self);
                },
                define: function (key, descriptor) {
                    var method = descriptor.value;
                    this[key] = function (self) {
                        return arguments.length > 1 ? method.apply(self, slice.call(arguments, 1)) : method.call(self);
                    };
                    g.prototype[key] = function () {
                        return shell(method.apply(this.valueOf(), arguments));
                    };
                    prime.define(this.prototype, key, descriptor);
                    return this;
                }
            });
        };
        for (var types = 'string,number,array,object,date,function,regexp'.split(','), i = types.length; i--;)
            shell[types[i]] = register();
        module.exports = shell;
    },
    '7': function (require, module, exports, global) {
        'use strict';
        var toString = Object.prototype.toString, types = /number|object|array|string|function|date|regexp|boolean/;
        var type = function (object) {
            if (object == null)
                return 'null';
            var string = toString.call(object).slice(8, -1).toLowerCase();
            if (string === 'number' && isNaN(object))
                return 'null';
            if (types.test(string))
                return string;
            return 'object';
        };
        module.exports = type;
    },
    '8': function (require, module, exports, global) {
        'use strict';
        var prime = require('1'), slice = Array.prototype.slice;
        var EID = 0;
        module.exports = prime({
            on: function (event, fn) {
                var listeners = this._listeners || (this._listeners = {}), events = listeners[event] || (listeners[event] = {});
                for (var k in events)
                    if (events[k] === fn)
                        return this;
                events[(EID++).toString(36)] = fn;
                return this;
            },
            off: function (event, fn) {
                var listeners = this._listeners, events, key, length = 0;
                if (listeners && (events = listeners[event])) {
                    for (var k in events) {
                        length++;
                        if (key == null && events[k] === fn)
                            key = k;
                        if (key && length > 1)
                            break;
                    }
                    if (key) {
                        delete events[key];
                        if (length === 1) {
                            delete listeners[event];
                            for (var l in listeners)
                                return this;
                            delete this._listeners;
                        }
                    }
                }
                return this;
            },
            emit: function (event) {
                var listeners = this._listeners, events;
                if (listeners && (events = listeners[event])) {
                    var args = arguments.length > 1 ? slice.call(arguments, 1) : [];
                    for (var k in events)
                        events[k].apply(this, args);
                }
                return this;
            }
        });
    },
    '9': function (require, module, exports, global) {
        'use strict';
        var $ = require('4'), string = require('a'), array = require('5');
        $.implement({
            setAttribute: function (name, value) {
                this.forEach(function (node) {
                    node.setAttribute(name, value);
                });
                return this;
            },
            getAttribute: function (name) {
                var attr = this[0].getAttributeNode(name);
                return attr && attr.specified ? attr.value : null;
            },
            hasAttribute: function (name) {
                var node = this[0];
                if (node.hasAttribute)
                    return node.hasAttribute(name);
                var attr = node.getAttributeNode(name);
                return !!(attr && attr.specified);
            },
            removeAttribute: function (name) {
                this.forEach(function (node) {
                    var attr = node.getAttributeNode(name);
                    if (attr)
                        node.removeAttributeNode(attr);
                });
                return this;
            }
        });
        var accessors = {};
        array.forEach('type,value,name,href,title,id'.split(','), function (name) {
            accessors[name] = function (value) {
                if (value !== undefined) {
                    this.forEach(function (node) {
                        node[name] = value;
                    });
                    return this;
                }
                return this[0][name];
            };
        });
        array.forEach('checked,disabled,selected'.split(','), function (name) {
            accessors[name] = function (value) {
                if (value !== undefined) {
                    this.forEach(function (node) {
                        node[name] = !!value;
                    });
                    return this;
                }
                return !!this[0][name];
            };
        });
        var classes = function (className) {
            var classNames = string.clean(className).split(' '), uniques = {};
            return array.filter(classNames, function (className) {
                if (className !== '' && !uniques[className])
                    return uniques[className] = className;
            }).sort();
        };
        accessors.className = function (className) {
            if (className !== undefined) {
                this.forEach(function (node) {
                    node.className = classes(className).join(' ');
                });
                return this;
            }
            return classes(this[0].className).join(' ');
        };
        $.implement({
            attribute: function (name, value) {
                var accessor = accessors[name];
                if (accessor)
                    return accessor.call(this, value);
                if (value != null)
                    return this.setAttribute(name, value);
                if (value === null)
                    return this.removeAttribute(name);
                if (value === undefined)
                    return this.getAttribute(name);
            }
        });
        $.implement(accessors);
        $.implement({
            check: function () {
                return this.checked(true);
            },
            uncheck: function () {
                return this.checked(false);
            },
            disable: function () {
                return this.disabled(true);
            },
            enable: function () {
                return this.disabled(false);
            },
            select: function () {
                return this.selected(true);
            },
            deselect: function () {
                return this.selected(false);
            }
        });
        $.implement({
            classNames: function () {
                return classes(this[0].className);
            },
            hasClass: function (className) {
                return array.indexOf(this.classNames(), className) > -1;
            },
            addClass: function (className) {
                this.forEach(function (node) {
                    var nodeClassName = node.className;
                    var classNames = classes(nodeClassName + ' ' + className).join(' ');
                    if (nodeClassName != classNames)
                        node.className = classNames;
                });
                return this;
            },
            removeClass: function (className) {
                this.forEach(function (node) {
                    var classNames = classes(node.className);
                    array.forEach(classes(className), function (className) {
                        var index = array.indexOf(classNames, className);
                        if (index > -1)
                            classNames.splice(index, 1);
                    });
                    node.className = classNames.join(' ');
                });
                return this;
            }
        });
        $.prototype.toString = function () {
            var tag = this.tag(), id = this.id(), classes = this.classNames();
            var str = tag;
            if (id)
                str += '#' + id;
            if (classes.length)
                str += '.' + classes.join('.');
            return str;
        };
        var textProperty = document.createElement('div').textContent == null ? 'innerText' : 'textContent';
        $.implement({
            tag: function () {
                return this[0].tagName.toLowerCase();
            },
            html: function (html) {
                if (html != null) {
                    this.forEach(function (node) {
                        node.innerHTML = html;
                    });
                    return this;
                }
                return this[0].innerHTML;
            },
            text: function (text) {
                if (text != undefined) {
                    this.forEach(function (node) {
                        node[textProperty] = text;
                    });
                    return this;
                }
                return this[0][textProperty];
            }
        });
        module.exports = $;
    },
    'a': function (require, module, exports, global) {
        'use strict';
        var string = require('b');
        string.implement({
            clean: function () {
                return string.trim((this + '').replace(/\s+/g, ' '));
            },
            camelize: function () {
                return (this + '').replace(/-\D/g, function (match) {
                    return match.charAt(1).toUpperCase();
                });
            },
            hyphenate: function () {
                return (this + '').replace(/[A-Z]/g, function (match) {
                    return '-' + match.toLowerCase();
                });
            },
            capitalize: function () {
                return (this + '').replace(/\b[a-z]/g, function (match) {
                    return match.toUpperCase();
                });
            },
            escape: function () {
                return (this + '').replace(/([-.*+?^${}()|[\]\/\\])/g, '\\$1');
            },
            number: function () {
                return parseFloat(this);
            }
        });
        if (typeof JSON !== 'undefined')
            string.implement({
                decode: function () {
                    return JSON.parse(this);
                }
            });
        module.exports = string;
    },
    'b': function (require, module, exports, global) {
        'use strict';
        var string = require('6')['string'];
        var names = ('charAt,charCodeAt,concat,contains,endsWith,indexOf,lastIndexOf,localeCompare,match,replace,search,slice,split' + ',startsWith,substr,substring,toLocaleLowerCase,toLocaleUpperCase,toLowerCase,toString,toUpperCase,trim,valueOf').split(',');
        for (var methods = {}, i = 0, name, method; name = names[i++];)
            if (method = String.prototype[name])
                methods[name] = method;
        if (!methods.trim)
            methods.trim = function () {
                return (this + '').replace(/^\s+|\s+$/g, '');
            };
        module.exports = string.implement(methods);
    },
    'c': function (require, module, exports, global) {
        'use strict';
        var $ = require('3'), Map = require('d');
        require('e');
        $.implement({
            delegate: function (event, selector, handle) {
                this.forEach(function (node) {
                    var self = $(node);
                    var delegation = self._delegation || (self._delegation = {}), events = delegation[event] || (delegation[event] = {}), map = events[selector] || (events[selector] = new Map());
                    var action = function (e) {
                        var target = $(e.target), match = target.matches(selector) ? target : target.parent(selector);
                        if (match)
                            handle.call(self, e, match);
                    };
                    map.set(handle, action);
                    self.on(event, action);
                });
                return this;
            },
            undelegate: function (event, selector, handle) {
                this.forEach(function (node) {
                    var self = $(node), delegation, events, map;
                    if (!(delegation = self._delegation) || !(events = delegation[event]) || !(map = events[selector]))
                        return;
                    var action = map.get(handle);
                    if (action) {
                        self.off(event, action);
                        map.remove(handle);
                        if (!map.count())
                            delete events[selector];
                        var e1 = true, e2 = true, x;
                        for (x in events) {
                            e1 = false;
                            break;
                        }
                        if (e1)
                            delete delegation[event];
                        for (x in delegation) {
                            e2 = false;
                            break;
                        }
                        if (!e2)
                            delete self._delegation;
                    }
                });
                return this;
            }
        });
        module.exports = $;
    },
    'd': function (require, module, exports, global) {
        'use strict';
        var prime = require('1'), array = require('5');
        var Map = prime({
                constructor: function () {
                    if (!this || this.constructor !== Map)
                        return new Map();
                    this.length = 0;
                    this._values = [];
                    this._keys = [];
                },
                set: function (key, value) {
                    var index = array.indexOf(this._keys, key);
                    if (index === -1) {
                        this._keys.push(key);
                        this._values.push(value);
                        this.length++;
                    } else {
                        this._values[index] = value;
                    }
                    return this;
                },
                get: function (key) {
                    var index = array.indexOf(this._keys, key);
                    return index === -1 ? null : this._values[index];
                },
                count: function () {
                    return this.length;
                },
                each: function (method, context) {
                    for (var i = 0, l = this.length; i < l; i++) {
                        if (method.call(context, this._values[i], this._keys[i], this) === false)
                            break;
                    }
                    return this;
                },
                backwards: function (method, context) {
                    for (var i = this.length - 1; i >= 0; i--) {
                        if (method.call(context, this._values[i], this._keys[i], this) === false)
                            break;
                    }
                    return this;
                },
                map: function (method, context) {
                    var results = new Map();
                    this.each(function (value, key) {
                        results.set(key, method.call(context, value, key, this));
                    }, this);
                    return results;
                },
                filter: function (method, context) {
                    var results = new Map();
                    this.each(function (value, key) {
                        if (method.call(context, value, key, this))
                            results.set(key, value);
                    }, this);
                    return results;
                },
                every: function (method, context) {
                    var every = true;
                    this.each(function (value, key) {
                        if (!method.call(context, value, key, this))
                            return every = false;
                    }, this);
                    return every;
                },
                some: function (method, context) {
                    var some = false;
                    this.each(function (value, key) {
                        if (method.call(context, value, key, this))
                            return !(some = true);
                    }, this);
                    return some;
                },
                index: function (value) {
                    var index = array.indexOf(this._values, value);
                    return index > -1 ? this._keys[index] : null;
                },
                remove: function (key) {
                    var index = array.indexOf(this._keys, key);
                    if (index !== -1) {
                        this._keys.splice(index, 1);
                        this.length--;
                        return this._values.splice(index, 1)[0];
                    }
                    return null;
                },
                keys: function () {
                    return this._keys.slice();
                },
                values: function () {
                    return this._values.slice();
                }
            });
        module.exports = Map;
    },
    'e': function (require, module, exports, global) {
        'use strict';
        var $ = require('4'), array = require('5'), slick = require('f');
        var walk = function (combinator, method) {
            return function (expression) {
                var parts = slick.parse(expression || '*');
                expression = array.map(parts, function (part) {
                    return combinator + ' ' + part;
                }).join(', ');
                return this[method](expression);
            };
        };
        $.implement({
            search: function (expression) {
                if (this.length === 1)
                    return $(slick.search(expression, this[0], new $()));
                var buffer = [];
                for (var i = 0, node; node = this[i]; i++)
                    buffer.push.apply(buffer, slick.search(expression, node));
                return $(buffer).sort();
            },
            find: function (expression) {
                if (this.length === 1)
                    return $(slick.find(expression, this[0]));
                var buffer = [];
                for (var i = 0, node; node = this[i]; i++)
                    buffer.push(slick.find(expression, node));
                return $(buffer);
            },
            sort: function () {
                return slick.sort(this);
            },
            matches: function (expression) {
                return slick.matches(this[0], expression);
            },
            nextSiblings: walk('~', 'search'),
            nextSibling: walk('+', 'find'),
            previousSiblings: walk('!~', 'search'),
            previousSibling: walk('!+', 'find'),
            children: walk('>', 'search'),
            firstChild: walk('^', 'find'),
            lastChild: walk('!^', 'find'),
            parent: function (expression) {
                for (var i = 0, node; node = this[i]; i++)
                    while (node = node.parentNode) {
                        if (!expression || slick.matches(node, expression))
                            return $(node);
                    }
                return null;
            },
            parents: function (expression) {
                var buffer = [];
                for (var i = 0, node; node = this[i]; i++)
                    while (node = node.parentNode) {
                        if (!expression || slick.matches(node, expression))
                            buffer.push(node);
                    }
                return $(buffer);
            }
        });
        module.exports = $;
    },
    'f': function (require, module, exports, global) {
        'use strict';
        var parse = require('g');
        var uniqueIndex = 0;
        var uniqueID = function (node) {
            return node.uniqueNumber || (node.uniqueNumber = 's:' + uniqueIndex++);
        };
        var uniqueIDXML = function (node) {
            var uid = node.getAttribute('uniqueNumber');
            if (!uid) {
                uid = 's:' + uniqueIndex++;
                node.setAttribute('uniqueNumber', uid);
            }
            return uid;
        };
        var isArray = Array.isArray || function (object) {
                return Object.prototype.toString.call(object) === '[object Array]';
            };
        var HAS = {
                GET_ELEMENT_BY_ID: function (test, id) {
                    test.innerHTML = '<a id="' + id + '"></a>';
                    return !!this.getElementById(id);
                },
                QUERY_SELECTOR: function (test) {
                    test.innerHTML = '_<style>:nth-child(2){}</style>';
                    test.innerHTML = '<a class="MiX"></a>';
                    return test.querySelectorAll('.MiX').length === 1;
                },
                EXPANDOS: function (test, id) {
                    test._custom_property_ = id;
                    return test._custom_property_ === id;
                },
                MATCHES_SELECTOR: function (test) {
                    test.innerHTML = '<a class="MiX"></a>';
                    var matches = test.matchesSelector || test.mozMatchesSelector || test.webkitMatchesSelector;
                    if (matches)
                        try {
                            matches.call(test, ':slick');
                        } catch (e) {
                            return matches.call(test, '.MiX') ? matches : false;
                        }
                    return false;
                },
                GET_ELEMENTS_BY_CLASS_NAME: function (test) {
                    test.innerHTML = '<a class="f"></a><a class="b"></a>';
                    if (test.getElementsByClassName('b').length !== 1)
                        return false;
                    test.firstChild.className = 'b';
                    if (test.getElementsByClassName('b').length !== 2)
                        return false;
                    test.innerHTML = '<a class="a"></a><a class="f b a"></a>';
                    if (test.getElementsByClassName('a').length !== 2)
                        return false;
                    return true;
                },
                GET_ATTRIBUTE: function (test) {
                    var shout = 'fus ro dah';
                    test.innerHTML = '<a class="' + shout + '"></a>';
                    return test.firstChild.getAttribute('class') === shout;
                }
            };
        var Finder = function Finder(document) {
            this.document = document;
            var root = this.root = document.documentElement;
            this.tested = {};
            this.uniqueID = this.has('EXPANDOS') ? uniqueID : uniqueIDXML;
            this.getAttribute = this.has('GET_ATTRIBUTE') ? function (node, name) {
                return node.getAttribute(name);
            } : function (node, name) {
                var node = node.getAttributeNode(name);
                return node && node.specified ? node.value : null;
            };
            this.hasAttribute = root.hasAttribute ? function (node, attribute) {
                return node.hasAttribute(attribute);
            } : function (node, attribute) {
                node = node.getAttributeNode(attribute);
                return !!(node && node.specified);
            };
            this.contains = document.contains && root.contains ? function (context, node) {
                return context.contains(node);
            } : root.compareDocumentPosition ? function (context, node) {
                return context === node || !!(context.compareDocumentPosition(node) & 16);
            } : function (context, node) {
                do {
                    if (node === context)
                        return true;
                } while (node = node.parentNode);
                return false;
            };
            this.sorter = root.compareDocumentPosition ? function (a, b) {
                if (!a.compareDocumentPosition || !b.compareDocumentPosition)
                    return 0;
                return a.compareDocumentPosition(b) & 4 ? -1 : a === b ? 0 : 1;
            } : 'sourceIndex' in root ? function (a, b) {
                if (!a.sourceIndex || !b.sourceIndex)
                    return 0;
                return a.sourceIndex - b.sourceIndex;
            } : document.createRange ? function (a, b) {
                if (!a.ownerDocument || !b.ownerDocument)
                    return 0;
                var aRange = a.ownerDocument.createRange(), bRange = b.ownerDocument.createRange();
                aRange.setStart(a, 0);
                aRange.setEnd(a, 0);
                bRange.setStart(b, 0);
                bRange.setEnd(b, 0);
                return aRange.compareBoundaryPoints(Range.START_TO_END, bRange);
            } : null;
            this.failed = {};
            var nativeMatches = this.has('MATCHES_SELECTOR');
            if (nativeMatches)
                this.matchesSelector = function (node, expression) {
                    if (this.failed[expression])
                        return true;
                    try {
                        return nativeMatches.call(node, expression);
                    } catch (e) {
                        if (slick.debug)
                            console.warn('matchesSelector failed on ' + expression);
                        return this.failed[expression] = true;
                    }
                };
            if (this.has('QUERY_SELECTOR')) {
                this.querySelectorAll = function (node, expression) {
                    if (this.failed[expression])
                        return true;
                    var result, _id, _expression, _slick_id, _combinator;
                    if (node !== this.document) {
                        _combinator = expression[0].combinator;
                        _id = node.getAttribute('id');
                        _expression = expression;
                        if (!_id) {
                            _slick_id = true;
                            _id = '__slick__';
                            node.setAttribute('id', _id);
                        }
                        expression = '#' + _id + ' ' + _expression;
                        if (_combinator.indexOf('~') > -1 || _combinator.indexOf('+') > -1) {
                            node = node.parentNode;
                            if (!node)
                                result = true;
                        }
                    }
                    if (!result)
                        try {
                            result = node.querySelectorAll(expression);
                        } catch (e) {
                            if (slick.debug)
                                console.warn('querySelectorAll failed on ' + (_expression || expression));
                            result = this.failed[_expression || expression] = true;
                        }
                    if (_slick_id)
                        node.removeAttribute('id');
                    return result;
                };
            }
        };
        Finder.prototype.has = function (FEATURE) {
            var tested = this.tested, testedFEATURE = tested[FEATURE];
            if (testedFEATURE != null)
                return testedFEATURE;
            var root = this.root, document = this.document, testNode = document.createElement('div');
            testNode.setAttribute('style', 'display: none;');
            root.appendChild(testNode);
            var TEST = HAS[FEATURE], result = false;
            if (TEST)
                try {
                    result = TEST.call(document, testNode, 's:' + uniqueIndex++);
                } catch (e) {
                }
            if (slick.debug && !result)
                console.warn('document has no ' + FEATURE);
            root.removeChild(testNode);
            return tested[FEATURE] = result;
        };
        var combinators = {
                ' ': function (node, part, push) {
                    var item, items;
                    var noId = !part.id, noTag = !part.tag, noClass = !part.classes;
                    if (part.id && node.getElementById && this.has('GET_ELEMENT_BY_ID')) {
                        item = node.getElementById(part.id);
                        if (item && item.getAttribute('id') === part.id) {
                            items = [item];
                            noId = true;
                            if (part.tag === '*')
                                noTag = true;
                        }
                    }
                    if (!items) {
                        if (part.classes && node.getElementsByClassName && this.has('GET_ELEMENTS_BY_CLASS_NAME')) {
                            items = node.getElementsByClassName(part.classList);
                            noClass = true;
                            if (part.tag === '*')
                                noTag = true;
                        } else {
                            items = node.getElementsByTagName(part.tag);
                            if (part.tag !== '*')
                                noTag = true;
                        }
                        if (!items || !items.length)
                            return false;
                    }
                    for (var i = 0; item = items[i++];)
                        if (noTag && noId && noClass && !part.attributes && !part.pseudos || this.match(item, part, noTag, noId, noClass))
                            push(item);
                    return true;
                },
                '>': function (node, part, push) {
                    if (node = node.firstChild)
                        do {
                            if (node.nodeType == 1 && this.match(node, part))
                                push(node);
                        } while (node = node.nextSibling);
                },
                '+': function (node, part, push) {
                    while (node = node.nextSibling)
                        if (node.nodeType == 1) {
                            if (this.match(node, part))
                                push(node);
                            break;
                        }
                },
                '^': function (node, part, push) {
                    node = node.firstChild;
                    if (node) {
                        if (node.nodeType === 1) {
                            if (this.match(node, part))
                                push(node);
                        } else {
                            combinators['+'].call(this, node, part, push);
                        }
                    }
                },
                '~': function (node, part, push) {
                    while (node = node.nextSibling) {
                        if (node.nodeType === 1 && this.match(node, part))
                            push(node);
                    }
                },
                '++': function (node, part, push) {
                    combinators['+'].call(this, node, part, push);
                    combinators['!+'].call(this, node, part, push);
                },
                '~~': function (node, part, push) {
                    combinators['~'].call(this, node, part, push);
                    combinators['!~'].call(this, node, part, push);
                },
                '!': function (node, part, push) {
                    while (node = node.parentNode)
                        if (node !== this.document && this.match(node, part))
                            push(node);
                },
                '!>': function (node, part, push) {
                    node = node.parentNode;
                    if (node !== this.document && this.match(node, part))
                        push(node);
                },
                '!+': function (node, part, push) {
                    while (node = node.previousSibling)
                        if (node.nodeType == 1) {
                            if (this.match(node, part))
                                push(node);
                            break;
                        }
                },
                '!^': function (node, part, push) {
                    node = node.lastChild;
                    if (node) {
                        if (node.nodeType == 1) {
                            if (this.match(node, part))
                                push(node);
                        } else {
                            combinators['+'].call(this, node, part);
                        }
                    }
                },
                '!~': function (node, part, push) {
                    while (node = node.previousSibling) {
                        if (node.nodeType === 1 && this.match(node, part))
                            push(node);
                    }
                }
            };
        Finder.prototype.search = function (context, expression, found) {
            if (!context)
                context = this.document;
            else if (context.document)
                context = context.document;
            var expressions = parse(expression);
            if (!expressions || !expressions.length)
                throw new Error('invalid expression');
            if (!found)
                found = [];
            var uniques, push = isArray(found) ? function (node) {
                    found[found.length] = node;
                } : function (node) {
                    found[found.length++] = node;
                };
            if (expressions.length > 1) {
                uniques = {};
                var plush = push;
                push = function (node) {
                    var uid = uniqueID(node);
                    if (!uniques[uid]) {
                        uniques[uid] = true;
                        plush(node);
                    }
                };
            }
            var node, nodes, part, ctx;
            main:
                for (var i = 0; expression = expressions[i++];) {
                    if (!slick.noQSA && this.querySelectorAll) {
                        nodes = this.querySelectorAll(context, expression);
                        if (nodes !== true) {
                            if (nodes && nodes.length)
                                for (var j = 0; node = nodes[j++];)
                                    if (node.nodeName > '@') {
                                        push(node);
                                    }
                            continue main;
                        }
                    }
                    if (expression.length === 1) {
                        part = expression[0];
                        combinators[part.combinator].call(this, context, part, push);
                    } else {
                        var cs = [context], c, f, u, p = function (node) {
                                var uid = uniqueID(node);
                                if (!u[uid]) {
                                    u[uid] = true;
                                    f[f.length] = node;
                                }
                            };
                        for (var j = 0; part = expression[j++];) {
                            f = [];
                            u = {};
                            for (var k = 0; c = cs[k++];)
                                combinators[part.combinator].call(this, c, part, p);
                            if (!f.length)
                                continue main;
                            if (j === expression.length)
                                found = f;
                            else
                                cs = f;
                        }
                    }
                    if (!found.length)
                        continue main;
                }
            if (uniques && found && found.length > 1)
                this.sort(found);
            return found;
        };
        Finder.prototype.sort = function (nodes) {
            return this.sorter ? Array.prototype.sort.call(nodes, this.sorter) : nodes;
        };
        var pseudos = {
                'empty': function () {
                    var child = this.firstChild;
                    return !(this && this.nodeType === 1) && !(this.innerText || this.textContent || '').length;
                },
                'not': function (expression) {
                    return !slick.match(this, expression);
                },
                'contains': function (text) {
                    return (this.innerText || this.textContent || '').indexOf(text) > -1;
                },
                'first-child': function () {
                    var node = this;
                    while (node = node.previousSibling)
                        if (node.nodeType == 1)
                            return false;
                    return true;
                },
                'last-child': function () {
                    var node = this;
                    while (node = node.nextSibling)
                        if (node.nodeType == 1)
                            return false;
                    return true;
                },
                'only-child': function () {
                    var prev = this;
                    while (prev = prev.previousSibling)
                        if (prev.nodeType == 1)
                            return false;
                    var next = this;
                    while (next = next.nextSibling)
                        if (next.nodeType == 1)
                            return false;
                    return true;
                },
                'first-of-type': function () {
                    var node = this, nodeName = node.nodeName;
                    while (node = node.previousSibling)
                        if (node.nodeName == nodeName)
                            return false;
                    return true;
                },
                'last-of-type': function () {
                    var node = this, nodeName = node.nodeName;
                    while (node = node.nextSibling)
                        if (node.nodeName == nodeName)
                            return false;
                    return true;
                },
                'only-of-type': function () {
                    var prev = this, nodeName = this.nodeName;
                    while (prev = prev.previousSibling)
                        if (prev.nodeName == nodeName)
                            return false;
                    var next = this;
                    while (next = next.nextSibling)
                        if (next.nodeName == nodeName)
                            return false;
                    return true;
                },
                'enabled': function () {
                    return !this.disabled;
                },
                'disabled': function () {
                    return this.disabled;
                },
                'checked': function () {
                    return this.checked || this.selected;
                },
                'selected': function () {
                    return this.selected;
                },
                'focus': function () {
                    var doc = this.ownerDocument;
                    return doc.activeElement === this && (this.href || this.type || slick.hasAttribute(this, 'tabindex'));
                },
                'root': function () {
                    return this === this.ownerDocument.documentElement;
                }
            };
        Finder.prototype.match = function (node, bit, noTag, noId, noClass) {
            if (!slick.noQSA && this.matchesSelector) {
                var matches = this.matchesSelector(node, bit);
                if (matches !== true)
                    return matches;
            }
            if (!noTag && bit.tag) {
                var nodeName = node.nodeName.toLowerCase();
                if (bit.tag === '*') {
                    if (nodeName < '@')
                        return false;
                } else if (nodeName != bit.tag) {
                    return false;
                }
            }
            if (!noId && bit.id && node.getAttribute('id') !== bit.id)
                return false;
            var i, part;
            if (!noClass && bit.classes) {
                var className = this.getAttribute(node, 'class');
                if (!className)
                    return false;
                for (part in bit.classes)
                    if (!RegExp('(^|\\s)' + bit.classes[part] + '(\\s|$)').test(className))
                        return false;
            }
            var name, value;
            if (bit.attributes)
                for (i = 0; part = bit.attributes[i++];) {
                    var operator = part.operator, escaped = part.escapedValue;
                    name = part.name;
                    value = part.value;
                    if (!operator) {
                        if (!this.hasAttribute(node, name))
                            return false;
                    } else {
                        var actual = this.getAttribute(node, name);
                        if (actual == null)
                            return false;
                        switch (operator) {
                        case '^=':
                            if (!RegExp('^' + escaped).test(actual))
                                return false;
                            break;
                        case '$=':
                            if (!RegExp(escaped + '$').test(actual))
                                return false;
                            break;
                        case '~=':
                            if (!RegExp('(^|\\s)' + escaped + '(\\s|$)').test(actual))
                                return false;
                            break;
                        case '|=':
                            if (!RegExp('^' + escaped + '(-|$)').test(actual))
                                return false;
                            break;
                        case '=':
                            if (actual !== value)
                                return false;
                            break;
                        case '*=':
                            if (actual.indexOf(value) === -1)
                                return false;
                            break;
                        default:
                            return false;
                        }
                    }
                }
            if (bit.pseudos)
                for (i = 0; part = bit.pseudos[i++];) {
                    name = part.name;
                    value = part.value;
                    if (pseudos[name])
                        return pseudos[name].call(node, value);
                    if (value != null) {
                        if (this.getAttribute(node, name) !== value)
                            return false;
                    } else {
                        if (!this.hasAttribute(node, name))
                            return false;
                    }
                }
            return true;
        };
        Finder.prototype.matches = function (node, expression) {
            var expressions = parse(expression);
            if (expressions.length === 1 && expressions[0].length === 1) {
                return this.match(node, expressions[0][0]);
            }
            if (!slick.noQSA && this.matchesSelector) {
                var matches = this.matchesSelector(node, expressions);
                if (matches !== true)
                    return matches;
            }
            var nodes = this.search(node, expression, { length: 0 });
            for (var i = 0, res; res = nodes[i++];)
                if (node === res)
                    return true;
            return false;
        };
        var finders = {};
        var finder = function (context) {
            var doc = context || document;
            if (doc.document)
                doc = doc.document;
            else if (doc.ownerDocument)
                doc = doc.ownerDocument;
            if (doc.nodeType !== 9)
                throw new TypeError('invalid document');
            var uid = uniqueID(doc);
            return finders[uid] || (finders[uid] = new Finder(doc));
        };
        var slick = function (expression, context) {
            return slick.search(expression, context);
        };
        slick.search = function (expression, context, found) {
            return finder(context).search(context, expression, found);
        };
        slick.find = function (expression, context) {
            return finder(context).search(context, expression)[0] || null;
        };
        slick.getAttribute = function (node, name) {
            return finder(node).getAttribute(node, name);
        };
        slick.hasAttribute = function (node, name) {
            return finder(node).hasAttribute(node, name);
        };
        slick.contains = function (context, node) {
            return finder(context).contains(context, node);
        };
        slick.matches = function (node, expression) {
            return finder(node).matches(node, expression);
        };
        slick.sort = function (nodes) {
            if (nodes && nodes.length > 1)
                finder(nodes[0]).sort(nodes);
            return nodes;
        };
        slick.parse = parse;
        module.exports = slick;
    },
    'g': function (require, module, exports, global) {
        'use strict';
        var escapeRe = /([-.*+?^${}()|[\]\/\\])/g, unescapeRe = /\\/g;
        var escape = function (string) {
            return (string + '').replace(escapeRe, '\\$1');
        };
        var unescape = function (string) {
            return (string + '').replace(unescapeRe, '');
        };
        var slickRe = RegExp('^(?:\\s*(,)\\s*|\\s*(<combinator>+)\\s*|(\\s+)|(<unicode>+|\\*)|\\#(<unicode>+)|\\.(<unicode>+)|\\[\\s*(<unicode1>+)(?:\\s*([*^$!~|]?=)(?:\\s*(?:(["\']?)(.*?)\\9)))?\\s*\\](?!\\])|(:+)(<unicode>+)(?:\\((?:(?:(["\'])([^\\13]*)\\13)|((?:\\([^)]+\\)|[^()]*)+))\\))?)'.replace(/<combinator>/, '[' + escape('>+~`!@$%^&={}\\;</') + ']').replace(/<unicode>/g, '(?:[\\w\\u00a1-\\uFFFF-]|\\\\[^\\s0-9a-f])').replace(/<unicode1>/g, '(?:[:\\w\\u00a1-\\uFFFF-]|\\\\[^\\s0-9a-f])'));
        var Part = function Part(combinator) {
            this.combinator = combinator || ' ';
            this.tag = '*';
        };
        Part.prototype.toString = function () {
            if (!this.raw) {
                var xpr = '', k, part;
                xpr += this.tag || '*';
                if (this.id)
                    xpr += '#' + this.id;
                if (this.classes)
                    xpr += '.' + this.classList.join('.');
                if (this.attributes)
                    for (k = 0; part = this.attributes[k++];) {
                        xpr += '[' + part.name + (part.operator ? part.operator + '"' + part.value + '"' : '') + ']';
                    }
                if (this.pseudos)
                    for (k = 0; part = this.pseudos[k++];) {
                        xpr += ':' + part.name;
                        if (part.value)
                            xpr += '(' + part.value + ')';
                    }
                this.raw = xpr;
            }
            return this.raw;
        };
        var Expression = function Expression() {
            this.length = 0;
        };
        Expression.prototype.toString = function () {
            if (!this.raw) {
                var xpr = '';
                for (var j = 0, bit; bit = this[j++];) {
                    if (j !== 1)
                        xpr += ' ';
                    if (bit.combinator !== ' ')
                        xpr += bit.combinator + ' ';
                    xpr += bit;
                }
                this.raw = xpr;
            }
            return this.raw;
        };
        var replacer = function (rawMatch, separator, combinator, combinatorChildren, tagName, id, className, attributeKey, attributeOperator, attributeQuote, attributeValue, pseudoMarker, pseudoClass, pseudoQuote, pseudoClassQuotedValue, pseudoClassValue) {
            var expression, current;
            if (separator || !this.length) {
                expression = this[this.length++] = new Expression();
                if (separator)
                    return '';
            }
            if (!expression)
                expression = this[this.length - 1];
            if (combinator || combinatorChildren || !expression.length) {
                current = expression[expression.length++] = new Part(combinator);
            }
            if (!current)
                current = expression[expression.length - 1];
            if (tagName) {
                current.tag = unescape(tagName);
            } else if (id) {
                current.id = unescape(id);
            } else if (className) {
                var unescaped = unescape(className);
                var classes = current.classes || (current.classes = {});
                if (!classes[unescaped]) {
                    classes[unescaped] = escape(className);
                    var classList = current.classList || (current.classList = []);
                    classList.push(unescaped);
                    classList.sort();
                }
            } else if (pseudoClass) {
                pseudoClassValue = pseudoClassValue || pseudoClassQuotedValue;
                ;
                (current.pseudos || (current.pseudos = [])).push({
                    type: pseudoMarker.length == 1 ? 'class' : 'element',
                    name: unescape(pseudoClass),
                    escapedName: escape(pseudoClass),
                    value: pseudoClassValue ? unescape(pseudoClassValue) : null,
                    escapedValue: pseudoClassValue ? escape(pseudoClassValue) : null
                });
            } else if (attributeKey) {
                attributeValue = attributeValue ? escape(attributeValue) : null;
                ;
                (current.attributes || (current.attributes = [])).push({
                    operator: attributeOperator,
                    name: unescape(attributeKey),
                    escapedName: escape(attributeKey),
                    value: attributeValue ? unescape(attributeValue) : null,
                    escapedValue: attributeValue ? escape(attributeValue) : null
                });
            }
            return '';
        };
        var Expressions = function Expressions(expression) {
            this.length = 0;
            var self = this;
            while (expression)
                expression = expression.replace(slickRe, function () {
                    return replacer.apply(self, arguments);
                });
        };
        Expressions.prototype.toString = function () {
            if (!this.raw) {
                var expressions = [];
                for (var i = 0, expression; expression = this[i++];)
                    expressions.push(expression);
                this.raw = expressions.join(', ');
            }
            return this.raw;
        };
        var cache = {};
        var parse = function (expression) {
            if (expression == null)
                return null;
            expression = ('' + expression).replace(/^\s+|\s+$/g, '');
            return cache[expression] || (cache[expression] = new Expressions(expression));
        };
        module.exports = parse;
    },
    'h': function (require, module, exports, global) {
        'use strict';
        var $ = require('4');
        $.implement({
            appendChild: function (child) {
                this[0].appendChild($(child)[0]);
                return this;
            },
            insertBefore: function (child, ref) {
                this[0].insertBefore($(child)[0], $(ref)[0]);
                return this;
            },
            removeChild: function (child) {
                this[0].removeChild($(child)[0]);
                return this;
            },
            replaceChild: function (child, ref) {
                this[0].replaceChild($(child)[0], $(ref)[0]);
                return this;
            }
        });
        $.implement({
            before: function (element) {
                element = $(element)[0];
                var parent = element.parentNode;
                if (parent)
                    this.forEach(function (node) {
                        parent.insertBefore(node, element);
                    });
                return this;
            },
            after: function (element) {
                element = $(element)[0];
                var parent = element.parentNode;
                if (parent)
                    this.forEach(function (node) {
                        parent.insertBefore(node, element.nextSibling);
                    });
                return this;
            },
            bottom: function (element) {
                element = $(element)[0];
                this.forEach(function (node) {
                    element.appendChild(node);
                });
                return this;
            },
            top: function (element) {
                element = $(element)[0];
                this.forEach(function (node) {
                    element.insertBefore(node, element.firstChild);
                });
                return this;
            }
        });
        $.implement({
            insert: $.prototype.bottom,
            remove: function () {
                this.forEach(function (node) {
                    var parent = node.parentNode;
                    if (parent)
                        parent.removeChild(node);
                });
                return this;
            },
            replace: function (element) {
                element = $(element)[0];
                element.parentNode.replaceChild(this[0], element);
                return this;
            }
        });
        module.exports = $;
    },
    'i': function (require, module, exports, global) {
        'use strict';
        var $ = require('4');
        $.implement({
            empty: function () {
                for (var i = 0, node; node = this[i]; i++) {
                    var first;
                    while (first = node.firstChild)
                        node.removeChild(first);
                }
                return this;
            }
        });
        module.exports = $;
    },
    'j': function (require, module, exports, global) {
        'use strict';
        var $ = require('4'), parse = require('g'), array = require('5');
        module.exports = function (expression, doc) {
            return $(array.map(parse(expression), function (expression) {
                var previous, result;
                array.forEach(expression, function (part, i) {
                    var node = (doc || document).createElement(part.tag);
                    if (part.id)
                        node.id = part.id;
                    if (part.classList)
                        node.className = part.classList.join(' ');
                    if (part.attributes)
                        array.forEach(part.attributes, function (attribute) {
                            node.setAttribute(attribute.name, attribute.value);
                        });
                    if (part.pseudos)
                        array.forEach(part.pseudos, function (pseudo) {
                            var n = $(node), method = n[pseudo.name];
                            if (method)
                                method.call(n, pseudo.value);
                        });
                    if (i === 0) {
                        result = node;
                    } else if (part.combinator === ' ') {
                        previous.appendChild(node);
                    } else if (part.combinator === '+') {
                        var parentNode = previous.parentNode;
                        if (parentNode)
                            parentNode.appendChild(node);
                    }
                    previous = node;
                });
                return result;
            }));
        };
    },
    'k': function (require, module, exports, global) {
        'use strict';
        var number = require('l');
        module.exports = number.implement({
            limit: function (min, max) {
                return Math.min(max, Math.max(min, this));
            },
            round: function (precision) {
                precision = Math.pow(10, precision || 0).toFixed(precision < 0 ? -precision : 0);
                return Math.round(this * precision) / precision;
            },
            times: function (fn, context) {
                for (var i = 0; i < this; i++)
                    fn.call(context, i, null, this);
                return this;
            },
            random: function (max) {
                return Math.floor(Math.random() * (max - this + 1) + this);
            }
        });
    },
    'l': function (require, module, exports, global) {
        'use strict';
        var number = require('6')['number'];
        var names = 'toExponential,toFixed,toLocaleString,toPrecision,toString,valueOf'.split(',');
        for (var methods = {}, i = 0, name, method; name = names[i++];)
            if (method = Number.prototype[name])
                methods[name] = method;
        module.exports = number.implement(methods);
    },
    'm': function (require, module, exports, global) {
        'use strict';
        var prime = require('1');
        var slice = Array.prototype.slice;
        function mixin(object) {
            var mixins = slice.call(arguments, 1);
            for (var i = 0; i < mixins.length; i++) {
                object.implement(prime.create(mixins[i].prototype));
            }
            return object;
        }
        module.exports = mixin;
    },
    'n': function (require, module, exports, global) {
        'use strict';
        var prime = require('1');
        var fn = require('o');
        var bound = prime({
                bound: function (name) {
                    var bound = this._bound || (this._bound = {});
                    return bound[name] || (bound[name] = fn.bound(this[name], this));
                }
            });
        module.exports = bound;
    },
    'o': function (require, module, exports, global) {
        'use strict';
        var fn = require('p');
        var slice = Array.prototype.slice;
        fn.implement({
            bound: function (thisArg) {
                var args = slice.call(arguments, 1), self = this;
                return function () {
                    return self.apply(thisArg, args.concat(slice.call(arguments)));
                };
            }
        });
        module.exports = fn;
    },
    'p': function (require, module, exports, global) {
        'use strict';
        var function_ = require('6')['function'];
        var names = 'apply,bind,call,isGenerator,toString'.split(',');
        for (var methods = {}, i = 0, name, method; name = names[i++];)
            if (method = Function.prototype[name])
                methods[name] = method;
        module.exports = function_.implement(methods);
    },
    'q': function (require, module, exports, global) {
        'Use Strict';
        var prime = require('1');
        var string = require('a');
        var mixin = require('m');
        var bound = require('n');
        var options = require('r');
        var Arr = require('5');
        var Obj = require('s');
        var FuzzySearch = prime({
                modules: [],
                options: {
                    'caseSensitive': false,
                    'termPath': ''
                },
                constructor: function (searchSet, options) {
                    this.setOptions(options);
                    this.searchSet = searchSet;
                },
                addModule: function (mod) {
                    this.modules.push(mod);
                },
                search: function (needle) {
                    var needle = this.options.caseSensitive ? string.clean(needle).toLowerCase() : string.clean(needle);
                    var result = [];
                    Arr.forEach(this.searchSet, function (value) {
                        var value = this.options.termPath.length == 0 ? value : Obj.fromPath(value, this.options.termPath);
                        if (this.options.caseSensitive) {
                            value = value.toLowerCase();
                        }
                        var score = this.getCombinedModulePoints(needle, value);
                        result.push({
                            'score': score.combined,
                            'details': score.details,
                            'value': value
                        });
                    }, this);
                    return result.sort(function (a, b) {
                        return b.score - a.score;
                    });
                },
                getCombinedModulePoints: function (needle, haystack) {
                    var result = {
                            'combined': 0,
                            'details': []
                        };
                    Arr.forEach(this.modules, function (mod) {
                        var score = mod.search(needle, haystack).getPoints();
                        var name = mod.getName();
                        var factor = mod.getFactor();
                        result.combined += factor * score;
                        result.details.push({
                            'name': name,
                            'score': score,
                            'factor': factor
                        });
                    });
                    return result;
                },
                getMaximumScore: function () {
                    var factorSum = 0;
                    Arr.forEach(this.modules, function (mod) {
                        factorSum += mod.getFactor();
                    });
                    return 100 * factorSum;
                }
            });
        mixin(FuzzySearch, options, bound);
        module.exports = FuzzySearch;
    },
    'r': function (require, module, exports, global) {
        'use strict';
        var prime = require('1');
        var object = require('s');
        var Options = prime({
                setOptions: function (options) {
                    var args = [
                            {},
                            this.options
                        ];
                    args.push.apply(args, arguments);
                    this.options = object.merge.apply(null, args);
                    return this;
                }
            });
        module.exports = Options;
    },
    's': function (require, module, exports, global) {
        'use strict';
        var object = require('t');
        var type = require('7');
        object.implement({
            merge: function (key, value) {
                if (typeof key == 'string') {
                    if (type(this[key]) == 'object')
                        object.merge(this[key], value);
                    else
                        this[key] = value;
                } else
                    for (var i = 0; i < arguments.length; i++) {
                        var obj = arguments[i];
                        for (var k in obj)
                            object.merge(this, k, obj[k]);
                    }
                return this;
            },
            fromPath: function (parts) {
                var source = this;
                if (typeof parts == 'string')
                    parts = parts.split('.');
                for (var i = 0, l = parts.length; i < l; i++) {
                    if (object.hasOwnProperty(source, parts[i]))
                        source = source[parts[i]];
                    else
                        return null;
                }
                return source;
            }
        });
        module.exports = object;
    },
    't': function (require, module, exports, global) {
        'use strict';
        var object = require('6')['object'];
        var names = 'hasOwnProperty,isPrototypeOf,propertyIsEnumerable,toLocaleString,toString,valueOf'.split(',');
        for (var methods = {}, i = 0, name, method; name = names[i++];)
            if (method = Object.prototype[name])
                methods[name] = method;
        module.exports = object.implement(methods);
    },
    'u': function (require, module, exports, global) {
        'Use Strict';
        var prime = require('1');
        var FSModule = require('v');
        var Arr = require('5');
        var lev = require('w');
        var LevenshteinFS = prime({
                inherits: FSModule,
                name: 'LevenshteinFS',
                options: { 'maxDistanceTolerance': 3 },
                search: function (term, haystack) {
                    this.lastTerm = term;
                    this.lastHaystack = haystack;
                    var needleWords = term.split(' ');
                    var haystackWords = haystack.split(' ');
                    var matches = [];
                    var nwl = needleWords.length;
                    var hwl = haystackWords.length;
                    for (var i = 0; i < nwl; i++) {
                        for (var j = 0; j < hwl; j++) {
                            var needleWord = needleWords[i];
                            var haystackWord = haystackWords[j];
                            var score = lev(needleWord, haystackWord);
                            if (score <= this.options.maxDistanceTolerance) {
                                matches.push({
                                    'match': needleWord,
                                    'score': score
                                });
                            }
                        }
                    }
                    this.lastResults = matches;
                    return this;
                },
                getPoints: function () {
                    var haystackWords = this.lastHaystack.split(' ');
                    var combinedScore = 0;
                    Arr.forEach(this.lastResults, function (result) {
                        combinedScore += result.score;
                    });
                    combinedScore += (haystackWords.length - this.lastResults.length) * this.options.maxDistanceTolerance;
                    var points = 50 / haystackWords.length * this.lastResults.length;
                    points += 50 / (haystackWords.length * this.options.maxDistanceTolerance) * (haystackWords.length * this.options.maxDistanceTolerance - combinedScore);
                    return points;
                }
            });
        module.exports = function (options) {
            return new LevenshteinFS(options);
        };
    },
    'v': function (require, module, exports, global) {
        'Use Strict';
        var prime = require('1');
        var mixin = require('m');
        var bound = require('n');
        var options = require('r');
        var FSModule = prime({
                lastTerm: '',
                lastHaystack: '',
                lastResults: null,
                options: { 'factor': 1 },
                constructor: function (options) {
                    this.setOptions(options);
                    this.lastResults = [];
                },
                search: function (searchTerm) {
                    throw new Error('search method not implemented');
                },
                getPoints: function () {
                    throw new Error('getPoints method not implemented');
                },
                getMatches: function () {
                    return this.lastResults;
                },
                getFactor: function () {
                    return this.options.factor || 1;
                },
                getName: function () {
                    if (!this.name)
                        throw new Error('set module name!');
                    return this.name;
                }
            });
        mixin(FSModule, bound, options);
        module.exports = FSModule;
    },
    'w': function (require, module, exports, global) {
        if (!Array.forEach) {
            Array.forEach = function forEach(array, iterator, context) {
                iterator = context ? iterator.bind(context) : iterator;
                Array.prototype.forEach.call(array, iterator);
            };
        }
        function Levenshtein(str_m, str_n) {
            var previous, current, matrix;
            this.valueOf = function () {
                return this.distance;
            };
            this.toString = this.inspect = function inspect(no_print) {
                var max, buff, sep, rows;
                max = matrix.reduce(function (m, o) {
                    return Math.max(m, o.reduce(Math.max, 0));
                }, 0);
                buff = Array((max + '').length).join(' ');
                sep = [];
                while (sep.length < (matrix[0] && matrix[0].length || 0))
                    sep[sep.length] = Array(buff.length + 1).join('-');
                sep = sep.join('-+') + '-';
                rows = matrix.map(function (row) {
                    var cells;
                    cells = row.map(function (cell) {
                        return (buff + cell).slice(-buff.length);
                    });
                    return cells.join(' |') + ' ';
                });
                return rows.join('\n' + sep + '\n');
            };
            matrix = [];
            if (str_m == str_n)
                return this.distance = 0;
            else if (str_m == '')
                return this.distance = str_n.length;
            else if (str_n == '')
                return this.distance = str_m.length;
            else {
                previous = [0];
                Array.forEach(str_m, function (v, i) {
                    i++, previous[i] = i;
                });
                matrix[0] = previous;
                Array.forEach(str_n, function (n_val, n_idx) {
                    current = [++n_idx];
                    Array.forEach(str_m, function (m_val, m_idx) {
                        m_idx++;
                        if (str_m.charAt(m_idx - 1) == str_n.charAt(n_idx - 1))
                            current[m_idx] = previous[m_idx - 1];
                        else
                            current[m_idx] = Math.min(previous[m_idx] + 1, current[m_idx - 1] + 1, previous[m_idx - 1] + 1);
                    });
                    previous = current;
                    matrix[matrix.length] = previous;
                });
                return this.distance = current[current.length - 1];
            }
        }
        module.exports = Levenshtein;
    },
    'x': function (require, module, exports, global) {
        'Use Strict';
        var prime = require('1');
        var FSModule = require('v');
        var Arr = require('5');
        var sift3 = require('y');
        var Sift3FS = prime({
                inherits: FSModule,
                name: 'Sift3FS',
                options: { 'maxDistanceTolerance': 3 },
                search: function (searchTerm, haystack) {
                    this.lastTerm = searchTerm;
                    this.lastHaystack = haystack;
                    if (!this.sift3) {
                        this.sift3 = new sift3();
                    }
                    var needleWords = searchTerm.split(' ');
                    var haystackWords = haystack.split(' ');
                    var matches = [];
                    var nwl = needleWords.length;
                    var hwl = haystackWords.length;
                    for (var i = 0; i < nwl; i++) {
                        for (var j = 0; j < hwl; j++) {
                            var needleWord = needleWords[i];
                            var haystackWord = haystackWords[j];
                            var score = this.sift3.getDifference(needleWord, haystackWord);
                            if (score <= this.options.maxDistanceTolerance) {
                                matches.push({
                                    'match': needleWord,
                                    'score': score
                                });
                            }
                        }
                    }
                    this.lastResults = matches;
                    return this;
                },
                getPoints: function () {
                    var haystackWords = this.lastHaystack.split(' ');
                    var combinedScore = 0;
                    Arr.forEach(this.lastResults, function (result) {
                        combinedScore += result.score;
                    });
                    combinedScore += (haystackWords.length - this.lastResults.length) * this.options.maxDistanceTolerance;
                    var points = 50 / haystackWords.length * this.lastResults.length;
                    points += 50 / (haystackWords.length * this.options.maxDistanceTolerance) * (haystackWords.length * this.options.maxDistanceTolerance - combinedScore);
                    return points;
                }
            });
        module.exports = function (options) {
            return new Sift3FS(options);
        };
    },
    'y': function (require, module, exports, global) {
        'Use Strict';
        var prime = require('1');
        var sift3 = prime({
                constructor: function (haystack) {
                    this.haystack = haystack;
                },
                getDifference: function (s1, s2) {
                    var c = 0, offset1 = 0, offset2 = 0, lcs = 0, maxOffset = 5, i = 0;
                    if (s1 == null || s1.length === 0) {
                        if (s2 == null || s2.length === 0) {
                            return 0;
                        } else {
                            return s2.length;
                        }
                    }
                    if (s2 == null || s2.length === 0) {
                        return s1.length;
                    }
                    while (c + offset1 < s1.length && c + offset2 < s2.length) {
                        if (s1.charAt(c + offset1) == s2.charAt(c + offset2)) {
                            lcs++;
                        } else {
                            offset1 = 0;
                            offset2 = 0;
                            for (; i < maxOffset; i++) {
                                if (c + i < s1.length && s1.charAt(c + i) == s2.charAt(c)) {
                                    offset1 = i;
                                    break;
                                }
                                if (c + i < s2.length && s1.charAt(c) == s2.charAt(c + i)) {
                                    offset2 = i;
                                    break;
                                }
                            }
                        }
                        c++;
                    }
                    return (s1.length + s2.length) / 2 - lcs;
                },
                setHaystack: function (haystack) {
                    this.haystack = haystack;
                }
            });
        module.exports = sift3;
    },
    'z': function (require, module, exports, global) {
        'Use Strict';
        var prime = require('1');
        var FSModule = require('v');
        var Arr = require('5');
        var IndexOfFS = prime({
                inherits: FSModule,
                name: 'IndexOfFS',
                options: {
                    'minTermLength': 3,
                    'maxIterations': 500,
                    'factor': 1
                },
                search: function (searchTerm, searchHaystack) {
                    this.lastTerm = searchTerm;
                    this.lastHaystack = searchHaystack;
                    var minLength = searchTerm.length >= this.options.minTermLength ? this.options.minTermLength : searchTerm.length;
                    var matches = [];
                    var iterations = 0;
                    do {
                        var cm = this.getClosestMatch(searchTerm, searchHaystack);
                        if (cm.length >= minLength) {
                            matches.push(cm);
                        }
                        var substrc = cm.length - 1 > 0 ? cm.length : 1;
                        searchTerm = searchTerm.substr(substrc);
                        iterations++;
                    } while (searchTerm.length >= minLength && iterations <= this.options.maxIterations);
                    this.lastResults = matches;
                    return this;
                },
                getClosestMatch: function (searchTerm, haystack) {
                    if (haystack.indexOf(searchTerm) != -1) {
                        return searchTerm;
                    }
                    var length = searchTerm.length;
                    for (var i = 0; i <= length; i++) {
                        var term = searchTerm.substr(0, i);
                        if (haystack.indexOf(term) != -1) {
                            continue;
                        }
                        return term.substr(0, i - 1);
                    }
                    return '';
                },
                getPoints: function () {
                    var sum = 0;
                    Arr.forEach(this.lastResults, function (result) {
                        sum += result.length;
                    });
                    return 100 / this.lastTerm.length * sum;
                }
            });
        module.exports = function (options) {
            return new IndexOfFS(options);
        };
    },
    '10': function (require, module, exports, global) {
        'Use Strict';
        var prime = require('1');
        var FSModule = require('v');
        var number = require('k');
        var WordCountFS = prime({
                inherits: FSModule,
                name: 'WordCountFS',
                options: { 'maxWordTolerance': 3 },
                search: function (searchTerm, haystack) {
                    this.lastTerm = searchTerm;
                    this.lastHaystack = haystack;
                    return this;
                },
                getPoints: function () {
                    var needleWords = this.lastTerm.split(' ');
                    var haystackWords = this.lastHaystack.split(' ');
                    return 100 / this.options.maxWordTolerance * (this.options.maxWordTolerance - number.limit(Math.abs(haystackWords.length - needleWords.length), 0, this.options.maxWordTolerance));
                }
            });
        module.exports = function (options) {
            return new WordCountFS(options);
        };
    }
}));
//@ sourceMappingURL=./source.map
