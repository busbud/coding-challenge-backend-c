(function () {
	"use strict";

	var _ = require("../lib/underscore.js"),
		assert = require("chai").assert,
		equal = assert.equal,
		deepEqual = assert.deepEqual,
		ok = assert.ok,
		raises = assert.Throw;

	describe("Functions", function () {

		it("bind", function () {
			var context = {
				name: "moe"
			};
			var func = function (arg) {
				return "name: " + (this && this.name || arg);
			};
			var bound = _.bind(func, context);
			equal(bound(), "name: moe", "can bind a function to a context");

			bound = _(func).bind(context);
			equal(bound(), "name: moe", "can do OO-style binding");

			bound = _.bind(func, null, "curly");
			equal(bound(), "name: curly", "can bind without specifying a context");

			func = function (salutation, name) {
				return salutation + ": " + name;
			};
			func = _.bind(func, this, "hello");
			equal(func("moe"), "hello: moe", "the function was partially applied in advance");

			func = _.bind(func, this, "curly");
			equal(func(), "hello: curly", "the function was completely applied in advance");

			func = function (salutation, firstname, lastname) {
				return salutation + ": " + firstname + " " + lastname;
			};
			func = _.bind(func, this, "hello", "moe", "curly");
			equal(func(), "hello: moe curly", "the function was partially applied in advance and can accept multiple arguments");

			func = function (context, message) {
				equal(this, context, message);
			};
			_.bind(func, 0, 0, "can bind a function to `0`")();
			_.bind(func, "", "", "can bind a function to an empty string")();
			_.bind(func, false, false, "can bind a function to `false`")();

			// These tests are only meaningful when using a browser without a native bind function
			// To test this with a modern browser, set underscore's nativeBind to undefined
			var F = function () {
				return this;
			};
			var Boundf = _.bind(F, {
				hello: "moe curly"
			});
			var newBoundf = new Boundf();
			equal(newBoundf.hello, undefined, "function should not be bound to the context, to comply with ECMAScript 5");
			equal(Boundf().hello, "moe curly", "When called without the new operator, it's OK to be bound to the context");
			ok(newBoundf instanceof F, "a bound instance is an instance of the original function");
		});

		it("partial", function () {
			var obj = {
				name: "moe"
			};
			var func = function () {
				return this.name + " " + _.toArray(arguments).join(" ");
			};

			obj.func = _.partial(func, "a", "b");
			equal(obj.func("c", "d"), "moe a b c d", "can partially apply");
		});

		it("bindAll", function () {
			var curly = {
				name: "curly"
			}, moe = {
					name: "moe",
					getName: function () {
						return "name: " + this.name;
					},
					sayHi: function () {
						return "hi: " + this.name;
					}
				};
			curly.getName = moe.getName;
			_.bindAll(moe, "getName", "sayHi");
			curly.sayHi = moe.sayHi;
			equal(curly.getName(), "name: curly", "unbound function is bound to current object");
			equal(curly.sayHi(), "hi: moe", "bound function is still bound to original object");

			curly = {
				name: "curly"
			};
			moe = {
				name: "moe",
				getName: function () {
					return "name: " + this.name;
				},
				sayHi: function () {
					return "hi: " + this.name;
				}
			};

			// "throws an error for bindAll with no functions named"
			raises(function () {
				_.bindAll(moe);
			}, Error);

			_.bindAll(moe, "sayHi");
			curly.sayHi = moe.sayHi;
			equal(curly.sayHi(), "hi: moe");
		});

		it("memoize", function () {
			var fib = function (n) {
				return n < 2 ? n : fib(n - 1) + fib(n - 2);
			};
			equal(fib(10), 55, "a memoized version of fibonacci produces identical results");
			fib = _.memoize(fib); // Redefine `fib` for memoization
			equal(fib(10), 55, "a memoized version of fibonacci produces identical results");

			var o = function (str) {
				return str;
			};
			var fastO = _.memoize(o);
			equal(o("toString"), "toString", "checks hasOwnProperty");
			equal(fastO("toString"), "toString", "checks hasOwnProperty");
		});

		it("delay", function (done) {
			var delayed = false;
			_.delay(function () {
				delayed = true;
			}, 100);
			setTimeout(function () {
				ok(!delayed, "didn't delay the function quite yet");
			}, 50);
			setTimeout(function () {
				ok(delayed, "delayed the function");
				done();
			}, 150);
		});

		it("defer", function (done) {
			var deferred = false;
			_.defer(function (bool) {
				deferred = bool;
			}, true);
			_.delay(function () {
				ok(deferred, "deferred the function");
				done();
			}, 50);
		});

		it("throttle", function (done) {
			var counter = 0;
			var incr = function () {
				counter++;
			};
			var throttledIncr = _.throttle(incr, 32);
			throttledIncr();
			throttledIncr();

			equal(counter, 1, "incr was called immediately");
			_.delay(function () {
				equal(counter, 2, "incr was throttled");
				done();
			}, 64);
		});

		it("throttle arguments", function (done) {
			var value = 0;
			var update = function (val) {
				value = val;
			};
			var throttledUpdate = _.throttle(update, 32);
			throttledUpdate(1);
			throttledUpdate(2);
			_.delay(function () {
				throttledUpdate(3);
			}, 64);
			equal(value, 1, "updated to latest value");
			_.delay(function () {
				equal(value, 3, "updated to latest value");
				done();
			}, 96);
		});

		it("throttle once", function (done) {
			var counter = 0;
			var incr = function () {
				return ++counter;
			};
			var throttledIncr = _.throttle(incr, 32);
			var result = throttledIncr();
			_.delay(function () {
				equal(result, 1, "throttled functions return their value");
				equal(counter, 1, "incr was called once");
				done();
			}, 64);
		});

		it("throttle twice", function (done) {
			var counter = 0;
			var incr = function () {
				counter++;
			};
			var throttledIncr = _.throttle(incr, 32);
			throttledIncr();
			throttledIncr();
			_.delay(function () {
				equal(counter, 2, "incr was called twice");
				done();
			}, 64);
		});

		it("more throttling", function (done) {
			var counter = 0;
			var incr = function () {
				counter++;
			};
			var throttledIncr = _.throttle(incr, 30);
			throttledIncr();
			throttledIncr();
			ok(counter === 1);
			_.delay(function () {
				ok(counter === 2);
				throttledIncr();
				ok(counter === 3);
				done();
			}, 85);
		});

		it("throttle repeatedly with results", function (done) {
			var counter = 0;
			var incr = function () {
				return ++counter;
			};
			var throttledIncr = _.throttle(incr, 100);
			var results = [];
			var saveResult = function () {
				results.push(throttledIncr());
			};
			saveResult();
			saveResult();
			_.delay(saveResult, 50);
			_.delay(saveResult, 150);
			_.delay(saveResult, 160);
			_.delay(saveResult, 230);
			_.delay(function () {
				equal(results[0], 1, "incr was called once");
				equal(results[1], 1, "incr was throttled");
				equal(results[2], 1, "incr was throttled");
				equal(results[3], 2, "incr was called twice");
				equal(results[4], 2, "incr was throttled");
				equal(results[5], 3, "incr was called trailing");
				done();
			}, 300);
		});

		it("throttle triggers trailing call when invoked repeatedly", function (done) {
			var counter = 0;
			var limit = 48;
			var incr = function () {
				counter++;
			};
			var throttledIncr = _.throttle(incr, 32);

			var stamp = new Date();
			while ((new Date() - stamp) < limit) {
				throttledIncr();
			}
			var lastCount = counter;
			ok(counter > 1);

			_.delay(function () {
				ok(counter > lastCount);
				done();
			}, 96);
		});

		it("throttle does not trigger leading call when leading is set to false", function (done) {
			var counter = 0;
			var incr = function () {
				counter++;
			};
			var throttledIncr = _.throttle(incr, 60, {
				leading: false
			});

			throttledIncr();
			throttledIncr();
			ok(counter === 0);

			_.delay(function () {
				ok(counter === 1);
				done();
			}, 96);
		});

		it("more throttle does not trigger leading call when leading is set to false", function (done) {
			var counter = 0;
			var incr = function () {
				counter++;
			};
			var throttledIncr = _.throttle(incr, 100, {
				leading: false
			});

			throttledIncr();
			_.delay(throttledIncr, 50);
			_.delay(throttledIncr, 60);
			_.delay(throttledIncr, 200);
			ok(counter === 0);

			_.delay(function () {
				ok(counter === 1);
			}, 250);

			_.delay(function () {
				ok(counter === 2);
				done();
			}, 350);
		});

		it("one more throttle with leading: false test", function (done) {
			var counter = 0;
			var incr = function () {
				counter++;
			};
			var throttledIncr = _.throttle(incr, 100, {
				leading: false
			});

			var time = new Date();
			while (new Date() - time < 350) {
				throttledIncr();
			}
			ok(counter <= 3);

			_.delay(function () {
				ok(counter <= 4);
				done();
			}, 200);
		});

		it("throttle does not trigger trailing call when trailing is set to false", function (done) {
			var counter = 0;
			var incr = function () {
				counter++;
			};
			var throttledIncr = _.throttle(incr, 60, {
				trailing: false
			});

			throttledIncr();
			throttledIncr();
			throttledIncr();
			ok(counter === 1);

			_.delay(function () {
				ok(counter === 1);

				throttledIncr();
				throttledIncr();
				ok(counter === 2);

				_.delay(function () {
					ok(counter === 2);
					done();
				}, 96);
			}, 96);
		});

		it("debounce", function (done) {
			var counter = 0;
			var incr = function () {
				counter++;
			};
			var debouncedIncr = _.debounce(incr, 32);
			debouncedIncr();
			debouncedIncr();
			_.delay(debouncedIncr, 16);
			_.delay(function () {
				equal(counter, 1, "incr was debounced");
				done();
			}, 96);
		});

		it("debounce asap", function (done) {
			var a, b;
			var counter = 0;
			var incr = function () {
				return ++counter;
			};
			var debouncedIncr = _.debounce(incr, 64, true);
			a = debouncedIncr();
			b = debouncedIncr();
			equal(a, 1);
			equal(b, 1);
			equal(counter, 1, "incr was called immediately");
			_.delay(debouncedIncr, 16);
			_.delay(debouncedIncr, 32);
			_.delay(debouncedIncr, 48);
			_.delay(function () {
				equal(counter, 1, "incr was debounced");
				done();
			}, 128);
		});

		it("debounce asap recursively", function (done) {
			var counter = 0;
			var debouncedIncr = _.debounce(function () {
				counter++;
				if (counter < 10) {
					debouncedIncr();
				}
			}, 32, true);
			debouncedIncr();
			equal(counter, 1, "incr was called immediately");
			_.delay(function () {
				equal(counter, 1, "incr was debounced");
				done();
			}, 96);
		});

		it("once", function () {
			var num = 0;
			var increment = _.once(function () {
				num++;
			});
			increment();
			increment();
			equal(num, 1);
		});

		it("Recursive onced function.", function () {
			var f = _.once(function () {
				ok(true);
				f();
			});
			f();
		});

		it("wrap", function () {
			var greet = function (name) {
				return "hi: " + name;
			};
			var backwards = _.wrap(greet, function (func, name) {
				return func(name) + " " + name.split("").reverse().join("");
			});
			equal(backwards("moe"), "hi: moe eom", "wrapped the salutation function");

			var inner = function () {
				return "Hello ";
			};
			var obj = {
				name: "Moe"
			};
			obj.hi = _.wrap(inner, function (fn) {
				return fn() + this.name;
			});
			equal(obj.hi(), "Hello Moe");

			var noop = function () {};
			var wrapped = _.wrap(noop, function ( /*fn*/ ) {
				return Array.prototype.slice.call(arguments, 0);
			});
			var ret = wrapped(["whats", "your"], "vector", "victor");
			deepEqual(ret, [noop, ["whats", "your"], "vector", "victor"]);
		});

		it("compose", function () {
			var greet = function (name) {
				return "hi: " + name;
			};
			var exclaim = function (sentence) {
				return sentence + "!";
			};
			var composed = _.compose(exclaim, greet);
			equal(composed("moe"), "hi: moe!", "can compose a function that takes another");

			composed = _.compose(greet, exclaim);
			equal(composed("moe"), "hi: moe!", "in this case, the functions are also commutative");
		});

		it("after", function () {
			var testAfter = function (afterAmount, timesCalled) {
				var afterCalled = 0;
				var after = _.after(afterAmount, function () {
					afterCalled++;
				});
				while (timesCalled--) {
					after();
				}
				return afterCalled;
			};

			equal(testAfter(5, 5), 1, "after(N) should fire after being called N times");
			equal(testAfter(5, 4), 0, "after(N) should not fire unless called N times");
			equal(testAfter(0, 0), 0, "after(0) should not fire immediately");
			equal(testAfter(0, 1), 1, "after(0) should fire when first invoked");
		});

	});
})();
