"use strict";
/*
I'm absolutely not certain how this should be implemented
if someone wants to implement it from inside a node
application.

There is one guy who developed a npm modules for that.

I considered two solutions:
a) Reject the requests if the machine is to loaded
(i.e. check something like the processor load average
and decided to reject requests based on that).

b) Reject requests if they seem to be queued for
too long.

I decided to implement b. It might not work at all
for some reason (I did not write any test, so it is
 a bit of joke solution). Anyway.

Other detail: my processing is essentially synchronous:
there is no IO: everything is executed by the CPU by
looking up a trie. So yes it takes memory to store the
data structure, but it should also be very fast.

So here is what I do: I make all requests be processed
on the 'nextTick' and then check how long it took to
reach the point where they are actually executed. If
this is too long I skip the processing returning
the proper http response.
*/
var MITIGATION_TIMEOUT = 700;

/**
 * Returns a handler that will return 503s if the server overheats, bypassing the normal processing, and hopefully keeping things quieter.
 *
 * @param {Function} handler the function that should be called if the server is still responsive.
 * @return {Function} the handler that will do the mitigation
 * @api public
 */
function mitigate(handler) {
    return function (req, res) {
        var now = Date.now();
        // in normal situations, nextTick should
        // be almost immediate
        process.nextTick(function () {
            var then = Date.now();
            if ((then - now) > MITIGATION_TIMEOUT) {
                // timeout: the server is probably overloaded
                res.writeHead(503, {'Content-Type': 'text/plain'});
                req.end();
            } else {
                // everything seems right
                handler(req, res);
            }
        });
    };
}
exports.mitigate = mitigate;
