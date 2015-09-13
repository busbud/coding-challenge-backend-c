// clint: a stupidly tiny command line interface helper
// inspired by commander.js

"use strict"

var prime   = require("prime"),
    Emitter = require("prime/util/emitter")

var Clint = prime({

    constructor: function(){
        this.shortcuts = {}
        this.rshortcuts = {}
        this.commands = {}
        this.parsers = {}
    }

})

Clint.implement(new Emitter)

var indent = function(num){
    var str = ""
    for (var i = 0; i < num; i++) str += " "
    return str
}

Clint.implement({

    // simply returns the (optionally indented) help string, separated with their description with your separator or :

    help: function(indentation, separator){

        if (separator == null) separator = " : "

        indentation = indent((indentation == null) ? 2 : indentation)

        var helpstring = "",
            commands = []

        for (var option in this.commands){
            var shortcut = this.rshortcuts[option]
            if (shortcut) option += ", " + shortcut
            commands.push(option)
        }

        var max = Math.max.apply(Math, commands.map(function(c){
            return c.length
        })), i = 0

        for (var option in this.commands){
            var usage = this.commands[option], command = commands[i]
            if (usage && usage !== true) helpstring += indentation + command + indent(max - command.length) + separator + usage + "\n"
            i++
        }

        return helpstring

    },

    // sets a command. usage: .command("--help", "-h" or null, "helps people", optionalParser)

    command: function(mm, m, msg, parse){
        this.commands[mm] = msg || true
        this.parsers[mm] = parse

        if (m){
            this.shortcuts[m] = mm
            this.rshortcuts[mm] = m
        }

        return this
    }

})

var defaultParser = function(arg){
    return arg
}

var execute = function(self, command, args){
    if (!args || !args.length) args = [null]

    var chunk = args.map(function(arg){
        var parsed = (self.parsers[command] || defaultParser)(arg)
        self.emit("command", command, parsed)
        return parsed
    })

    self.emit.apply(self, ["chunk", command].concat(chunk))
}


Clint.implement({

    // starts parsing and emit events. usage: .parse(process.argv.slice(2)) to parse command line arguments.

    parse: function(args){

        var temp = [], command

        args.forEach(function(arg, i){
            var theCommand = this.commands[arg] && arg || this.shortcuts[arg]

            if (theCommand){
                if (command) execute(this, command, temp)
                command = theCommand
                temp = null
            } else if (command){
                (temp || (temp = [])).push(arg)
            }

        }, this)

        if (command) execute(this, command, temp)
        this.emit('complete')

        return this

    }

})

module.exports = function(){
    return new Clint
}
