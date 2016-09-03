'use strict';

var defaultController = {


    /**
     *
     * index
     *
     * @params req {Request}
     * @params res {Response}
     * */
    index : function(req, res) {

        // pass a local variable to the view
        return res.render('index', { name: 'Tobi' }, function(err, html) {
            return res.send(html);
        });

    }
};

module.exports = defaultController;