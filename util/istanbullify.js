var istanbul = require("istanbul"),
    through = require("through"),
    path = require("path");

module.exports = function(patterns) {
    function transform(file) {
        var inList = patterns.some(function(el) {
            return path.resolve(el) == path.resolve(file);
        });


        if (!inList) {
            return through();
        }
        
        var instrumenter = new istanbul.Instrumenter({});
        var data = "";
        return through(function(buf) {
            data += buf;
        }, function() {
            var self = this;
            instrumenter.instrument(data, file, function(err, code) {
                if (!err) {
                    self.queue(code);
                } else {
                    self.emit("error", err);
                }
                self.queue(null);
            });
        });
    }
    return transform;
};
