var glob = require("glob"),
    fs = require("fs");

function getModules(pats) {
    if (!pats) {
        throw new Error("provide patterns to get names");
    }
    if (!(pats instanceof Array)) {
        pats = [pats];
    }
    var res = [];
    pats.forEach(function(el, i) {
        res = res.concat(glob.sync(el));
    });
    var modules = [];
    res.forEach(function(filename) {
        var data = fs.readFileSync(filename, {encoding: "utf-8"});
        var lines = data.split("\n");
        lines.forEach(function(line) {
            var match = line.match(/module [a-zA-Z0-9.]+/g);
            if (match) {
                modules.push(match[0].replace("module ", ""));
            }
        });
    });
    return modules;
}

function getFiles(pats, dir) {
    if (dir === undefined) {
        dir = ".";
    }
    var modules = getModules(pats);
    var files = [];
    modules.forEach(function(module) {
        files.push(dir + "/" + module + "/index.js");
    });
    return files;
}

module.exports = {
    modules: getModules,
    files: getFiles
};


