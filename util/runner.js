var fs = require("fs");

function indir(dir, run) {
    fs.stat(dir, function(err, stats) {
        if (err) {
            fs.mkdir(dir, function(err) {
                if (err) throw err;
                run();
            });
        }
        run();
    });
}


function testLauncher(entry, main, modules, distDir) {
    if (distDir === undefined) {
        distDir = "dist";
    }
    if (main === undefined) {
        main = "Main";
    }
    if (modules === undefined) {
        modules = [];
    }
    var run = function() {
        var path = distDir + "/" + entry;
        var content = "";
        modules.forEach(function(module) {
            content += "require('" + module + "');\n";
        });
        content += "require('" + main + "').main()";
        fs.writeFileSync(path, content);
    };
    return indir(distDir, run);
}



function mainLauncher(module, entry, distDir) {
    if (distDir === undefined) {
        distDir = "dist";
    }
    if (module === undefined) {
        module = "Main";
    }
    if (entry === undefined) {
        entry = "main.js";
    }
    var run = function() {
        var path = distDir + "/" + entry;
        var content = "require('" + module + "').main()";
        fs.writeFileSync(path, content);
    };
    return indir(distDir, run);
}

module.exports = {
    test: testLauncher,
    main: mainLauncher
};

