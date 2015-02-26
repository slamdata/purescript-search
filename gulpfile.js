var gulp = require("gulp"),
    purescript = require("gulp-purescript"),
    browserify = require("gulp-browserify"),
    concat = require("gulp-concat"),
    sequence = require("run-sequence"),
    rename = require("gulp-rename"),
    coveralls = require("gulp-coveralls"),
    mocha = require("gulp-mocha"),

    br = require("browserify"),
    childProcess = require("child_process"),
    runnerMaker = require("./util/runner.js"),

    source = require('vinyl-source-stream'),
    buffer = require('vinyl-buffer'),

    handleError = require("./util/handle-error.js"),
    
    karma = require("karma").server,
    nameGetter = require("./util/get-make-names.js"),
    tr = require("./util/istanbullify.js");

var paths = {
    src: "src/**/*.purs",
    bowerSrc: [
        "bower_components/purescript-*/src/**/*.purs",
        "bower_components/purescript-*/src/**/*.purs.hs"
    ],
    concatSrc: [
    ],
    dest: {
        file: "build.js",
        dir: "public",
        dist: "dist",
        tmp: "dist/node_modules",
        entry: "dist/main.js",
        testEntry: "dist/test.js",
        testBuild: "testBundle.js"
    },
    docs: {
        dest: "MODULES.md",
        src: ["src/**/*.purs"]
    },
    test: ["test/**/*.purs"]
};

// ====================================================================== //

gulp.task("base-runner", function() {
    runnerMaker.main();
});

gulp.task("test-runner", function() {
    var modules = nameGetter.modules([paths.src]);
    runnerMaker.test("test.js", "Test.Main", modules, "dist");
});

gulp.task("runner", function(cb) {
    sequence("base-runner", "test-runner", cb);
});

gulp.task("docs", function() {
    var docgen = purescript.pscDocs();
    handleError(docgen);
    return gulp.src(paths.docs.src)
        .pipe(docgen)
        .pipe(gulp.dest(paths.docs.dest));
});

gulp.task("psci", function() {
    gulp.src(
        [paths.src].concat(paths.bowerSrc).concat(paths.test)
    ).pipe(purescript.dotPsci({}));
});

gulp.task("copy-node-modules", function() {
    return gulp.src(["node_modules/**/*"])
        .pipe(gulp.dest("dist/node_modules"));
});

// ====================================================================== //
// Compiling
// ====================================================================== //
gulp.task("make-prod", function() {
    var psc = purescript.psc({
        output: paths.dest.file,
        modules: ["Main"],
        main: "Main"
    });
    handleError(psc);
    return gulp.src(
        [paths.src].concat(paths.bowerSrc))
        .pipe(psc)
        .pipe(gulp.dest(paths.dest.dist));
});

gulp.task("make-dev", function() {
    var psc = purescript.pscMake({
        output: paths.dest.tmp
    });
    handleError(psc);
    return gulp.src(
        [paths.src].concat(paths.bowerSrc).concat(paths.test)
    )
        .pipe(psc);
});
// ====================================================================== //
// Bundling
// ====================================================================== //
function bundleIt(entry) {
    return function() {
        var bundler = br({
            entries: [entry]
        });
        var bundle = function() {
            return bundler
                .bundle()
                .pipe(source(paths.dest.file))
                .pipe(buffer())
                .pipe(gulp.dest(paths.dest.dist));
        };
        return bundle();
    };
}
gulp.task("bundle-dev", ["make-dev"], bundleIt("./" + paths.dest.entry));
gulp.task("bundle-prod", ["make-prod"],  bundleIt("./" + paths.dest.dist + "/" + paths.dest.file));

gulp.task("bundle-test", function() {
    var bundler = br({
        entries: ["./dist/test.js"]
    });
    var pats = nameGetter.files([paths.src], "./dist/node_modules");
    var bundle = function() {
        return bundler
            .transform(tr(pats), {global: true})
            .bundle()
            .pipe(source("testBundle.js"))
            .pipe(buffer())
            .pipe(gulp.dest(paths.dest.dist));
    };
    return bundle();
});

// ====================================================================== //
// Tests
// ====================================================================== //

gulp.task("karma", function(cb) {
    karma.start({
        configFile: __dirname + "/karma.conf.js",
        action: "run",
        singleRun: true
    }, cb);
});


gulp.task("karma-watch", function(cb) {
    karma.start({
        configFile: __dirname + "/karma.conf.js",
        action: "run"
    }, cb);
});


gulp.task("cover", function() {
    gulp.src("./coverage/**/lcov.info")
        .pipe(coveralls());
});

// ====================================================================== //
// Aggregates
// ====================================================================== //

gulp.task("concat-build", ["bundle-dev"], function() {
    gulp.src(paths.concatSrc.concat([paths.dest.dist + "/" + paths.dest.file]))
        .pipe(concat("concated.js"))
        .pipe(gulp.dest("public"));
});

gulp.task("test-dev", function(cb) {
    sequence(
        "make-dev",
        "bundle-test",
        cb);
});

gulp.task("test", function(cb) {
    sequence(
        "copy-node-modules",
        "runner",
        "test-dev",
        "karma",
        "cover",
        cb);
});

gulp.task("prod", function(cb) {
    sequence(
        "copy-node-modules",
        "make-prod",
        "bundle-prod",
        "concat-build",
        cb);
});

gulp.task("compile-dev", ["runner", "make-dev", "bundle-dev"]);



// ====================================================================== //
// Watchers
// ====================================================================== //

gulp.task("watch-dev", function() {
    gulp.watch(
        [paths.src].concat(paths.test),
        ["compile-dev", "concat-build",  "docs"]
    );
});


gulp.task("watch-test", function() {
    gulp.watch(
        [paths.src].concat(paths.test),
        ["test-dev"]
    );
});

gulp.task("dev", [
    "copy-node-modules",
    "runner",
    "compile-dev",
    "concat-build",
    "watch-dev"
]);

gulp.task("tdd", [
    "copy-node-modules",
    "runner",
    "test-dev",
    "watch-test"
]);


gulp.task("default", ["dev"]);




