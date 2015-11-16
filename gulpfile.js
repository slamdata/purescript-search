'use strict'

var gulp = require('gulp'),
    purescript = require('gulp-purescript'),
    mocha = require('gulp-mocha'),
    runSequence = require('run-sequence'),
    run = require('gulp-run');


function sequence() {
    var args = [].slice.apply(arguments);
    return function() {
        runSequence.apply(null, args);
    };
}

var sources = [
    'src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs'
];

var foreigns = [
    'src/**/*.js',
    'bower_components/purescript-*/src/**/*.js'
];

var testSources = [
    'test/**/*.purs'
];

var testForeigns = [
    'test/**/*.js'
];

gulp.task('docs', function() {
    return purescript.pscDocs({
        src: sources,
        docgen: {
            "Text.SlamSearch": "docs/Text/SlamSearch.md",
            "Text.SlamSearch.Types": "docs/Text/SlamSearch/Types.md",
            "Text.SlamSearch.Printer": "docs/Text/SlamSearch/Printer.md",
            "Text.SlamSearch.Parser": "docs/Text/SlamSearch/Parser.md",
            "Text.SlamSearch.Parser.Tokens": "docs/Text/SlamSearch/Parser/Tokens.md"
        }
    });
});

gulp.task('make', function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task('test-make', function() {
    return purescript.psc({
        src: sources.concat(testSources),
        ffi: foreigns.concat(testForeigns)
    });
});


gulp.task('test-bundle', ['test-make'], function() {
    return purescript.pscBundle({
        src: 'output/**/*.js',
        main: 'Test.Main',
        output: 'dist/test.js'
    });
});

gulp.task('test', ['test-bundle'], function() {
    return gulp.src("dist/test.js").pipe(mocha());
});

gulp.task('default', ['make']);
