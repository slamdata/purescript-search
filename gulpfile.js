'use strict'

var gulp = require('gulp'),
    purescript = require('gulp-purescript'),
    mocha = require("gulp-mocha");

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
