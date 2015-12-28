'use strict';

var gulp = require('gulp'),
    purescript = require('gulp-purescript');

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
        output: 'public/test.js'
    });
});

gulp.task("bundle", ["make"], function() {
    return purescript.pscBundle({
        src: "output/**/*.js",
        main: "Main",
        output: "index.js"
    });
});

gulp.task("default", ["test-bundle", "docs"]);
