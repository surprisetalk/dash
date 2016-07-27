
var gulp = require('gulp');
var nodemon = require('gulp-nodemon');
// var jshint = require('gulp-jshint');
 
// TODO: https://github.com/gulpjs/gulp/tree/master/docs/recipes
// TODO: browser-sync: https://github.com/gulpjs/gulp/blob/master/docs/recipes/server-with-livereload-and-css-injection.md
// TODO: css preprocessing

gulp.task('start', function () {
  nodemon({
    script: 'server.js'
  , watch: '../'
  , ext: 'js html jade'
  , env: { 'NODE_ENV': 'development' }
  });
});
 
// gulp.task('lint', function () {
//   gulp.src('./**/*.js')
//     .pipe(jshint());
// });

// gulp.task('develop', function () {
//   nodemon({ script: 'server.js'
//           , ext: 'html js'
//           , ignore: ['ignored.js']
//           , tasks: ['lint'] })
//     .on('restart', function () {
//       console.log('restarted!');
//     });
// });
