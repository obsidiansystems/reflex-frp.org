const gulp = require('gulp');
const nunjucksRender = require('gulp-nunjucks-render');

gulp.task('default', () => {
  return gulp.src('src/html/pages/**/*.+(html|njk)')
    .pipe(nunjucksRender({
      path: ['src/html/templates/']
    }))
    .pipe(gulp.dest('public'));
});
