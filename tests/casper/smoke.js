/*
 * can't use casper.cli since "casperjs test" treats all args as test filenames
 */
var root = require('system').env['ROOT_URL'];
if (root === undefined) {
  casper.echo('Must pass ROOT_URL environment variable!').exit();
}

if (root[root.length - 1] !== '/') {
  root = root + '/';
}
function url(path) {
  if (path === undefined) path = '';
  path = path.replace(/^\//, '');
  return root + path;
}

casper.test.begin('Front page should load', 1, function (test) {
  casper.start(url(), function () {
    test.assertSelectorHasText('h1', 'Hibi');
  });

  casper.run(function () {
    test.done();
  });
});
