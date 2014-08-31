/*
 * can't use casper.cli since "casperjs test" treats all args as test filenames
 */
function envOrDie(name) {
  var value = require('system').env[name];
  if (value === undefined) {
    casper.die('Must pass ' + name + ' environment variable!', 1);
  }
  return value;
}

var testerEmail = envOrDie('TESTER_EMAIL');
var testerPassword = envOrDie('TESTER_PASSWORD');

var root = envOrDie('ROOT_URL');
if (root[root.length - 1] !== '/') {
  root = root + '/';
}
function url(path) {
  if (path === undefined) path = '';
  path = path.replace(/^\//, '');
  return root + path;
}

var shotNumber = 0;
function shot(name) {
  return casper.capture('smoke-' + (shotNumber++) + '-' + name + '.png');
}

casper.test.begin('Front page should load', 1, function (test) {
  casper.start(url(), function () {
    shot('front-page');
    test.assertSelectorHasText('h1', 'Hibi');
  });

  casper.run(function () {
    test.done();
  });
});

casper.test.begin('Can log in, and add and complete a task', 3, function (test) {
  casper.start(url(), function () {
    casper.click('a[href$="/auth/login"]');

    casper.then(function () {
      shot('login-page');
      casper.click('a[href$="/googleemail/forward"]');
    });

    casper.then(function () {
      shot('google-login');
      test.assertTextExists('Sign in with your Google Account');

      casper.fill('form#gaia_loginform', {
        Email: testerEmail,
        Passwd: testerPassword
      }, true);
    });

    casper.then(function () {
      var approvalForm = document.querySelector('form#connect-approve');
      if (approvalForm) {
        shot('google-approval-form');
        approvalForm.submit();
      }
    });

    var taskId = (Math.random() * 10000000).toFixed();

    casper.then(function () {
      shot('logged-in');
      test.assertSelectorHasText('header', 'Hibi');

      var form = '.new-task form';

      casper.fill(form, {
        title: 'Test task ' + taskId
      }, true);

      casper.waitFor(function () {
        return casper.fetchText(form + ' *[name=title]').length == 0;
      }, function () {
        shot('added-task');
        var lastTask = 'ul.tasks > *:last-child .task';
        test.assertSelectorHasText(lastTask, taskId);

        casper.click(lastTask + ' .title');

        casper.waitFor(function () {
          return !casper.fetchText('.task').match(taskId);
        }, function () {
          shot('completed-task');
        });
      });
    });
  });

  casper.run(function () {
    test.done();
  });
});
