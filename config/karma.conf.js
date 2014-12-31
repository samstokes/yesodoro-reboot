// Karma configuration

// Force timezone to Hawaii, since:
//  * it's fixed (doesn't depend on current system timezone)
//  * we're not usually in Hawaii so it'll probably catch timezone bugs
process.env['TZ'] = 'US/Hawaii';

module.exports = function(config) {
  config.set({

    // base path, that will be used to resolve files and exclude
    basePath: '..',


    // frameworks to use
    frameworks: ['jasmine'],


    // list of files / patterns to load in the browser
    files: [
      'static/js/vendor/jquery-1.6.4.js',
      'static/js/vendor/angular-1.1.5.js',
      'static/js/lib/**/*.js',
      'static/js/app/**/*.js',
      'node_modules/jasmine-jquery/lib/jasmine-jquery.js',
      'tests/jasmine/support/**/*.js',
      'tests/jasmine/**/*Spec.js'
    ],


    // list of files to exclude
    exclude: [
    ],


    // test results reporter to use
    // possible values: 'dots', 'progress', 'junit', 'growl', 'coverage'
    reporters: ['spec', 'coverage'],


    coverageReporter: {
      reporters: [
        {type: 'html', dir: 'coverage/'},
        {type: 'text-summary'} // to stdout
      ]
    },


    preprocessors: {
      'static/js/lib/**/*.js': ['coverage'],
      'static/js/app/**/*.js': ['coverage']
    },


    // web server port
    port: 9876,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,


    // Start these browsers, currently available:
    // - Chrome
    // - ChromeCanary
    // - Firefox
    // - Opera (has to be installed with `npm install karma-opera-launcher`)
    // - Safari (only Mac; has to be installed with `npm install karma-safari-launcher`)
    // - PhantomJS
    // - IE (only Windows; has to be installed with `npm install karma-ie-launcher`)
    browsers: ['PhantomJS'],


    // If browser does not capture in given timeout [ms], kill it
    captureTimeout: 60000,


    // Continuous Integration mode
    // if true, it capture browsers, run tests and exit
    singleRun: true
  });
};
