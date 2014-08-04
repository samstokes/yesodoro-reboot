'use strict';

describe('hibiNote directive', function () {
  // mock out angular.markdown directive so we just test it's called.
  beforeEach(module(function ($compileProvider) {
    $compileProvider.directive('markdown', function () {
        return {restrict: 'E', scope: {ngModel: '='}, template: 'MKD {{ngModel}}'};
      });
  }));

  beforeEach(module('app.directives'));

  var note = {
    note: {
      body: 'I have discovered a *truly* marvellous proof, but this margin \n' +
            'is too small to contain it.\n\n' +
            '- Fermat'
    }
  };

  var $compile, scope, elem;

  beforeEach(inject(function (_$compile_, _$rootScope_) {
    $compile = _$compile_;

    scope = _$rootScope_.$new();
    scope.note = note;

    elem = $compile('<li hibi-note="note" />')(scope);
    scope.$digest();
  }));

  it('should have the class .note', function () {
    expect(elem.find('.note')).not.toBeEmpty();
  });

  it('should render the note body with class .body', function () {
    var body = elem.find('.note .body');
    expect(body.text()).toContain('marvellous proof');
  });

  it('should render the note body using the markdown directive', function () {
    expect(elem.text()).toMatch(/^MKD/);
  });

  it('should render the note creation time with class .created', function () {
    expect(elem.find('.note .created')).not.toBeEmpty();
  });
});
