'use strict';

describe('hibiTask directive', function () {
  beforeEach(module('app.directives'));

  var $compile, scope, elem, image;

  beforeEach(inject(function (_$compile_, _$rootScope_) {
    $compile = _$compile_;

    scope = _$rootScope_.$new();
    elem = $compile('<hibi-icon src="test.svg" title="Test Icon"/>')(scope);
    scope.$digest();
    image = elem.find('img');
  }));

  it('should render the icon image', function () {
    expect(image[0]).toBeDefined();
    expect(image.attr('src')).toEqual('test.svg');
  });

  it('should have a tooltip (title attr for now)', function () {
    expect(elem.attr('title')).toEqual('Test Icon');
  });

  it('should set the image alt text to the tooltip text', function () {
    expect(image.attr('alt')).toEqual('Test Icon');
  });

  it('should not fire a request to //{{src}} before the digest cycle finishes', function () {
    elem = $compile('<hibi-icon src="test.svg" />')(scope);
    image = elem.find('img');
    expect(image[0]).toBeDefined();
    expect(image.attr('src')).not.toBeDefined();

    scope.$digest();
    expect(image.attr('src')).toBeDefined();
  });

  describe('in a button', function () {
    var button;

    beforeEach(function () {
      button = $compile('<button><hibi-icon src="test.svg" title="Test Icon"/></button>')(scope);
      scope.$digest();
      elem = button.find('hibi-icon');
      image = elem.find('img');
    });

    it('should still render all the same elements', function () {
      expect(button[0]).toBeDefined();
      expect(elem[0]).toBeDefined();
      expect(image[0]).toBeDefined();
    });

    it('should give the button a tooltip too', function () {
      expect(button.attr('title')).toEqual('Test Icon');
    });
  });
});
