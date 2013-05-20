var expandyIndicator = {
  "Collapsed": "☞",
  "Expanded": "☟"
};


function expandy(widgetId, handleSelector, targetSelector) {
  var indicator = $('#' + widgetId),
      handle = $(handleSelector),
      target = $(targetSelector);

  /*
   * Style expandy handle as we want, without requiring widget users to add
   * our CSS classes to their markup
   */
  handle.addClass('expandy-handle');

  handle.click(function () {
    target.slideToggle('fast', function () {
      if (target.css('display') === 'none') {
        indicator.html(' ' + expandyIndicator['Collapsed']);
      } else {
        indicator.html(' ' + expandyIndicator['Expanded']);
      }
    });
  });
}
