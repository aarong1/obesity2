// Packery functionality for Shiny app
$(document).ready(function() {
  
  // Initialize Packery when the page loads
  setTimeout(function() {
    var $grid = $('.grid').packery({
      itemSelector: '.grid-item',
      percentPosition: true
    });

    // Handle click events on grid items
    $grid.on('click', '.grid-item-content', function(event) {
      var itemContent = event.currentTarget;
      setItemContentPixelSize(itemContent);

      var itemElem = itemContent.parentNode;
      var $item = $(itemElem);
      var isExpanded = $item.hasClass('is-expanded');
      $(itemElem).toggleClass('is-expanded');

      // force redraw
      var redraw = itemContent.offsetWidth;
      // renable default transition
      itemContent.style[transitionProp] = '';

      addTransitionListener(itemContent);
      setItemContentTransitionSize(itemContent, itemElem);

      if (isExpanded) {
        // if shrinking, shiftLayout
        $grid.packery('shiftLayout');
      } else {
        // if expanding, fit it
        $grid.packery('fit', itemElem);
      }
    });
  }, 100); // Small delay to ensure DOM is ready

  // Browser compatibility for transitions
  var docElem = document.documentElement;
  var transitionProp = typeof docElem.style.transition == 'string' ?
      'transition' : 'WebkitTransition';
  var transitionEndEvent = {
    WebkitTransition: 'webkitTransitionEnd',
    transition: 'transitionend'
  }[transitionProp];

  function setItemContentPixelSize(itemContent) {
    var previousContentSize = getSize(itemContent);
    // disable transition
    itemContent.style[transitionProp] = 'none';
    // set current size in pixels
    itemContent.style.width = previousContentSize.width + 'px';
    itemContent.style.height = previousContentSize.height + 'px';
  }

  function addTransitionListener(itemContent) {
    // reset 100%/100% sizing after transition end
    var onTransitionEnd = function() {
      itemContent.style.width = '';
      itemContent.style.height = '';
      itemContent.removeEventListener(transitionEndEvent, onTransitionEnd);
    };
    itemContent.addEventListener(transitionEndEvent, onTransitionEnd);
  }

  function setItemContentTransitionSize(itemContent, itemElem) {
    // set new size
    var size = getSize(itemElem);
    itemContent.style.width = size.width + 'px';
    itemContent.style.height = size.height + 'px';
  }

  // Utility function to get element size (using jQuery or native methods)
  function getSize(elem) {
    var $elem = $(elem);
    return {
      width: $elem.outerWidth(),
      height: $elem.outerHeight()
    };
  }
});