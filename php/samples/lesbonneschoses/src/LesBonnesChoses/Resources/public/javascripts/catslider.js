;( function( $, window, undefined ) {

  'use strict';

  $.CatSlider = function( options, element ) {
    this.$el = $( element );
    this._init( options );
  };

  $.CatSlider.prototype = {

    _init : function( options ) {

      // the categories (ul)
      this.$categories = this.$el.find( '.products > ul' );
      // the navigation
      this.$navcategories = this.$el.find( 'nav a' );
      var animEndEventNames = {
        'WebkitAnimation' : 'webkitAnimationEnd',
        'OAnimation' : 'oAnimationEnd',
        'msAnimation' : 'MSAnimationEnd',
        'animation' : 'animationend'
      };
      // animation end event name
      this.animEndEventName = animEndEventNames[ Modernizr.prefixed( 'animation' ) ];
      // animations and transforms support
      this.support = Modernizr.csstransforms && Modernizr.cssanimations;
      // if currently animating
      this.isAnimating = false;
      // current category
      this.current = 0;
      var $currcat = this.$categories.eq( 0 );
      if( !this.support ) {
        this.$categories.hide();
        $currcat.show();
      }
      else {
        $currcat.addClass( 'current' );
      }
      // current nav category
      this.$navcategories.eq( 0 ).addClass( 'selected' );
      // initialize the events
      this._initEvents();

    },
    _initEvents : function() {

      var self = this;
      this.$navcategories.on( 'click.catslider', function() {
        self.showCategory( $( this ).parent('li').index() );
        return false;
      } );

      // reset on window resize..
      $( window ).on( 'resize', function() {
        self.$categories.removeClass().eq( 0 ).addClass( 'current' );
        self.$navcategories.eq( self.current ).removeClass( 'selected' ).end().eq( 0 ).addClass( 'selected' );
        self.current = 0;
      } );

    },
    showCategory : function( catidx ) {
      if( catidx === this.current || this.isAnimating ) {
        return false;
      }
      this.isAnimating = true;
      // update selected navigation
      this.$navcategories.eq( this.current ).removeClass( 'selected' ).end().eq( catidx ).addClass( 'selected' );

      var dir = catidx > this.current ? 'right' : 'left',
        toClass = dir === 'right' ? 'moveToLeft' : 'moveToRight',
        fromClass = dir === 'right' ? 'moveFromRight' : 'moveFromLeft',
        // current category
        $currcat = this.$categories.eq( this.current ),
        // new category
        $newcat = this.$categories.eq( catidx ),
        $newcatchild = $newcat.children(),
        lastEnter = dir === 'right' ? $newcatchild.length - 1 : 0,
        self = this;

      if( this.support ) {

        $currcat.removeClass().addClass( toClass );
        
        setTimeout( function() {

          var setup = function() {

            $( this ).off( self.animEndEventName );
            $newcat.addClass( 'current' );
            self.current = catidx;
            var $this = $( this );
            // solve chrome bug
            self.forceRedraw( $this.get(0) );
            self.isAnimating = false;

          }

          $newcat.removeClass().addClass( fromClass );
          $newcatchild.eq( lastEnter ).on( self.animEndEventName, setup);
          if(!$newcatchild.length) setTimeout(setup, $currcat.children().length * 90)

        }, $newcatchild.length * 90 );

      }
      else {

        $currcat.hide();
        $newcat.show();
        this.current = catidx;
        this.isAnimating = false;

      }

    },
    forceRedraw : function(element) {
      if (!element || !element.appendChild) { return; }
      var n = document.createTextNode(' '),
        position = element.style.position;
      element.appendChild(n);
      element.style.position = 'relative';
      setTimeout(function(){
        element.style.position = position;
        n.parentNode.removeChild(n);
      }, 25);
    }

  }

  $.fn.catslider = function( options ) {
    var instance = $.data( this, 'catslider' );
    if ( typeof options === 'string' ) {
      var args = Array.prototype.slice.call( arguments, 1 );
      this.each(function() {
        instance[ options ].apply( instance, args );
      });
    }
    else {
      this.each(function() {
        instance ? instance._init() : instance = $.data( this, 'catslider', new $.CatSlider( options, this ) );
      });
    }
    return instance;
  };

} )( jQuery, window );