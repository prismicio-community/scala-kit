$(function() {

  /**
   * Pages setup
   */
  var setup = {

    'home': function() {
      return $.when( $('#home #caroussel').catslider() )
    },

    init: function() {
      (this[$('body > div.main').attr('id')] || (function() {}))()
    }

  }

  // Initial setup
  setup.init()

  /**
   * Pretty page transitions if the browser supports
   * HTML5 pushState
   */
  if(Modernizr.history) {

    var load = function(href) {
      return Helpers.scrollTop()
        .then(function() { 
          return Helpers.fade()
        })
        .then(function() { 
          return $.get(href) 
        })
        .then(function(html) {
          var $body = $($.parseHTML(html.match(/<body[^>]*>([\s\S.]*)<\/body>/i)[0])),
              $fragment = $body.filter('div.main')

          return { $el: $fragment, page: $fragment.attr('id') }
        })
        .then(function(loaded) {
          return $('body > div.main').attr('data-to', loaded.page).delay(250).promise().then(
            function() {
              $('body > div.main').attr('id', loaded.page).html(loaded.$el.html()).removeAttr('data-to')
              return loaded
            }
          )
        })
        .then(function(loaded) { 
          (setup[loaded.page] || (function() {}))()
          Helpers.fade()
          return loaded.page
        })
    }

    var url = document.location.toString()

    // Intercept clicks on links
    $(document.body).on('click', '[href]:not([target])', function(e) {
      e.preventDefault()

      var href = $(this).attr('href')
      if(href == document.location.pathname) return $.when('DONE')

      load(href).then(function(newState) {
        history.pushState(null, null, href)
        url = document.location.toString()
      })
    })

    // Intercept back/forward
    $(window).on('popstate', function(e) {
      if(document.location.toString() != url) {
        load(document.location.pathname)
      }
      url = document.location.toString()
    })

  }

  /**
   * Useful functions & helpers
   */
  var Helpers = {

    scrollTop: function() {
      return $(document.body).animate({scrollTop: 0}, Math.min(250, $(document.body).scrollTop())).promise()
    },

    fade: function() {
      var $el = $('body > div.main')
      return Helpers.defer(function() {
        return $el[$el.is('.fade') ? 'removeClass' : 'addClass']('fade').delay(250).promise()
      })
    },

    swap: function($el, newPage) {
      return this.scrollTop()
        .then(function() { return Helpers.fade().then(function() { return $('body > div.main').attr('data-to', newPage).delay(250).promise() }) })
        .then(function() { $.when( $('body > div.main').attr('id', newPage).html($el.html()).removeAttr('data-to') ) })
        .then(function() { Helpers.fade(); return $.when('DONE')  })
    },

    defer: function(f) {
      var p = $.Deferred()
      setTimeout(function() {
        f().then(function(x) { p.resolve(x) })
      },0)
      return p
    }

  }

})