rekonApp = Sammy('#container', function(){

  $container = $(this.$element);

  header = function(header, url) {
    $container.find('h1').html(header + " &ndash; <em> " + url + "</em>");
  };

  breadcrumb = function(crumb) {
    $('<li>').append(crumb).appendTo('ul#footer-navi');
  };

  searchable = function(selector) {
    $('#row_search').quicksearch(selector, {selector: 'th'});
  };
  
  this.use('Template');
  this.use('NestedParams');

  this.before(function(){
    $('#main').empty();
    $('#content h1').html('');
    $('#footer-navi li:not(.perm)').remove();
  });

  this.get('#/buckets', function(context){
    header('Buckets', Rekon.baseUrl());

    context.render('buckets.html.template').appendTo('#main');
    
    Rekon.client.buckets(function(buckets) {
      bucketRows = buckets.map(function(bucket){ return {bucket: bucket};});
      context.renderEach('bucket-row.html.template', bucketRows).replace('#buckets tbody').then(
        function(){ searchable('#buckets table tbody tr'); }
      );
    });
  });

  this.get('#/buckets/:bucket', function(context){
    var name   = this.params['bucket'];
    var bucket = new RiakBucket(name, Rekon.client);
    
    header('Bucket', Rekon.riakUrl(name));
    breadcrumb($('<a>').attr('href', '#/bucket-props/' + name).text('Props'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name)).attr('target', '_blank').text('Riak').addClass('action'));

    context.render('bucket.html.template', {bucket: name}).appendTo('#main');

    bucket.keys(function(keys) {
      if (keys.length > 0) {
        keyRows = keys.map(function(key) { return {bucket:name, key:key}; });
        context.renderEach('key-row.html.template', keyRows).replace('#keys tbody').then(
          function(){ searchable('#bucket table tbody tr'); }
        );
      } else {
        context.render('bucket-empty.html.template').replace('#keys tbody');
      }
    });
  });

  this.get('#/bucket-props/:bucket', function(context) {
    var name   = this.params['bucket'];
    var bucket = new RiakBucket(name, Rekon.client);

    header('Bucket Properties', Rekon.riakUrl(name));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name).text('Keys'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name)).attr('target', '_blank').text('Riak').addClass('action'));

    bucket.getProps(function(props) {
      var pre_commit, post_commit;
      pre_commit  = props.precommit.join(",");
      post_commit = props.postcommit.join(",");
      if(pre_commit === "") {pre_commit = "None";}
      if(post_commit === "") {post_commit = "None";}
      context.render('bucket-hooks.html.template', {pre_commit: pre_commit, post_commit: post_commit},
        function(){
          context.render('bucket-props.html.template', {props: props}).appendTo('#main').then(function(){
            var $selects, $select, i;
            $selects = $('select[data-select-value]');
            /* select the nvalue */
            $('select#n_val').val($('select#n_val').attr('data-select-value'));
            /* bind the limit based off of the n_val */
            Rekon.capControlsSelector();
            /* reselect cap control vals based off of nval */
            for(i=0; i<$selects.length;i++) { 
              $select = $($selects[i]);
              $select.val($select.attr('data-select-value'));
            }
            $('select#n_val').change(Rekon.capControlsSelector);
          });
        }
      ).appendTo('#main');
    });
  });

  this.get('#/buckets/:bucket/:key', function(context) {
    var name   = this.params['bucket'];
    var key    = this.params['key'];
    var bucket = new RiakBucket(name, Rekon.client);

    header('Key', Rekon.riakUrl(name + '/' + key));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name).text('Keys'));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name + '/' + key + '/edit').text('Edit').addClass('action'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name + '/' + key)).attr('target', '_blank').
      text('Riak').addClass('action'));

    context.render('key.html.template').appendTo('#main');

    bucket.get(key, function(status, object) {
      context.render('key-content-type.html.template', {object: object}, function(){
        context.render('key-meta.html.template', {object: object}).appendTo('#key tbody');
      }).appendTo('#key tbody');

      switch(object.contentType) {
      case 'image/png':
      case 'image/jpeg':
      case 'image/jpg':
      case 'image/gif':
        context.render('value-image.html.template', {bucket: name, key: key}).appendTo('#value');
        return;
      case 'application/json':
        value = JSON.stringify(object.body, null, 4);
        break;
      default:
        value = object.body;
        break;
      }
      context.render('value-pre.html.template', {value: value}).appendTo('#value');
    });
  });

  this.get('#/buckets/:bucket/:key/edit', function(context) {
    var name   = this.params['bucket'];
    var key    = this.params['key'];
    var bucket = new RiakBucket(name, Rekon.client);
    var app    = this;

    header('Edit Key', Rekon.riakUrl(name + '/' + key));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name).text('Keys'));
    breadcrumb($('<a>').attr('href', '#/buckets/' + name + '/' + key).text('View').addClass('action'));
    breadcrumb($('<a>').attr('href', Rekon.riakUrl(name + '/' + key)).attr('target', '_blank').
      text('Riak').addClass('action'));

    context.render('edit-key.html.template', {bucket: name, key: key}).appendTo('#main');

    bucket.get(key, function(status, object) {
      switch(object.contentType) {
      case 'image/png':
      case 'image/jpeg':
      case 'image/jpg':
      case 'image/gif':
        alert('Image editing is not supported currently.');
        app.redirect('#/buckets/' + name + '/' + key);
        return;
      case 'application/json':
        value = JSON.stringify(object.body, null, 4);
        break;
      default:
        value = object.body;
        break;
      }
      context.render('edit-key-content-type.html.template', {object: object}, function(html){
        context.render('key-meta.html.template', {object: object}).appendTo('#edit-key tbody');
      }).appendTo('#edit-key tbody').then(function(html){
        $select = $('select[name=content-type]');
        $select.val(object.contentType);
      });
      context.render('edit-value.html.template', {value: value}).appendTo('#edit-value');
    });
  });

  this.get('#/stats', function(context){
    header('Node Stats', document.location.origin + "/stats");

    $.getJSON('/stats', function(data) {
      context.render('stats.html.template', {stats:data}).appendTo('#main').then(
        function(){ searchable('#stats tbody tr'); }
      );
    });
  });

  this.post('#/buckets', function(context) {
    var name = this.params['bucket'];
    this.redirect('#/buckets/' + name);
  });

  this.post('#/buckets/:bucket', function(context){
    var app    = this;
    var name   = this.params['bucket'];
    var key    = this.params['key'] === '' ? undefined : this.params['key'];
    var object = new RiakObject(name, key, Rekon.client, '{}', 'application/json');
    object.store(function(status, rObject){
      switch(status) {
      case 'siblings':
        alert("Oh noes! Siblings have been born and Rekon doesn't handle that yet.");
        break;
      case 'failure':
        alert("There was an error creating a new Riak object.");
        break;
      case 'ok':
      default:
        console.log(rObject);
        app.redirect('#/buckets/' + name + '/' + rObject.key);
        break;
      }
    });
  });

  this.post('#/bucket-props/:bucket', function(context) {
    var app      = this;
    var name     = this.params['bucket'];
    var bucket   = new RiakBucket(name, Rekon.client);
    var props    = Rekon.typecastBucketProps(this.params['props']);

    bucket.props = props;
    bucket.store(function(){
      app.redirect("#/bucket-props/" + name);
    });
  });

  this.post('#/buckets/:bucket/:key', function(context){ 
    var app    = this;
    var name   = this.params['bucket'];
    var key    = this.params['key'];
    var bucket = new RiakBucket(name, Rekon.client);

    bucket.get(key, function(status, object) {
      object.contentType = app.params['content-type'];
      object.body        = app.params['value'];

      if (object.contentType == 'application/json') {
        object.body = JSON.parse(object.body);
      }

      object.store(function(status, rObject) {
        switch(status) {
        case 'siblings':
          alert("Oh noes! Siblings have been born and Rekon doesn't handle that yet.");
          break;
        case 'failure':
          alert("There was an error saving to Riak.");
          break;
        case 'ok':
        default:
          app.redirect('#/buckets/' + name + '/' + key);
          break;
        }
      });
    });
  });

  this.get('#/stats', function(context){
    header('Node Stats', document.location.origin + "/stats");

    $.getJSON('/stats', function(data) {
      context.render('stats.html.template', {stats:data}).appendTo('#main').then(
        function(){ searchable('#stats tbody tr'); }
      );
    });
  });

  this.get('#/luwak', function(context){
    luwak = new Luwak(Rekon.client);

    header('Luwak', document.location.origin + "/luwak");
    context.render('luwak.html.template').appendTo('#main').then(function(){

      luwak.files(function(files) {
        if (files === null) {
          console.log('not working');
          $('#files .pending td').html(
          '<p><b>Luwak is not enabled.</b> Please add <code>{luwak, [{enabled, true}]}</code> to your app.config.</p>');
        }
        else if (files.length > 0) {
          fileRows = files.map(function(file){ return {file:file};});
          context.renderEach('luwak-row.html.template', fileRows).replace('#files tbody').then(
            function() { searchable('#luwak tbody'); }
          );
        } else{
          $('#files .pending td').html('<p>You have not added any files to luwak.</p>');
        }
      });
    });
  });

});

Rekon = {
  client : new RiakClient(),

  locationUrl : function() {
    return document.location.protocol + '//' + document.location.host;
  },

  baseUrl : function() {
    return this.locationUrl() + this.client.baseUrl;
  },

  luwakUrl : function() {
    return this.locationUrl() + this.client.luwakUrl;
  },

  riakUrl : function(append) {
    if (append === undefined) {
      append = "";
    }
    return this.baseUrl() + append;
  },

  typecastBucketProps : function(props) {
    keys = ['w', 'r', 'dw', 'rw', 'n_val', 'young_vclock', 'old_vclock', 'small_vclock', 'big_vclock'];
    for(var i=0; i<keys.length; i++) {
      key = keys[i];
      val = parseInt(props[key], 10);
      if (val) {
        props[key] = parseInt(props[key], 10);
      }
    }
    props.allow_mult      = !!props.allow_mult;
    props.last_write_wins = !!props.last_write_wins;

    return props;
  },

  capControlsSelector : function() {
    var nVal = parseInt($('select#n_val').val(), 10);
    $('.cap-control').each(function(i, select) {
      var $select = $(select);
      var value   = parseInt($select.val(), 10);
      var endVal  = parseInt($select.find('option:last').val(), 10);
      if (isNaN(endVal)) { endVal = 0; }

      /* figure out if we need to append or trim */
      if (endVal > nVal) {
        $select.find('option').each(function(j, option) {
          var $option = $(option);
          if (parseInt($option.val(), 10) > nVal) {
            $option.remove();
          }
        });
        if (value) {
          $select.val($select.find('option:last').val());
        }
      } 
      else if (endVal < nVal) {
        while(endVal < nVal) {
          endVal++;
          $('<option>').val(endVal).html(endVal).appendTo($select);
        } 
      }
    });
  }

};

$('#keys a.delete').live('click', function(e){
  var link = this;
  e.preventDefault();
  if(!confirm("Are you sure you want to delete:\n" + $(link).attr('href'))) { return; }

  $.ajax({
    type: 'DELETE',
    url: $(link).attr('href')
  }).success(function(){
    $(link).closest('tr').remove();
  }).error(function(){
    alert('There was an error deleting this object from Riak.');
  });
});

var filterInteger = function(){
  var value = parseInt($(this).val(), 10);
  if (isNaN(value)) {
    value = 1;
  }
  $(this).val(value);
};

$("input[data-filter=integer]").live('blur', filterInteger);

/*
* Bootstrap the application
*/
jQuery(function($) {
  rekonApp.run('#/buckets');

});
