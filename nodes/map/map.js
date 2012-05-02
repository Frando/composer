var collections = {};

collections["spots"] = {
  
  // New Spots
  // -------------------

  enter: function(spots) {
    var that = this;
    spots.each(function(spot, key, index) {
      var spot = $(_.template($('script[name=nav_spot]').html(), {
        spot: spot,
        active: that.activeSpot === spot
      })).css('left', spot.pos.x)
         .css('bottom', -70)
         .appendTo($('.spots-navigation'));

    });
    _.delay(this.collections["spots"].update, 0, spots)
  },

  // Existing Spots
  // -------------------

  update: function(spots) {
    spots.each(function(spot) {
      $('#'+_.htmlId(spot._id))
       .css('left', spot.pos.x)
       .css('bottom', 10)
       .css('background-image', "url('http://a.tiles.mapbox.com/v3/mapbox.mapbox-streets/"+spot.get('longitude')+","+spot.get('latitude')+",11/100x100.png')")
    });
  },

  // Removed Spots
  // -------------------

  exit: function(spots) {
    spots.each(function(spot) {
      $('#'+_.htmlId(spot._id)).remove();
    });
  }
};


sc.views.Node.define('/type/map', {

  className: 'content-node map',

  collections: collections,

  events: {
    'change .name': '_updateData',
    'change .descr': '_updateData',
    'click .spots-navigation .spot': '_gotoSpot',
    'click .remove-spot': '_removeSpot'
  },

  _removeSpot: function(e) {
    var spot = this.spots.get($(e.currentTarget).parent().attr('data-id'));
    this.map.removeLayer(spot.marker);
    this.spots.del(spot._id);
    this.activeSpot = null;
    this.trigger('update', this.spots);
    this.render();
    return false;
  },

  _gotoSpot: function(e) {
    this.gotoSpot(this.spots.get($(e.currentTarget).attr('data-id')));
  },

  _updateData: function(e) {
    this.activeSpot.name = this.$('.name').val();
    this.activeSpot.descr = this.$('.descr').val();
  },


  // Contructor
  // -------------------

  initialize: function(options) {

  },


  // Calculating layout
  // -------------------

  layout: function(property) {
    this.data["spots"].each(function(spot, key, index) {
      spot.pos = {
        x: index*100,
      };
    });
  },

  // Jump to Spot
  // -------------------

  gotoSpot: function(spot) {
    this.activeSpot = spot;
    this.render();
    this.map.setView(new L.LatLng(spot.get('latitude'), spot.get('longitude')), 15);
  },

  // Jump to next Spot
  // -------------------

  nextSpot: function() {
    if (!this.activeSpot) return this.gotoSpot(this.spots.first());
    var currentIndex = this.spots.index(this.activeSpot.id);
    this.gotoSpot(this.spots.at((currentIndex + 1) % this.spots.length));
  },

  // Jump to previous Spot
  // -------------------

  prevSpot: function() {
    if (!this.activeSpot) return this.gotoSpot(this.spots.last());
    var currentIndex = this.spots.index(this.activeSpot.id);
    if (currentIndex<=0) return this.gotoSpot(this.spots.last());
    this.gotoSpot(this.spots.at((currentIndex - 1) % this.spots.length));
  },

  // Add a new spot
  // -------------------

  addSpot: function(lat, lng, name, descr, id, silent) {
    var spot = {
      id: id ? id : this.map._container.id + Data.uuid(),
      name: name || 'Untitled',
      descr: descr || 'Undescribed.' ,
      latitude: lat,
      longitude: lng
    };

    var s = this.data["spots"].add(spot);
    s.marker = new L.Marker(new L.LatLng(lat, lng), { draggable: true });

    // Update when dragged.
    function drag(e) {
      var pos = e.target._latlng;
      spot.latitude = pos.lat;
      spot.longitude = pos.lng;
      this.render();
      this.trigger('update', this.spots);
    }
    
    function click(e) {
      this.activeSpot = s;
      this.render();
    }

    s.marker.on('drag', _.bind(drag, this));
    s.marker.on('click', _.bind(click, this));
    this.map.addLayer(s.marker);

    if (!silent) this.trigger('update', this.spots);
    return s;
  },


  // Keyboard navigation - a pleasure
  // -------------------

  registerKeyBindings: function() {
    // $(document)
    //   .keydown('right', _.bind(function() { this.nextSpot(); this.render(); }, this))
    //   .keydown('left',  _.bind(function() { this.prevSpot(); this.render(); }, this))
    //   .keydown('esc',  _.bind(function() { this.activeSpot = null; this.render(); }, this));
  },


  // Register Map Events
  // -------------------

  registerMapEvents: function() {
    var that = this;
    var clickCount = 0;
    // Add new spot, every time the map gets clicked
    this.map.on('click', function(e) {
      if (that.activeSpot) { }

      clickCount += 1;
      if (clickCount <= 1) {
        _.delay(function() {
          if (clickCount <= 1) {
            that.activeSpot = that.addSpot(e.latlng.lat, e.latlng.lng);
            that.render();
          }
          clickCount = 0;
        }, 300);
      }
    });
  },

  // Render the beast
  // -------------------

  focus: function () {
    $(this.textEl).click();
  },

  select: function () {
    sc.views.Node.prototype.select.apply(this);
  },

  deselect: function () {
    sc.views.Node.prototype.deselect.apply(this);
  },

  renderMap: function() {
    var that = this;

    _.delay(function() {
      that.map = new L.Map('map', {
        layers: new L.TileLayer('http://a.tiles.mapbox.com/v3/mapbox.mapbox-streets/{z}/{x}/{y}.png', {}),
        center: new L.LatLng(51.505, -0.09),
        zoom: 13,
        maxZoom: 17,
        attributionControl: false
      });

      that.spots = that.data["spots"] = new Data.Collection({
        "type": {
          "_id": "/type/spot",
          "name": "Spots",
          "properties": {
            "latitude": {"name": "Latitude", "type": "number" },
            "longitude": {"name": "Longitude", "type": "number" },
            "name": { "name": "Name", "type": "string"},
            "descr": { "name": "Description", "type": "string" }
          }
        }
      });

      var spots = [
        {
          "_id": "/spot/1",
          "latitude": 38.91310029076162,
          "longitude": -77.03267812728882,
          "name": "MapBox",
          "descr": "MapBox Headquarters in Washington DC"
        },
        {
          "_id": "/spot/2",
          "latitude": 48.30293692666153,
          "longitude": 14.294521808624268,
          "name": "Quasipartikel",
          "descr": "The Quasipartikel Quasioffice in Linz, Austria"
        },
        {
          "_id": "/spot/3",
          "latitude": 40.689253770619686,
          "longitude": -74.04454708099365,
          "name": "Statue of Liberty",
          "descr": "Sightseeing, anyone?"
        }
      ];

      that.activeSpot = null;

      _.each(spots, function(s) {
        that.addSpot(s.latitude, s.longitude, s.name, s.descr, s.id, true);
      }, that);

      this.activeSpot = that.spots.first();
      that.registerMapEvents();
      that.registerKeyBindings();
    }, 50);

  },

  // Render the beast
  // -------------------

  render: function () {
    var that = this;
    if (!this.rendered) {
      sc.views.Node.prototype.render.apply(this, arguments);
      $(this.contentEl).html(_.tpl('map', this.model));
      this.renderMap();
      this.rendered = true;
    }

    _.delay(function() {
      that.layout();
      that.refresh();

      that.$('.spots-navigation .spot.active').removeClass('active');
      if (that.activeSpot) {
        that.$('.spot-details').replaceWith(_.template($('script[name=spot]').html(), {
          spot: that.activeSpot
        }));
        that.$('#'+ _.htmlId(that.activeSpot._id)).addClass('active');
      } else {
        that.$('.spot-details').empty();
      }
    }, 100);

    return this;
  }
});