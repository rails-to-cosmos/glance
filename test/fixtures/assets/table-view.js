// Stand-in for table-view/web/table-view.js, enough for the serve tests: they
// check what the server does with an assets directory, not what the renderer
// draws.  The real renderer is vendored at assets/table-view.js and compiled
// in; this file is what --assets serves over it.
(function (root) {
  "use strict";
  root.TableView = { mount: function () { return {}; } };
})(this);
