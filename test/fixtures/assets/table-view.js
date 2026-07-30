// Stand-in for table-view/web/table-view.js, enough for the serve tests: they
// check what the server does with an assets directory, not what the renderer
// draws.  The real renderer is a sibling checkout, pointed at with --assets.
(function (root) {
  "use strict";
  root.TableView = { mount: function () { return {}; } };
})(this);
