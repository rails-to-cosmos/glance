// Stand-in for table-view/web/table-view.js, enough for the serve tests: they
// check what the server does with an assets directory.  The real renderer is
// vendored at assets/table-view.js and compiled in; --assets serves this one.
(function (root) {
  "use strict";
  root.TableView = { mount: function () { return {}; } };
})(this);
