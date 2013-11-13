RiakControl.Store.reopen({
  adapter: DS.FixtureAdapter,
  simulateRemoteResponse: false
});

RiakControl.store = RiakControl.Store.create();
// fixtures are assigned in the tests where they are needed
RiakControl.Node.FIXTURES = [];
document.write('<div id="ember-testing-container"><div id="ember-testing"></div></div>');

RiakControl.rootElement = "#ember-testing";
RiakControl.setupForTesting();
RiakControl.injectTestHelpers();
