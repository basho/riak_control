RiakControl.Store.reopen({
  adapter: DS.FixtureAdapter,
  simulateRemoteResponse: false
});

RiakControl.store = RiakControl.Store.create();
// fixtures are assigned in the tests where they are needed
RiakControl.Node.FIXTURES = [];

RiakControl.rootElement = "#ember-testing";
RiakControl.setupForTesting();
RiakControl.injectTestHelpers();
