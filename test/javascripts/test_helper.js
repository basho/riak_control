RiakControl.Store.reopen({
  adapter: DS.FixtureAdapter,
  simulateRemoteResponse: false
});

RiakControl.store = RiakControl.Store.create();

RiakControl.setupForTesting();
RiakControl.injectTestHelpers();
