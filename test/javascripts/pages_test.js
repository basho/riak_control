module("Pages", {
  setup: function() {
    RiakControl.reset();
    RiakControl.Node.FIXTURES = [{
      id: "low_mem_node",
      status: "valid",
      reachable: true,
      ring_pct: 0.1875,
      pending_pct: 0.1875,
      mem_total: 100,
      mem_used: 100,
      mem_erlang: 100,
      low_mem: true,
      me: true,
      claimant: false
    }];

    $.mockjax({
      url: "/admin/cluster",
      responseText: MockResponses.ClusterResponse
    });

    $.mockjax({
      url: "/admin/partitions",
      responseText: MockResponses.PartitionsResponse
    });

    $.mockjaxSettings.logging = false;
  },

  teardown: function() {
    $.mockjaxClear();
  }
});

test("snapshot page shows problematic nodes", function() {
  visit("/").then(function() {
    equal(find("#snapshot-headline").text(), "Current Snapshot");
    equal(find("#low_mem-nodes-list li").length, 1, "Detects 1 low memory node");
  });
});

test("cluster page lists changes", function() {
  visit("/cluster").then(function() {
    equal(find("#cluster-headline").text(), "Cluster Management");
    ok(find("#planned-area").text().indexOf("leave") !== -1, "Displays the leaving node");
  });
});

test("nodes page lists nodes", function() {
  visit("/nodes").then(function() {
    equal(find("#node-headline").text(), "Node Management");
    ok(find("#node-list").text().indexOf("low_mem_node") !== -1, "Displays the low mem node")
  });
})

test("ring page shows partitions", function() {
  visit("/ring").then(function() {
    equal(find("#ring-headline").text(), "Ring Status");
    equal(find("#partition-container div div.ember-view").length, 64);
  });
})
