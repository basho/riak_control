minispade.register('core', function() {
  RiakControl.Node = DS.Model.extend({
    name: DS.attr("string"),
    status: DS.attr("string"),
    reachable: DS.attr("boolean"),
    ring_pct: DS.attr("number"),
    pending_pct: DS.attr("number"),
    mem_total: DS.attr("number"),
    mem_used: DS.attr("number"),
    mem_erlang: DS.attr("number"),
    low_mem: DS.attr("boolean"),
    me: DS.attr("boolean")
  });
});
