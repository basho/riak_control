Ember.TEMPLATES['application'] = Ember.Handlebars.compile('<div id="header">    <div id="navbar">        <a id="riak-control-logo"></a>        <nav>            <ul id="nav-ul">                <li id="nav-ring" class="nav-li"><a {{action showRing href=true}} class="gui-text-bold nav-item"></a><span class="indicator"></span></li>                <li id="nav-nodes" class="nav-li"><a {{action showNodes href=true}} class="gui-text-bold nav-item"></a><span class="indicator"></span></li>                <li id="nav-cluster" class="nav-li"><a {{action showCluster href=true}} class="gui-text-bold nav-item"></a><span class="indicator"></span></li>                <li id="nav-snapshot" class="nav-li"><a {{action showSnapshot href=true}} class="gui-text-bold nav-item"></a><span class="indicator"></span></li>            </ul>        </nav>    </div></div><div id="wrapper" class="split gui-text">    <section id="content-well">{{outlet}}</section>        <footer>        <div class="side-line"></div>        <div class="title-box">            <span class="vert-border-left"></span>            <a id="basho-logo" href="http://www.basho.com" target="_blank"><img src="/admin/ui/images/basho-logo.png" alt=""/></a>            <span class="vert-border-right"></span>        </div>        <div class="side-line"></div>        <div class="clear"></div>    <footer></div><!-- #wrapper --><div id="tooltips" class="hide">    <div id="display-tips" class="gui-text"></div></div>');
Ember.TEMPLATES['snapshot'] = Ember.Handlebars.compile('<div id="snapshot-page">    <section id="title-container">    <div class="side-line"></div>    <div class="title-box">      <span class="vert-border-left"></span>      <h1 id="snapshot-headline" class="gui-headline-bold page-title">Current Snapshot</h1>      <span class="vert-border-right"></span>    </div>    <div class="side-line"></div>    <div class="clear"></div>  </section>  <div class="relative health-info">    {{#if healthyCluster}}      <div id="healthy-cluster">          <img id="health-indicator" src="/admin/ui/images/healthy-cluster.png" alt="" />          <section>            <h2 class="gui-headline-bold has-cut">Your cluster is healthy.</h2>            <h3 class="">You currently have...</h3>            <ul class="gui-text bulleted">                <li><span class="emphasize monospace">0</span> Unreachable nodes</li>                <li><span class="emphasize monospace">0</span> Incompatible nodes</li>                <li><span class="emphasize monospace">0</span> Nodes marked as down</li>                <li><span class="emphasize monospace">0</span> Nodes experiencing low memory</li>                <li>Nothing to worry about because Riak is your friend</li>            </ul>          </section>      </div>    {{else}}      <div id="unhealthy-cluster">          <img id="health-indicator" src="/admin/ui/images/unhealthy-cluster.png" alt="" />          <section>            <h2 class="gui-headline-bold has-cut">Your cluster has problems.</h2>            {{#if areUnreachableNodes}}              <!-- Unreachable Nodes List -->              <h3 id="unreachable-nodes-title" class="">The following nodes are currently unreachable:</h3>              <ul id="unreachable-nodes-list" class="gui-text bulleted monospace">                {{#each unreachableNodes}}                  <li><a class="go-to-cluster" {{action showCluster href=true}}>{{name}}</a></li>                {{/each}}              </ul>            {{/if}}            {{#if areIncompatibleNodes}}              <!-- Incompatible Nodes List -->              <h3 id="incompatible-nodes-title" class="">The following nodes are currently incompatible with Riak Control:</h3>              <ul id="incompatible-nodes-list" class="gui-text bulleted monospace">                {{#each incompatibleNodes}}                  <li><a class="go-to-cluster" {{action showCluster href=true}}>{{name}}</a></li>                {{/each}}              </ul>            {{/if}}            {{#if areDownNodes}}              <!-- Down Nodes List -->              <h3 id="down-nodes-title" class="">The following nodes are currently marked down:</h3>              <ul id="down-nodes-list" class="gui-text bulleted monospace">                {{#each downNodes}}                  <li><a class="go-to-cluster" {{action showCluster href=true}}>{{name}}</a></li>                {{/each}}              </ul>            {{/if}}            {{#if areLowMemNodes}}              <!-- Low-Mem Nodes List -->              <h3 id="low_mem-nodes-title" class="">The following nodes are currently experiencing low memory:</h3>              <ul id="low_mem-nodes-list" class="gui-text bulleted monospace">                {{#each lowMemNodes}}                  <li><a class="go-to-cluster" {{action showCluster href=true}}>{{name}}</a></li>                {{/each}}              </ul>            {{/if}}          </section>      </div>    {{/if}}  </div>  </div>');
Ember.TEMPLATES['cluster'] = Ember.Handlebars.compile('<div id="cluster-page">    <section id="title-container">      <div class="side-line"></div>      <div class="title-box">        <span class="vert-border-left"></span>        <h1 id="cluster-headline" class="gui-headline-bold page-title">Cluster Management</h1>        <span class="vert-border-right"></span>      </div>      <div class="side-line"></div>      <div class="clear"></div>    </section>    <div id="add-node">        <h2 class="gui-headline">Join Nodes</h2>        <span class="gui-text-flat italic">Type a node name or list of names separated by commas.</span>        <table class="add-node-table">            <tr class="no-highlight">                <td id="add-node-box">                    {{view RiakControl.AddNodeView id="node-to-add"}}                </td>                <td class="button-column">                    <a class="gui-point-button gui-text-bold right" {{action addNode target="controller"}}>                        <span class="gui-button-msg">ADD NODES</span>                    </a>                </td>            </tr>        </table>        {{#if errorMessage}}            <div class="error-message">                <a class="close-error gui-text" {{action hideError target="controller"}}></a>                <a class="error-text offline gui-text-flat">{{errorMessage}}</a>            </div>        {{/if}}    </div><!-- #add-node -->    <div id="current-area">        <h2 class="gui-headline">            Current Cluster            <span id="total-number" class="gui-text-flat italic"></span><br/>        </h2>        <section id="node-list">            {{#if controller.isLoading}}                <div class="spinner-box">                    <img id="cluster-spinner" class="spinner" src="/admin/ui/images/spinner.gif">                    <h4 class="gui-headline-bold">Loading...</h4>                </div>            {{else}}                <ul class="list-header">                    <li class="item1"><h4 class="gui-headline-bold">Actions</h4></li>                    <li class="item2"><h4 class="gui-headline-bold">Name &amp; Status</h4></li>                    <li class="item3"><h4 class="gui-headline-bold">Partitions</h4></li>                    <li class="item4"><h4 class="gui-headline-bold">RAM Usage</h4></li>                </ul>                <div class="clear"></div>                {{collection RiakControl.CurrentClusterView contentBinding="activeCurrentCluster"}}            {{/if}}        </section>    </div>    <div id="area-separator">        <div class="vert-line"></div>    </div>    <div id="planned-area">        <h2 class="gui-headline">            Staged Changes            <span class="gui-text-flat italic">(Your new cluster after convergence.)</span>        </h2>        {{#if controller.displayPlan}}            <section id="planned-list" class="">                <ul class="list-header">                    <li class="item1"><h4 class="gui-headline-bold">Name &amp; Status</h4></li>                    <li class="item2"><h4 class="gui-headline-bold">Partitions</h4></li>                    <li class="item3"><h4 class="gui-headline-bold">Action</h4></li>                    <li class="item4"><h4 class="gui-headline-bold">Replacement</h4></li>                </ul>                <div class="clear"></div>                {{collection RiakControl.StagedClusterView contentBinding="activeStagedCluster"}}            </section>            <div class="accept-plan">                <div class="gui-checkbox-wrapper">                    <label for="confirmed-check">This plan is correct.</label>                    <input class="gui-checkbox" type="checkbox" name="confirmed" id="confirmed-check" value="accept"/>                  </div>                <a id="commit-button" class="gui-point-button-right gui-text-bold right" {{action commitPlan target="controller"}}>                    <span class="gui-button-msg">COMMIT</span>                </a>            </div>            <div class="clear-plan-box">                <span class="gui-text-flat serif">                    Changed your mind? Click this button to remove all staged changes.                </span>                <a class="gui-rect-button gui-text-bold" {{action clearPlan target="controller"}}>                    <span class="gui-button-msg">CLEAR PLAN</span>                </a>            </div>        {{else}}            <section id="planned-list">                <div class="spinner-box">                    {{#if controller.ringNotReady}}                        <h4 class="gui-headline-bold">                            Cannot plan until cluster state has converged. Check "Ring Ready" in "riak-admin ring_status".                        </h4>                    {{else}}                        {{#if controller.legacyRing}}                            <h4 class="gui-headline-bold">You are currently running a legacy version of Riak that does not support staged changes.</h4>                        {{else}}                            {{#if controller.emptyPlan}}                                <h4 class="gui-headline-bold">Currently no staged changes to display.</h4>                            {{else}}                                {{#if controller.isLoading}}                                    <img id="cluster-spinner" class="spinner" src="/admin/ui/images/spinner.gif">                                    <h4 class="gui-headline-bold">Loading...</h4>                                {{/if}}                            {{/if}}                        {{/if}}                    {{/if}}                </div>            </section>        {{/if}}    </div>    <div class="clear"></div>    </div>');
Ember.TEMPLATES['nodes'] = Ember.Handlebars.compile('<div id="nodes-page">    <section id="title-container">      <div class="side-line"></div>      <div class="title-box">        <span class="vert-border-left"></span>        <h1 id="node-headline" class="gui-headline-bold page-title">Node Management</h1>        <span class="vert-border-right"></span>      </div>      <div class="side-line"></div>      <div class="clear"></div>    </section>    <div id="current-area">        <h2 class="gui-headline">            Current Cluster<br/>        </h2>        <span id="total-number" class="gui-text-flat italic">          Click the radio button for each node you would like to stop or mark as down, then click "APPLY" to apply your changes. If the radio button is grayed out, the action is not          available due to the current status of the node.        </span><br/>        <section id="node-list">            {{#if controller.isLoading}}                <div class="spinner-box">                    <img id="cluster-spinner" class="spinner" src="/admin/ui/images/spinner.gif">                    <h4 class="gui-headline-bold">Loading...</h4>                </div>            {{else}}                <ul class="list-header">                    <li class="item1"><h4 class="gui-headline-bold">Stop</h4></li>                    <li class="item2"><h4 class="gui-headline-bold">Down</h4></li>                    <li class="item3"><h4 class="gui-headline-bold">Name &amp; Status</h4></li>                    <li class="item4"><h4 class="gui-headline-bold">Partitions</h4></li>                    <li class="item5"><h4 class="gui-headline-bold">RAM Usage</h4></li>                </ul>                <div class="clear"></div>                {{collection RiakControl.CurrentNodesView contentBinding="content"}}            {{/if}}        </section>        <section class="buttons">          <a class="gui-point-button-right gui-text-bold right" {{action applyChanges target="controller"}}>            <span class="gui-button-msg">APPLY</span>          </a>          <a class="gui-rect-button gui-text-bold right" {{action clearChecked target="controller"}}>            <span class="gui-button-msg">CLEAR</span>          </a>          <div class="clear"></div>        </section>    </div>    </div>');
Ember.TEMPLATES['ring'] = Ember.Handlebars.compile('<div id="ring-page">  <section id="title-container">    <div class="side-line"></div>    <div class="title-box">      <span class="vert-border-left"></span>      <h1 id="ring-headline" class="gui-headline-bold page-title">Current Ring</h1>      <span class="vert-border-right"></span>    </div>    <div class="side-line">      {{outlet partitionFilter}}    </div>    <div class="clear"></div>  </section>  <ul class="pagination first gui-text">    <li name="prev"><span class="paginator" {{action prevPage href=true target="controller"}}>Prev</span></li>    {{#each pages}}      {{view RiakControl.PaginationItemView contentBinding="this"}}    {{/each}}    <li name="next"><span class="paginator" {{action nextPage href=true target="controller"}}>Next</span></li>  </ul>  <div class="cut"></div>  <div id="partition-list">      <table class="list-table" id="ring-table">          <thead>              <tr class="table-head has-cut">                  <th><h3>#</h3></th>                  <th><h3>Owner Node</h3></th>                  <th><h3>KV</h3></th>                  <th><h3>Pipe</h3></th>                  <th><h3>Search</h3></th>              </tr>          </thead>          {{#collection RiakControl.PartitionView contentBinding="controller.paginatedContent"}}            {{#with view.content}}            <td class="partition-number gui-text">{{i}}</td>            <td class="owner-box gui-text">                <div class="owner gui-field">{{node}}</div>                <div class="partition-index hide">{{index}}</div>            </td>            {{/with}}            {{#with view}}            <td class="kv-box gui-text">                <a {{bindAttr class="kvIndicator lightClasses"}}>                    <span class="kv-status">{{kvStatus}}</span>                    <span class="hide fallback-to"></span>                </a>            </td>            <td class="pipe-box gui-text">                <a {{bindAttr class="pipeIndicator lightClasses"}}>                    <span class="pipe-status">{{pipeStatus}}</span>                    <span class="hide fallback-to"></span>                </a>            </td>            <td class="search-box gui-text">                <a {{bindAttr class="searchIndicator lightClasses"}}>                    <span class="search-status">{{searchStatus}}</span>                    <span class="hide fallback-to"></span>                </a>            </td>            {{/with}}          {{/collection}}      </table>  </div>  <div class="cut"></div>  <ul class="pagination gui-text">    <li name="prev"><span class="paginator" {{action prevPage href=true target="controller"}}>Prev</span></li>    {{#each pages}}      {{view RiakControl.PaginationItemView contentBinding="this"}}    {{/each}}    <li name="next"><span class="paginator" {{action nextPage href=true target="controller"}}>Next</span></li>  </ul>  </div>');
Ember.TEMPLATES['partition_filter'] = Ember.Handlebars.compile('<div id="ring-filter" class="right">    <div class="gui-dropdown-wrapper">        <div class="gui-dropdown-bg gui-text">Filter by...</div>        <div class="gui-dropdown-cap left"></div>        {{view RiakControl.PartitionFilterSelectView id="filter" classNames="gui-dropdown" contentBinding="filters" optionLabelPath="content.name" optionValuePath="content.value" prompt="All" selectionBinding="controller.selectedPartitionFilter"}}    </div></div>');
Ember.TEMPLATES['pagination_item'] = Ember.Handlebars.compile('{{#with view}}<a {{action paginateRing content href=true}}>  <span {{bindAttr class="spanClasses isActive:active"}}>{{content.page_id}}</span></a>{{/with}}');
Ember.TEMPLATES['current_cluster_item'] = Ember.Handlebars.compile('{{#with view}}    <div class="node">        <div class="item1 toggle-container">            {{#view RiakControl.CurrentClusterToggleView}}                <div class="actions-toggle gui-field">                    <a class="slider"></a>                </div>            {{/view}}        </div>        <div class="item2 name-box gui-text">            <div {{bindAttr class="indicatorLights"}}>            </div><div class="gui-text field-container inline-block">                <div class="name gui-field">{{name}}</div>            </div>        </div>        <div class="item3 gui-text ring-pct-box">            <div {{bindAttr class="coloredArrows"}}></div>            <div class="left gui-text pct-box">                <span class="ring-pct">{{ringPctReadable}}%</span>            </div>            <div class="clear"></div>        </div>        <div class="item4 gui-text memory-box">            {{#if reachable}}                <div class="membar-bg">                    <div class="mem-colors">                        <div class="erlang-mem mem-color" {{bindAttr style="memErlangStyle"}} {{bindAttr name="memErlangCeil"}}></div>                        <div class="non-erlang-mem mem-color" {{bindAttr style="memNonErlangStyle"}} {{bindAttr name="memNonErlang"}}></div>                        <div class="unknown-mem" {{bindAttr style="memFreeStyle"}} {{bindAttr name="memFreeReadable"}}></div>                    </div>                    <div class="membar-fg"></div>                </div>                <span class="used-memory">{{memUsedReadable}}%</span>            {{else}}                <div class="membar-bg">                    <div class="mem-colors">                        <div class="unknown-mem" style="width: 100%"></div>                    </div>                    <div class="membar-fg"></div>                </div>                <span class="used-memory"></span>            {{/if}}        </div>        <div class="clear"></div>        <!-- Actions container -->        <div class="actions-container">            <div class="actions-pointer"></div>            <div class="actions-box">                <h4 class="gui-headline-bold">                    Use these actions to prepare this node to leave the cluster.                </h4>                {{#if me}}                    <span class="warning gui-text-flat italic">                        Warning: This node is hosting Riak Control.  If it leaves the                        cluster, Riak Control will be shut down.                    </span>                {{/if}}                <div class="replacement-controls">                    <div class="gui-radio-wrapper default">                        <input class="gui-radio" type="radio" value="leave" {{bindAttr name="name" id="normalLeaveRadio"}} checked="checked"/>                        <label class="serif" {{bindAttr for="normalLeaveRadio"}}>Allow this node to leave normally.</label>                    </div>                    <div class="gui-radio-wrapper">                        <input class="gui-radio" type="radio" value="remove" {{bindAttr name="name" id="forceLeaveRadio"}} />                        <label class="serif" {{bindAttr for="forceLeaveRadio"}}>Force this node to leave.</label>                    </div>                    <div {{bindAttr class="replaceRadioClasses"}}>                        <input class="gui-radio" type="radio" value="replace" {{bindAttr name="name" id="replaceRadio"}} />                        <label class="serif" {{bindAttr for="replaceRadio"}}>Choose a new node to replace this one.</label>                    </div>                    <div class="extra-actions">                        <div class="right-angle-arrow"></div>                        {{#if controller.joiningNodesExist}}                            <div class="gui-dropdown-wrapper replacement-node-dropdown">                                <div class="gui-dropdown-bg gui-text">Select Replacement Node</div>                                <div class="gui-dropdown-cap left"></div>                                {{view Ember.Select classNames="gui-dropdown" contentBinding="controller.joiningNodes" optionLabelPath="content.name"}}                            </div>                            <div class="gui-checkbox-wrapper">                                <input class="gui-checkbox" type="checkbox" {{bindAttr name="name" id="forceReplaceCheck"}} value="true" />                                <label class="serif" {{bindAttr for="forceReplaceCheck"}}>Force this replacement?</label>                            </div>                            <div class="clear"></div>                            <div class="disabler"></div>                        {{else}}                            <div class="no-joining-nodes gui-text-flat serif italic">                                No new nodes are currently staged to join.                            </div>                            <div class="disabler show slide-up"></div>                        {{/if}}                    </div>                    <div class="clear"></div>                </div>                <div class="clear"></div>                <span class="gui-text-flat serif italic stage-instructions">Click "STAGE" when you are ready to stage this action.</span>                <a class="stage-button gui-point-button-right gui-text-bold right" {{action stageChange target="view"}}>                    <span class="gui-button-msg">STAGE</span>                </a>                <div class="clear"></div>            </div>            <div class="clear"></div>        </div><!-- .actions-box -->        <div class="clear"></div>    </div><!-- .node -->{{/with}}');
Ember.TEMPLATES['current_nodes_item'] = Ember.Handlebars.compile('{{#with view}}    <div class="node">        <div class="item1">            <div {{bindAttr class="stopRadioClasses"}}>                <input class="gui-radio" type="radio" value="leave" {{bindAttr name="name" id="stopRadio"}} />                <div {{bindAttr class="stopDisablerClasses"}}></div>            </div>        </div>        <div class="item2">            <div {{bindAttr class="downRadioClasses"}}>                <input class="gui-radio" type="radio" value="leave" {{bindAttr name="name" id="downRadio"}} />                <div {{bindAttr class="downDisablerClasses"}}></div>            </div>        </div>        <div class="item3 name-box gui-text">            <div {{bindAttr class="indicatorLights"}}>            </div><div class="gui-text field-container inline-block">                <div class="name gui-field">{{name}}</div>            </div>        </div>        <div class="item4 gui-text ring-pct-box">            <div {{bindAttr class="coloredArrows"}}></div>            <div class="left gui-text pct-box">                <span class="ring-pct">{{ringPctReadable}}%</span>            </div>            <div class="clear"></div>        </div>        <div class="item5 gui-text memory-box">            {{#if reachable}}                <div class="membar-bg">                    <div class="mem-colors">                        <div class="erlang-mem mem-color" {{bindAttr style="memErlangStyle"}} {{bindAttr name="memErlangCeil"}}></div>                        <div class="non-erlang-mem mem-color" {{bindAttr style="memNonErlangStyle"}} {{bindAttr name="memNonErlang"}}></div>                        <div class="unknown-mem" {{bindAttr style="memFreeStyle"}} {{bindAttr name="memFreeReadable"}}></div>                    </div>                    <div class="membar-fg"></div>                </div>                <span class="used-memory">{{memUsedReadable}}%</span>            {{else}}                <div class="membar-bg">                    <div class="mem-colors">                        <div class="unknown-mem" style="width: 100%"></div>                    </div>                    <div class="membar-fg"></div>                </div>                <span class="used-memory"></span>            {{/if}}        </div>        <div class="clear"></div>    </div><!-- .node -->{{/with}}');
Ember.TEMPLATES['staged_cluster_item'] = Ember.Handlebars.compile('{{#with view}}    <div class="node">        <div class="item1 name-box gui-text">            <div {{bindAttr class="indicatorLights"}}>            </div><div class="gui-text field-container inline-block">                <div class="name gui-field">{{name}}</div>            </div>        </div>        <div class="item2 gui-text ring-pct-box">            <div class="left gui-text pct-box">                <span class="ring-pct">{{ring_pct_readable}}%</span>            </div>            <div class="clear"></div>        </div>        {{#if isAction}}          <div class="item3 action-taken gui-text">              <span class="action-name">{{node_action}}</span>          </div>        {{/if}}        {{#if isReplaced}}            <div class="item4 replacing-box">                <div class="gui-text field-container inline-block">                    <div class="name gui-field">{{replacement}}</div>                </div>            </div>        {{/if}}        <div class="clear"></div>    </div>{{/with}}');
