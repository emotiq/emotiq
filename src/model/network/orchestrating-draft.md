# Node orchestration API requirements

Methods needed:

* `version` - returns version and commit hash or currently running code
* `status` - returns information about the node:
  * state:
    * HEALTHY
    * SUSPENDED
    * SYNCING
  * blockchain size
  * last block hash
  * number of peers
* `suspend` - stop any blockchain operations
* `resume` - resume blockchain operations
