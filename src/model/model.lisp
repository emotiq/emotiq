#|

We use the model to have a common format for all of our messages that
may easily be converted to JSON and CLOS objects.  Eventually, we should
be able to roundtrip from JSON to CLOS and back if we wish.

The model synchronously returns lisp objects than can be serialized
via CL-JSON.  A future version of the model will optionally return the
corresponding CLOS class for easier Lisp-side introspection.

Currently used by the http <file:../network/rest/> and websocket
<file:../network/websocket/> services to unify messages from a single
source.

The model only ever returns copies of information, i.e. all references
returned by querying the model result in freshly consed structures
that are guaranteed not to be changed further by the system.

|#
