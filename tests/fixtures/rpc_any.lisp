; An actor that calls a dynamic-value host import and returns its result.
(import theater:simple/rpc describe ((actor-id string)) any)
(export (fn probe ((id string)) any (describe id)))
