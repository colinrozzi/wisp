; Test Theater log import

; Import log from theater runtime
(import theater:simple/runtime log ((msg string)) s32)

; Function that logs and returns a value
(fn hello () s32
  (let (_ (log "Hello from wisp!"))
    42))

(export hello)
