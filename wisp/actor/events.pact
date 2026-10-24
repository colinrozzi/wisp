interface wisp-events {
    exports {
        // An interpreter actor recorded trigger firing number `seq` (its newest).
        // The daemon only notes it, to wake readers blocked in `theater-repl wait`.
        event-recorded: func(seq: u64)
    }
}
