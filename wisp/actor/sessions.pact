interface wisp-sessions {
    // One interpreter actor in the runtime (a REPL session), however it was spawned.
    record session-info {
        id: string,      // short id, as `theater-repl list` prints it
        name: string,
        actor: string,   // full actor id: the address msg-send takes
        age-secs: u64,   // 0 if the daemon did not spawn it
    }

    exports {
        list-sessions: func() -> list<session-info>
    }
}
