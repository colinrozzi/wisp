interface wisp-sessions {
    // One live REPL session held by the theater-repl daemon.
    record session-info {
        id: string,      // short id, as `theater-repl list` prints it
        name: string,
        actor: string,   // full actor id: the address msg-send takes
        age-secs: u64,
    }

    exports {
        list-sessions: func() -> list<session-info>
    }
}
