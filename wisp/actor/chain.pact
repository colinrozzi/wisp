interface wisp-chain {
    // One recorded chain event. Hashes are hex; the payload is not kept, only
    // its size.
    record chain-event {
        hash: string,
        parent: string,      // "" for the actor's first event
        event-type: string,
        data-bytes: s32,
    }

    // The newest recorded event and how many have been seen since the watch.
    record chain-head {
        hash: string,        // "" until the first event arrives
        count: s32,
        recording: bool,     // false once the actor has stopped
    }

    // Is each event's parent the event before it, over every event seen?
    record chain-check {
        ok: bool,            // no breaks
        count: s32,          // events seen
        kept: s32,           // events still in the ring
        dropped: s32,        // count - kept: the ring's gap, unchecked by tail
        breaks: s32,         // events whose parent was not the previous hash
        from-root: bool,     // the first event seen was the actor's first
    }

    exports {
        watch: func(actor: string) -> result<_, string>
        unwatch: func(actor: string) -> result<_, string>
        head: func(actor: string) -> result<chain-head, string>
        tail: func(actor: string, n: s32) -> result<list<chain-event>, string>
        verify: func(actor: string) -> result<chain-check, string>
    }
}
