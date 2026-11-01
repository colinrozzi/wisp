interface wisp-crypto {
    // Hashes, signatures and keys are lowercase hex. Signing keys live in the
    // daemon's key directory; no function here returns one.
    exports {
        sha256: func(data: list<u8>) -> string
        key-new: func(name: string) -> result<string, string>
        key-public: func(name: string) -> result<string, string>
        key-list: func() -> result<list<string>, string>
        sign: func(name: string, data: list<u8>) -> result<string, string>
        verify: func(public-key: string, signature: string, data: list<u8>) -> bool
    }
}
