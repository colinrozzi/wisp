interface wisp-crypto {
    // Hashes, signatures and ed25519/x25519 keys are lowercase hex; P-256 public
    // keys are base64url. Private keys live in the daemon's key directory; no
    // function here returns one.
    exports {
        sha256: func(data: list<u8>) -> string
        key-new: func(name: string) -> result<string, string>
        key-public: func(name: string) -> result<string, string>
        key-list: func() -> result<list<string>, string>
        sign: func(name: string, data: list<u8>) -> result<string, string>
        verify: func(public-key: string, signature: string, data: list<u8>) -> bool
        p256-key-new: func(name: string) -> result<string, string>
        p256-public: func(name: string) -> result<string, string>
        vapid-header: func(name: string, audience: string, subject: string) -> result<string, string>
        webpush-encrypt: func(p256dh: string, auth: string, payload: list<u8>) -> result<list<u8>, string>
        x25519-key-new: func(name: string) -> result<string, string>
        x25519-public: func(name: string) -> result<string, string>
        seal: func(recipient: string, plaintext: list<u8>) -> result<list<u8>, string>
        open: func(name: string, sealed: list<u8>) -> result<list<u8>, string>
        scrypt-hash: func(password: list<u8>) -> result<string, string>
        scrypt-check: func(password: list<u8>, line: string) -> bool
    }
}
