interface wisp-source {
    exports {
        resolve-path: func(base: string, path: string) -> result<string, string>
        read-source: func(path: string) -> result<string, string>
    }
}
