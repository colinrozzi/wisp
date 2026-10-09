interface wisp {
    @package: string = "theater:simple"
    exports {
        evaluate: func(source: string) -> string
    }
}
