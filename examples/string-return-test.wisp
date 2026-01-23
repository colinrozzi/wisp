; Test returning a string from an exported function

(fn greet () string
  "Hello from wisp!")

(export greet)
