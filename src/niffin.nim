import ./parser


proc main()=    
  echoAst(parse("""
  fun apply[T: fun(): int](t: T): int {
    return t();
  }
  """))

when isMainModule:
  main()
