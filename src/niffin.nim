import ./parser


proc main()=    
  echoAst(parse("""
  fun `$`[T: fun(): int](t: T): int {
    return t();
  }
  """))

when isMainModule:
  main()
