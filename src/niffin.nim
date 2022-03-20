import ./parser, ./lexer

proc main()=   
  const input = 
    """
    fun add(a: i32, b: i32): i32 {
      return a + b;
    }
    discard 1.add(2);
    """
  echo "Tokens:"
  for tk in tokenize(input):
    echo repr(tk)
  echo ""
  echo "AST:"
  echoAst(parse(input))

when isMainModule:
  main()
