import ./lexer, std/[streams, terminal, strformat, tables, strutils, sequtils], ./ast, ./fileman

type
  Parser* = object
    prev, cur: Token
    lexer: Lexer
    hadError: bool
  Precedence = enum
    prNone, prTuple, prConditional, prOr, prAnd, prEquality, prComparison, prCat, 
    prRange, prSum, prProduct, prPow, prPrefix, prCall, prAccess, prPrimary
  InfixParselet = proc(p: var Parser, node: Node): Node
  PrefixParselet = proc(p: var Parser): Node
  ParserRule = ref object
    prefix: PrefixParselet
    infix: InfixParselet
    precedence: Precedence

const
  TupleSuccPrecedence = succ(prTuple) 
  WellKnownIdents = [
    "true", "false"
  ]

proc errEcho(msg: string)=
  styledEcho(fgRed, msg, resetStyle)

proc errAt(msg: string, pos: SourcePosition)=
  errEcho(fmt"{msg} at {pos}")

proc hadError*(p: Parser): bool =
  p.hadError

proc advance(p: var Parser)=
  p.prev = p.cur
  while true:
    p.cur = p.lexer.getNextToken()
    if p.cur.kind == tkInvalid:
      errAt("Invalid token", p.cur.pos)
    if p.cur.kind notin {tkInvalid, tkComment, tkEol}: 
      break

proc errAtCurrent(p: var Parser, msg: string)=
  errAt(msg, p.cur.pos)
  p.hadError = true

proc errAtPrevious(p: var Parser, msg: string)=
  errAt(msg, p.prev.pos)
  p.hadError = true

proc match(p: var Parser, tk: TokenKind): bool =
  result = false
  if p.cur.kind == tk:
    p.advance()
    result = true

proc matchAny(p: var Parser, tks: varargs[TokenKind]): bool =
  result = false
  if p.cur.kind in tks:
    p.advance()
    result = true

proc consume(p: var Parser, tk: TokenKind, msg: string): bool =
  result = true
  if not p.match(tk):
    p.errAtCurrent(msg)
    result = false

proc initParser*(s: Stream, fileIndex: FileIndex): Parser =
  result = Parser(lexer: initLexer(s, fileIndex), hadError: false)
  result.advance()

proc parseExpression(p: var Parser): Node
proc parsePrecedence(p: var Parser, prec: Precedence): Node
proc getRule(tk: TokenKind): ParserRule
proc echoAst*(n: Node, indent: Natural = 0) # for debug

proc literal(p: var Parser): Node =
  case p.prev.kind
  of tkFloatLit:
    newFloatNode(p.prev.pos, p.prev.floatVal)
  of tkIntLit:
    newIntNode(p.prev.pos, p.prev.intVal)
  of tkStringLit:
    newStrNode(p.prev.pos, p.prev.strVal)
  of tkNil:
    newNode(nkNilLit, p.prev.pos)
  of tkFalse:
    newIdentNode(p.prev.pos, "false")
  of tkTrue:
    newIdentNode(p.prev.pos, "true")
  of tkIdentifier:
    newIdentNode(p.prev.pos, p.prev.strVal)
  of tkSymbol:
    newSymNode(p.prev.pos, p.prev.strVal)
  else:
    p.errAtCurrent("Invalid literal")
    newInvalidNode(p.prev.pos)
  
proc prefix(p: var Parser): Node =
  let 
    oper = p.prev
    rhs = p.parsePrecedence(prPrefix)
  newNodeWithChildren(
    nkPrefix, 
    oper.pos, 
    newSymNode(oper.pos, $oper.kind),
    rhs
  )

proc infix(p: var Parser, lhs: Node): Node =
  let
    oper = p.prev
    rule = getRule(oper.kind)
    rhs = p.parsePrecedence(succ(rule.precedence))
  newNodeWithChildren(
    nkInfix,
    oper.pos,
    newSymNode(oper.pos, $oper.kind),
    lhs,
    rhs
  )

proc call(p: var Parser, lhs: Node ): Node =
  let paren = p.prev
  result = newNodeWithChildren(nkCall, paren.pos)
  if lhs.kind == nkInfix and lhs[0].strVal == "." and lhs[2].kind in {nkSymbol, nkIdent}:
    result.add(lhs[2])
    result.add(lhs[1])

  if not p.match(tkRightParen):
    while true:
      result.add(p.parsePrecedence(TupleSuccPrecedence))
      if not p.match(tkComma):
        break
    discard p.consume(tkRightParen, "Expected ')' after call")

proc index(p: var Parser, callee: Node ): Node =
  let bracket = p.prev
  result = newNodeWithChildren(nkIndex, bracket.pos, callee)
  if not p.match(tkRightBracket):
    while true:
      result.add(p.parsePrecedence(TupleSuccPrecedence))
      if not p.match(tkComma):
        break
    discard p.consume(tkRightBracket, "Expected ']' after item access")

proc grouping(p: var Parser): Node =
  result = p.parseExpression()
  discard p.consume(tkRightParen, "Expected ')' after expression")

proc comma(p: var Parser, lhs: Node): Node=
  result = newNodeWithChildren(
    nkTuple, 
    p.prev.pos, 
    lhs
  )
  while true:
    result.add(p.parsePrecedence(TupleSuccPrecedence))
    if not p.match(tkComma):
      break

proc list(p: var Parser): Node =
  result = newNodeWithChildren(nkList, p.prev.pos)
  if not p.match(tkRightBracket):
    while true:
      result.add(p.parsePrecedence(TupleSuccPrecedence))
      if not p.match(tkComma):
        break
    discard p.consume(tkRightBracket, "Expected ']' after list")

proc conditional(p: var Parser, lhs: Node): Node=
  let 
    oper = p.prev
    then = p.parsePrecedence(prConditional)
  if p.consume(tkColon, "Expected ':' after conditional"):
    let els = p.parsePrecedence(prConditional)
    newNodeWithChildren(
      nkConditional,
      oper.pos,
      lhs,
      then,
      els
    )
  else:
    newInvalidNode(
      oper.pos,
      lhs,
      then
    )

proc formalParam(p: var Parser, allowModifier: bool = true): Node =
  if not p.consume(tkIdentifier, "Expected identifier"):
    return newInvalidNode(p.cur.pos)
  let 
    nameTk = p.prev
    name = newIdentNode(nameTk.pos, nameTk.strVal) 
    typ = 
      if p.match(tkColon):
        if allowModifier and p.match(tkVar):
          newNodeWithChildren(
            nkVarType,
            p.prev.pos,
            p.parsePrecedence(TupleSuccPrecedence)
          )
        else:
          p.parsePrecedence(TupleSuccPrecedence)
      else:
        newEmptyNode()
  result = newNodeWithChildren(
    nkParam,
    nameTk.pos,
    name, 
    typ
  )



proc lambdaExpr(p: var Parser): Node =
  let kwd = p.prev 

  let generics = newNodeWithChildren(nkGenericParams, p.cur.pos)
  if p.match(tkLeftBracket):
    while true:
      generics.add(p.formalParam(false))
      if likely(p.match(tkComma)):
        continue
      break
    discard p.consume(tkRightBracket, "Expected ']' after generic parameters")
    

  if not p.consume(tkLeftParen, "Expected '(' in lambda expression"):
    return newInvalidNode(kwd.pos)
  let params = newNodeWithChildren(nkFormalParams, kwd.pos)
  if p.cur.kind != tkRightParen:
    while true:
      params.add(p.formalParam())
      if likely(p.match(tkComma)):
        continue
      break
  if not p.consume(tkRightParen, "Expected ')' after lambda expression parameters"):
    return newInvalidNode(p.cur.pos)
  let retType = 
    if p.match(tkColon):
      p.parseExpression()
    else:
      newEmptyNode()
  
  if not p.match(tkDashGreater):
    return newNodeWithChildren(
      nkFunType,
      kwd.pos,
      generics,
      params,
      retType
    )
  let 
    arrow = p.prev
    body = p.parseExpression()
  result = newNodeWithChildren(
    if kwd.kind == tkProc: nkProcDef
    else: nkFunDef,
    kwd.pos,
    generics,
    params,
    retType,
    newNodeWithChildren(
      nkReturn,
      arrow.pos,
      body
    )
  ) 



let rules = {
  tkLeftParen: ParserRule(prefix: grouping, infix: call, precedence: prCall),
  tkRightParen: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  tkLeftBracket: ParserRule(prefix: list, infix: index, precedence: prCall),
  tkRightBracket: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  tkSymbol: ParserRule(prefix: literal, infix: nil, precedence: prNone),
  tkIdentifier: ParserRule(prefix: literal, infix: nil, precedence: prNone),
  tkIntLit: ParserRule(prefix: literal, infix: nil, precedence: prNone),
  tkFloatLit: ParserRule(prefix: literal, infix: nil, precedence: prNone),
  tkStringLit: ParserRule(prefix: literal, infix: nil, precedence: prNone),
  tkNil: ParserRule(prefix: literal, infix: nil, precedence: prNone),
  tkFalse: ParserRule(prefix: literal, infix: nil, precedence: prNone),
  tkTrue: ParserRule(prefix: literal, infix: nil, precedence: prNone),
  tkColon: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  tkComma: ParserRule(prefix: nil, infix: comma, precedence: prTuple),
  tkQuestion: ParserRule(prefix: nil, infix: conditional, precedence: prConditional),
  tkOr: ParserRule(prefix: literal, infix: nil, precedence: prOr),
  tkAnd: ParserRule(prefix: literal, infix: nil, precedence: prAnd),
  tkEqual2: ParserRule(prefix: nil, infix: infix, precedence: prEquality),
  tkBangEqual: ParserRule(prefix: nil, infix: infix, precedence: prEquality),
  tkLess: ParserRule(prefix: nil, infix: infix, precedence: prComparison),
  tkLessEqual: ParserRule(prefix: nil, infix: infix, precedence: prComparison),
  tkGreater: ParserRule(prefix: nil, infix: infix, precedence: prComparison),
  tkGreaterEqual: ParserRule(prefix: nil, infix: infix, precedence: prComparison),
  tkAmpersand: ParserRule(prefix: nil, infix: infix, precedence: prCat),
  tkDot2: ParserRule(prefix: nil, infix: infix, precedence: prRange),
  tkPlus: ParserRule(prefix: prefix, infix: infix, precedence: prSum),
  tkMinus: ParserRule(prefix: prefix, infix: infix, precedence: prSum),
  tkStar: ParserRule(prefix: nil, infix: infix, precedence: prProduct),
  tkSlash: ParserRule(prefix: nil, infix: infix, precedence: prProduct),
  tkDot: ParserRule(prefix: nil, infix: infix, precedence: prAccess),
  tkSemicolon: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  tkEqual: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  tkIn: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  tkLeftBrace: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  tkRightBrace: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  tkFun: ParserRule(prefix: lambdaExpr, infix: nil, precedence: prNone),
  tkDashGreater: ParserRule(prefix: nil, infix: nil, precedence: prNone),
  
  
  tkEof: ParserRule(prefix: nil, infix: nil, precedence: prNone),
}.toTable()


proc parseExpression(p: var Parser): Node = 
  p.parsePrecedence(prTuple)

proc parsePrecedence(p: var Parser, prec: Precedence): Node = 
  p.advance()
  let fn = getRule(p.prev.kind).prefix
  if fn.isNil():
    p.errAtPrevious("Expected expression")
    return newInvalidNode(p.prev.pos)
  var lhs = fn(p)

  while prec <= getRule(p.cur.kind).precedence:
    p.advance()
    let fn = getRule(p.prev.kind).infix
    lhs = fn(p, lhs)
  lhs


proc getRule(tk: TokenKind): ParserRule = 
  rules[tk]

proc varDef(p: var Parser): Node =
  let kwd = p.cur
  p.advance()
  if not p.matchAny(tkIdentifier, tkSymbol):
    p.errAtCurrent("Expected identifier or symbol in var declaration")
    return newInvalidNode(kwd.pos)
  let 
    name = p.prev
    typ: Node = 
      if p.match(tkColon):
        p.parseExpression()
      else:
        newEmptyNode()
    init: Node =
      if p.match(tkEqual):
        p.parseExpression()
      else:
        newEmptyNode()
  if p.consume(tkSemicolon, "Expected ';' after declaration"):
    newNodeWithChildren(
      nkVarDef,
      kwd.pos,
      if name.kind == tkIdentifier:
        newIdentNode(name.pos, name.strVal)
      else:
        newSymNode(name.pos, name.strVal),
      typ,
      init
    )
  else:
    newInvalidNode(kwd.pos)
    
proc letDef(p: var Parser): Node =
  let kwd = p.cur
  p.advance()
  if not p.matchAny(tkIdentifier, tkSymbol):
    p.errAtCurrent("Expected identifier or symbol in let declaration")
    return newInvalidNode(kwd.pos)
  let 
    name = p.prev
    typ: Node = 
      if p.match(tkColon):
        p.parseExpression()
      else:
        newEmptyNode()
  if not p.consume(tkEqual, "Let bindings must be assigned at their declaration"):
    return newInvalidNode(name.pos)

  let init: Node = p.parseExpression()

  if p.consume(tkSemicolon, "Expected ';' after declaration"):
    newNodeWithChildren(
      nkLetDef,
      kwd.pos,
      if name.kind == tkIdentifier:
        newIdentNode(name.pos, name.strVal)
      else:
        newSymNode(name.pos, name.strVal),
      typ,
      init
    )
  else:
    newInvalidNode(kwd.pos)

proc parseDecl(p: var Parser): Node

proc stmtBlock(p: var Parser): Node =
  if not p.consume(tkLeftBrace, "Expected '{'"):
    return newInvalidNode(p.cur.pos)
  result = newNodeWithChildren(nkStmtList, p.prev.pos)
  while p.cur.kind notin {tkRightBrace, tkEof}:
    result.add(p.parseDecl())
  if not p.consume(tkRightBrace, "Expected '}' after block"):
    result = newInvalidNode(result.position)


proc ifStmt(p: var Parser): Node = 
  let kwd = p.cur
  p.advance()
  let 
    condition = p.parseExpression()
    then = p.stmtBlock()
  result = newNodeWithChildren(nkIf, kwd.pos, condition, then)
  var lastIf = result
  while p.match(tkElif):
    let 
      kwd = p.prev
      condition = p.parseExpression()
      then = p.stmtBlock()
    var currentIf = newNodeWithChildren(nkIf, kwd.pos, condition, then)
    lastIf.add(currentIf)
    lastIf = currentIf
  if p.match(tkElse):
    lastIf.add(p.stmtBlock())

proc whileStmt(p: var Parser): Node =
  p.advance()
  let 
    kwd = p.prev
    condition = p.parseExpression()
    body = p.stmtBlock()
  return newNodeWithChildren(
    nkWhile,
    kwd.pos,
    condition,
    body
  )

proc isValidForPattern(node: Node): bool =
  case node.kind
  of nkTuple:
    node.children.map(isValidForPattern).foldl(a and b)
  of nkIdent:
    true
  else: 
    false

proc funOrProcDef(p: var Parser): Node = 
  p.advance()
  let kwd = p.prev
  if not p.matchAny(tkIdentifier, tkSymbol):
    return p.lambdaExpr();
  let name = p.prev
  let generics = newNodeWithChildren(nkGenericParams, p.cur.pos)
  if p.match(tkLeftBracket):
    while true:
      generics.add(p.formalParam(false))
      if likely(p.match(tkComma)):
        continue
      break
    discard p.consume(tkRightBracket, "Expected ']' after generic parameters")
    

  if not p.consume(tkLeftParen, "Expected '(' in fun or proc declaration"):
    return newInvalidNode(kwd.pos)
  let params = newNodeWithChildren(nkFormalParams, kwd.pos)
  if p.cur.kind != tkRightParen:
    while true:
      params.add(p.formalParam())
      if likely(p.match(tkComma)):
        continue
      break
  if not p.consume(tkRightParen, "Expected ')' after fun or proc parameters"):
    return newInvalidNode(p.cur.pos)
  let 
    retType = 
      if p.match(tkColon):
        p.parseExpression()
      else:
        newEmptyNode()
    body = p.stmtBlock()
  result = newNodeWithChildren(
    if kwd.kind == tkProc: nkProcDef
    else: nkFunDef,
    kwd.pos,
    if name.kind == tkIdentifier:
      newIdentNode(name.pos, name.strVal)
    else:
      newSymNode(name.pos, name.strVal),
    generics,
    params,
    retType,
    body
  );
  

proc forStmt(p: var Parser): Node =
  p.advance()
  let 
    kwd = p.prev
    pattern = p.parseExpression()
    
  if not isValidForPattern(pattern):
    errAt("Invalid for pattern", pattern.position)
    return newInvalidNode(pattern.position)

  if not p.consume(tkIn, "Expected 'in' at for statement"):
    return newInvalidNode(kwd.pos)
  let 
    iter = p.parseExpression()
    body = p.stmtBlock()
  newNodeWithChildren(
    nkFor,
    kwd.pos,
    pattern,
    iter,
    body
  )


proc parseStmt(p: var Parser): Node =
  let kwd = p.cur
  case p.cur.kind
  of tkDiscard:
    p.advance()
    if not p.match(tkSemicolon):
      result = newNodeWithChildren(
        nkDiscard,
        kwd.pos,
        p.parseExpression()
      ) 
      discard p.consume(tkSemicolon, "Expected ';' after statement")
  of tkBreak:
    p.advance()
    result = newNode(nkBreak, kwd.pos)
    discard p.consume(tkSemicolon, "Expected ';' after statement")
  of tkContinue:
    p.advance()
    result = newNode(nkContinue, kwd.pos)
    discard p.consume(tkSemicolon, "Expected ';' after statement")
  of tkReturn:
    p.advance()
    let retExpr = 
      if p.cur.kind != tkSemicolon:
        p.parseExpression()
      else:
        newEmptyNode()
    result = newNodeWithChildren(nkReturn, kwd.pos, retExpr)
    discard p.consume(tkSemicolon, "Expected ';' after statement")
  of tkIf:
    result = p.ifStmt()
  of tkWhile:
    result = p.whileStmt()
  of tkFor:
    result = p.forStmt()
  of tkLeftBrace:
    result = p.stmtBlock()
  else:
    result = p.parseExpression()
    if p.match(tkEqual):
      let 
        equal = p.prev
        rhs = p.parseExpression()
      result = newNodeWithChildren(
        nkAssign,
        equal.pos,
        result,
        rhs
      )
    discard p.consume(tkSemicolon, "Expected ';' after expression")

proc parseDecl(p: var Parser): Node =
  
  case p.cur.kind
  of tkVar:
    result = p.varDef()
  of tkLet:
    result = p.letDef()
  of tkFun, tkProc:
    result = p.funOrProcDef()
  else: 
    result = p.parseStmt()

proc parseStmtList(p: var Parser): Node = 
  result = newNodeWithChildren(nkStmtList, p.cur.pos)
  while p.cur.kind != tkEof:
    result.children.add(p.parseDecl())

proc parse*(p: var Parser): Node =
  p.parseStmtList()

proc parse*(c: string): Node =
  let stream = newStringStream(c)
  defer:
    stream.close()

  var p = initParser(stream, InvalidFileIndex) 
  p.parse()
 
proc echoIndented(text: string, indent: Natural)=
  echo "  ".repeat(indent) & text 

proc echoAst*(n: Node, indent: Natural = 0)=
  case n.kind
  of nkIdent:
    echoIndented(fmt"Ident {n.strVal}", indent)
  of nkStrLit:
    echoIndented(fmt"String '{n.strVal}'", indent)
  of nkSymbol:
    echoIndented(fmt"Symbol {n.strVal}", indent)
  of nkFloatLit:
    echoIndented(fmt"Float {n.floatVal}", indent)
  of nkIntLit:
    echoIndented(fmt"Int {n.intVal}", indent)
  of nkNilLit:
    echoIndented(fmt"Nil", indent)
  else:
    echoIndented($n.kind, indent)
    for c in n.children:
      echoAst(c, indent + 1)
  
  
