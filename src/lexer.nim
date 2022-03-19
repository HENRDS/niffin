import std/[strformat, lexbase, parseutils, streams, macros], ./fileman



type
  TokenKind* = enum
    tkInvalid = "tkInvalid", tkEof = "<EOF>", tkComment = "tkComment", tkEol = "<EOL>",
    tkLeftParen = "(", tkRightParen = ")", tkLeftBracket = "[", 
    tkRightBracket = "]", tkLeftBrace = "{", tkRightBrace = "}",
    tkPlus = "+", tkMinus = "-", tkStar = "*", tkSlash = "/",
    tkAmpersand = "&", tkEqual = "=", tkEqual2 = "==",
    tkLess = "<", tkGreater = ">", tkLessEqual = "<=",
    tkGreaterEqual = ">=", tkBangEqual = "!=", tkComma = ",",
    tkColon = ":", tkSemicolon = ";", tkDot = ".", tkDot2 = "..", 
    tkDashGreater = "->", tkQuestion = "?",

    tkStringLit = "tkStringLit",
    tkFloatLit = "tkFloatLit",
    tkIntLit = "tkIntLit",
    tkIdentifier = "tkIdentifier",
    tkSymbol = "tkSymbol",
    
    tkAnd = "and", tkAs = "as", tkBreak = "break",
    tkClass = "class", tkContinue = "continue", tkDiscard= "discard", 
    tkElif = "elif", tkElse = "else", tkEnum = "enum", tkFalse = "false", 
    tkFor = "for", tkFun = "fun", tkIf = "if", tkIn = "in", tkLet = "let", tkNot = "not",
    tkNil = "nil", tkOr = "or", tkReturn = "return",
    tkTrue = "true", tkVar = "var", tkWhile = "while"
const 
  Keyworks* = {tkAnd..tkWhile}
  UnaryOperators* = {tkPlus, tkMinus, tkNot}
  DecimalDigits = {'0'..'9'}
  IdentChars* = {'a'..'z', 'A'..'Z', '0'..'9', '\x80'..'\xFF'}
  IdentStartChars* = {'a'..'z', 'A'..'Z', '\x80'..'\xFF'}
  WhitespaceChars = {' ', '\t', '\v', '\f'}


type    
  Token* = ref object
    pos*: SourcePosition
    case kind*: TokenKind
    of tkStringLit, tkIdentifier, tkSymbol:
      strVal*: string
    of tkFloatLit:
      floatVal*: BiggestFloat
    of tkIntLit:
      intVal*: BiggestInt
    of tkComment:
      comment*: string
    else:
      discard
  Lexer* = object of BaseLexer
    currentFile: FileIndex
    
proc `$`*(tk: Token): string = 
  case tk.kind
  of tkStringLit, tkSymbol, tkIdentifier:
    tk.strVal
  of tkFloatLit:
    $tk.floatVal
  of tkIntLit:
    $tk.intVal
  of tkComment:
    tk.comment
  else:
    $tk.kind

proc initLexer*(s: Stream, index: FileIndex): Lexer=
  result = Lexer(currentFile: index)
  result.open(s)

proc repr*(tk: Token): string = 
  case tk.kind
  of tkStringLit, tkIdentifier, tkSymbol:
    fmt"<{tk.kind} '{tk.strVal}' {tk.pos}>"
  of tkFloatLit:
    fmt"<{tk.kind} {tk.floatVal} {tk.pos}>"
  of tkIntLit:
    fmt"<{tk.kind} {tk.intVal} {tk.pos}>"
  of tkComment:
    fmt"<{tk.kind} '{tk.comment}' {tk.pos}>"
  else:
    fmt"<{tk.kind} {tk.pos}>"

proc isKeyword*(tk: Token): bool =
  tk.kind in Keyworks
    

template current(L: Lexer): char =
  L.buf[L.bufpos]

proc here(L: Lexer): SourcePosition =
  initSourcePosition(L.currentFile, L.lineNumber, L.getColNumber(L.bufpos))

proc positionOf(L: Lexer, i: int): SourcePosition =
  initSourcePosition(L.currentFile, L.lineNumber, L.getColNumber(i))

proc getNumber(L: var Lexer): Token = 
  let start = L.bufpos
  var i : BiggestInt = 0
  if L.current() == '0':
    inc(L.bufpos)
    case L.current()
    of 'b', 'B':
      let x = L.buf.parseBin(i, start);
      inc(L.bufpos, x - 1)
      return Token(kind: tkIntLit, intVal: i, pos: L.positionOf(start))
    of 'o', 'O':
      let x = L.buf.parseOct(i, start)
      inc(L.bufpos, x - 1)
      return Token(kind: tkIntLit, intVal: i, pos: L.positionOf(start))
    of 'x', 'X':
      let x = L.buf.parseHex(i, start)
      inc(L.bufpos, x - 1)
      return Token(kind: tkIntLit, intVal: i, pos: L.positionOf(start))
    of DecimalDigits:
      inc(L.bufpos)
      while L.current in DecimalDigits:
        inc(L.bufpos)
    else:
      discard
  else:
    while L.current() in DecimalDigits:
      inc(L.bufpos)
  let intEnd = L.bufpos
  if L.current() == '.':
    inc(L.bufpos)
    let fracPos = L.bufpos
    while L.current() in DecimalDigits:
      inc(L.bufpos)
    if fracPos == L.bufpos:
      # if there were no digits after the dot
      # rewind until before the dot.
      # 1.E7 is not allowed
      dec(L.bufpos)
      discard L.buf.parseBiggestInt(i, start)
      return Token(kind: tkIntLit, pos: L.positionOf(start), intVal: i)

  if L.current() in {'e', 'E'}:
    inc(L.bufpos)
    var 
      expPos = L.bufpos
      hasSignal = false
    if L.current() in {'+', '-'}:
      inc(L.bufpos)
      hasSignal = true

    while L.current() in DecimalDigits:
      inc(L.bufpos)
    if (L.bufpos == expPos) or (hasSignal and L.bufpos == expPos + 1):
      if expPos - 1 == intEnd:
        L.bufpos = expPos - 1
        discard L.buf.parseBiggestInt(i, start)
        return Token(kind: tkIntLit, pos: L.positionOf(start), intVal: i)
      else:
        var f = 0.0
        let x = L.buf.parseBiggestFloat(f, start)
        if x == 0:
          return Token(kind: tkInvalid, pos: L.positionOf(start))
        return Token(kind: tkFloatLit, pos: L.positionOf(start), floatVal: f)    
  
  if L.bufpos == intEnd:
    discard L.buf.parseBiggestInt(i, start)
    return Token(kind: tkIntLit, pos: L.positionOf(start), intVal: i)
  else:
    var f = 0.0
    discard L.buf.parseBiggestFloat(f, start)
    return Token(kind: tkFloatLit, pos: L.positionOf(start), floatVal: f)

    


proc getIdentifier(L: var Lexer): Token =
  let 
    i = L.bufpos
    pos = L.here()
  while L.buf[L.bufpos] in IdentChars:
    inc(L.bufpos)
  let 
    ident = L.buf[i..<L.bufpos]
    tkType = case ident 
              of "and": tkAnd
              of "as": tkAs
              of "break": tkBreak
              of "class": tkClass
              of "continue": tkContinue
              of "discard": tkDiscard
              of "elif": tkElif
              of "else": tkElse
              of "enum": tkEnum
              of "false": tkFalse
              of "for": tkFor
              of "fun": tkFun
              of "if": tkIf
              of "in": tkIn
              of "let": tkLet
              of "not": tkNot
              of "nil": tkNil
              of "or": tkOr
              of "return": tkReturn
              of "true": tkTrue
              of "var": tkVar
              of "while": tkWhile
              else: tkIdentifier
  
  return case tkType 
         of tkIdentifier: Token(kind: tkIdentifier, strVal: ident, pos: pos)
         else: Token(kind: tkType, pos: pos)

proc getStrLiteral(L: var Lexer): Token =
  let pos = L.here()
  inc(L.bufpos)
  let i = L.bufpos
  while L.buf[L.bufpos] != '"':
    inc(L.bufpos)
  result = Token(kind: tkStringLit, strVal: L.buf[i..<L.bufpos], pos: pos)
  inc(L.bufpos)

proc getComment(L: var Lexer): Token=
  let pos = L.here()
  inc(L.bufpos)
  let i = L.bufpos
  while L.current notin {'\c', '\n', EndOfFile}:
    inc(L.bufpos)
  Token(kind: tkComment, comment: L.buf[i..<L.bufpos], pos: pos)

proc skipWhitespace(L: var Lexer) =
  while L.current() in WhitespaceChars:
    inc(L.bufpos)

proc getNextToken*(L: var Lexer): Token =
  L.skipWhitespace()
  let 
    c = L.buf[L.bufpos]
    pos = L.here()

  case c 
  of '(':
    result = Token(kind: tkLeftParen, pos: pos)
    inc(L.bufpos)
  of ')':
    result = Token(kind: tkRightParen, pos: pos)
    inc(L.bufpos)
  of '[':
    result = Token(kind: tkLeftBracket, pos: pos)
    inc(L.bufpos)
  of ']':
    result = Token(kind: tkRightBracket, pos: pos)
    inc(L.bufpos)
  of '{':
    result = Token(kind: tkLeftBrace, pos: pos)
    inc(L.bufpos)
  of '}':
    result = Token(kind: tkRightBrace, pos: pos)
    inc(L.bufpos)
  of '+':
    result = Token(kind: tkPlus, pos: pos)
    inc(L.bufpos)
  of '-':
    inc(L.bufpos)
    if L.buf[L.bufpos] == '>':
      result = Token(kind: tkDashGreater, pos: pos)
      inc(L.bufpos)
    else:
      result = Token(kind: tkMinus, pos: pos)
  of '*':
    result = Token(kind: tkStar, pos: pos)
    inc(L.bufpos)
  of '/':
    result = Token(kind: tkSlash, pos: pos)
    inc(L.bufpos)
  of '&':
    result = Token(kind: tkAmpersand, pos: pos)
    inc(L.bufpos)
  of '=':
    inc(L.bufpos)
    if L.buf[L.bufpos] == '=':
      result = Token(kind: tkEqual2, pos: pos)
      inc(L.bufpos)
    else:
      result = Token(kind: tkEqual, pos: pos)
  of '<':
    inc(L.bufpos)
    if L.buf[L.bufpos] == '=':
      result = Token(kind: tkLessEqual, pos: pos)
      inc(L.bufpos)
    else:
      result = Token(kind: tkLess, pos: pos)
  of '>':
    inc(L.bufpos)
    if L.buf[L.bufpos] == '=':
      result = Token(kind: tkGreaterEqual, pos: pos)
      inc(L.bufpos)
    else:
      result = Token(kind: tkGreater, pos: pos)
  of '!':
    inc(L.bufpos)
    if L.buf[L.bufpos] == '=':
      result = Token(kind: tkBangEqual, pos: pos)
      inc(L.bufpos)
    else:
      result = Token(kind: tkInvalid, pos: pos)
  of '.':
    inc(L.bufpos)
    if L.current() == '.':
      result = Token(kind: tkDot2, pos: pos)
      inc(L.bufpos)
    else:
      result = Token(kind: tkDot, pos: pos)
  of ';':
    result = Token(kind: tkSemicolon, pos: pos)
    inc(L.bufpos)
  of '?':
    result = Token(kind: tkQuestion, pos: pos)
    inc(L.bufpos)
  of ',':
    result = Token(kind: tkComma, pos: pos)
    inc(L.bufpos)
  of ':':
    result = Token(kind: tkColon, pos: pos)
    inc(L.bufpos)
  of '"':
    result = L.getStrLiteral()
  of '\c':
    let i = L.handleCR(L.bufpos)
    result = Token(kind: tkEol, pos: pos)
    L.bufpos = i
  of '\n':
    let i = L.handleLF(L.bufpos)
    result = Token(kind: tkEol, pos: pos)
    L.bufpos = i
  of '#':
    result = L.getComment()
  of DecimalDigits:
    result = L.getNumber()
  of IdentStartChars:
    result = L.getIdentifier()
  of EndOfFile:
    result = Token(kind: tkEof, pos: pos)
  else:
    result = Token(kind: tkInvalid, pos: pos)
    inc(L.bufpos)


iterator tokenize*(code: string): Token =
  var lexer = initLexer(newStringStream(code), InvalidFileIndex)
  while true:
    let tk = lexer.getNextToken()
    yield tk
    if tk.kind == tkEof:
      break