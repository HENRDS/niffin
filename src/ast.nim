import ./fileman, std/[strformat, hashes]

type 
  NodeKind* = enum
    nkNone = "Invalid", 
    nkEmpty = "Empty", 
    # Valued nodes
    nkStrLit = "String", 
    nkIntLit = "Int", 
    nkFloatLit = "Float", 
    nkIdent = "Ident", 
    nkSymbol = "Symbol",
    # Nodes with children
    nkNilLit = "Nil",
    nkList = "List",
    nkTuple = "Tuple",
    nkInfix = "Infix", 
    nkPrefix = "Prefix", 
    nkIf = "If", 
    nkCall = "Call", 
    nkAssign = "Assign",
    nkWhile = "While",
    nkFor = "For",
    nkBreak = "Break",
    nkContinue = "Continue",
    nkReturn = "Return",
    nkIndex = "Index",
    nkVarDef = "Var",
    nkLetDef = "Let",
    nkConditional = "Conditional",
    nkDiscard = "Discard",
    nkFormalParams = "FormalParams",
    nkParam = "Param",
    nkGenericParams = "GenericParams",
    nkFunType = "FunType",
    nkVarType = "VarType",
    nkFunDef = "FunDef",
    nkProcDef = "ProcDef",
    nkStmtList = "StmtList"
  Node* {.final,acyclic.} = ref object 
    position*: SourcePosition
    case kind*: NodeKind
    of nkStrLit, nkIdent, nkSymbol:
      strVal*: string
    of nkIntLit:
      intVal*: BiggestInt
    of nkFloatLit:
      floatVal*: BiggestFloat
    else:
      children*: seq[Node]

const 
  ValuedNodes = { nkStrLit, nkIdent, nkIntLit, nkFloatLit, nkSymbol}
  

proc hash*(node: Node): Hash =
  return cast[Hash](node)

proc newEmptyNode*(): Node=
  Node(kind: nkEmpty)

proc newNode*(kind: NodeKind, pos: SourcePosition): Node =
  Node(kind: kind, position: pos)

proc newNodeWithChildren*(kind: NodeKind, pos: SourcePosition, children: varargs[Node]): Node =
  result = newNode(kind, pos)
  case kind
  of ValuedNodes:
    raise newException(Exception, fmt"Node kind {kind} cannot have children")
  else:
    result.children = @children

proc newIntNode*(pos: SourcePosition, val: BiggestInt): Node= 
  result = newNode(nkIntLit, pos)
  result.intVal = val

proc newFloatNode*(pos: SourcePosition, val: BiggestFloat): Node= 
  result = newNode(nkFloatLit, pos)
  result.floatVal = val

proc newStrNode*(pos: SourcePosition, val: string): Node= 
  result = newNode(nkStrLit, pos)
  result.strVal = val

proc newSymNode*(pos: SourcePosition, val: string): Node= 
  result = newNode(nkSymbol, pos)
  result.strVal = val
  
proc newIdentNode*(pos: SourcePosition, val: string): Node= 
  result = newNode(nkIdent, pos)
  result.strVal = val  

proc newInvalidNode*(pos: SourcePosition, children: varargs[Node]): Node =
  result = newNode(nkNone, pos)
  result.children = @children


proc `[]`*(node: Node, index: int): Node =
  node.children[index]

proc `[]`*(node: Node, index: BackwardsIndex): Node =
  node.children[index]

proc `[]`*(node: Node, ran: Slice[int]): seq[Node] =
  node.children[ran]
  
proc `[]`*(node: Node, ran: HSlice[int, BackwardsIndex]): seq[Node] =
  node.children[ran]


proc add*(n: Node, x: Node)=
  n.children.add(x)


 