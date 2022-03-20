import ./ast, std/[tables, sequtils]

type
  FunctionKind = enum
    fkNone, fkLambda, fkFunction
  DefinitionState = enum
    dsDeclared, dsDefinedMutable, dsDefinedImmutable
  ResolveResult* = ref object
    locals*: Table[Node, Natural]
  Resolver* = object
    hadError: bool
    functionKind: FunctionKind
    envs: seq[Table[string, DefinitionState]]
    result: ResolveResult
  
proc hadError*(r: Resolver): bool =
  r.hadError

proc beginScope(r: var Resolver)=
  r.envs.add(initTable[string, DefinitionState]())

proc endScope(r: var Resolver)=
  discard r.envs.pop()

proc initResolver(): Resolver =
  Resolver(
    hadError: false,
    functionKind: fkNone,
    envs: newSeq[Table[string, DefinitionState]]()
  )

proc newResolveResult(): ResolveResult =
  ResolveResult(locals: initTable[Node, Natural]())

proc resolveLocal(r: var Resolver, name: string, node: Node)=
  for i in countdown(r.envs.high(), 0, 1):
    let env = r.envs[i]
    if name in env:
      r.result.locals[node] = i
      return

proc declare(r: var Resolver, name: string)=
  if r.envs.len() == 0:
    return
  r.envs[^1][name] = dsDeclared

proc define(r: var Resolver, name: string, state: DefinitionState)=
  if r.envs.len() == 0:
    return
  r.envs[^1][name] = state

proc resolveInternal(r: var Resolver, n: Node)=
  case n.kind
  of nkIdent, nkSymbol:
    r.resolveLocal(n.strVal, n)
  of nkStrLit, nkIntLit, nkFloatLit, nkNilLit, nkEmpty, nkNone:
    return
  of nkVarDef, nkLetDef:
    r.declare(n[0].strVal)
    r.resolveInternal(n[1])
    r.resolveInternal(n[2])
    r.define(
      n[0].strVal, 
      if n.kind == nkLetDef: dsDefinedImmutable 
      else: dsDefinedMutable
    )
  of nkFunDef, nkProcDef:
    let enclosing = r.functionKind
    r.functionKind = fkFunction
    r.define(n[0].strVal, dsDefinedImmutable)
    r.beginScope()
    # Resolve generics
    r.resolveInternal(n[1])
    # Resolve params
    r.resolveInternal(n[2])
    # Resolve return type
    r.resolveInternal(n[3])
    # Resolve body
    r.resolveInternal(n[4])
    r.endScope()
    r.functionKind = enclosing
  of nkParam:
    r.declare(n[0].strVal)
    r.resolveInternal(n[1])
    r.define(
      n[0].strVal, 
      if n[1].kind == nkVarType: dsDefinedMutable
      else: dsDefinedImmutable
    )
  of nkStmtList:
    r.beginScope()
    for s in n.children:
      r.resolveInternal(s)
    r.endScope()
  else:
    for c in n.children:
      r.resolveInternal(c)

proc resolve*(r: var Resolver, n: Node): ResolveResult =
  r.result = newResolveResult()
  r.resolveInternal(n)
  r.result

    