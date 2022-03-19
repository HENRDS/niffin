import std/[os, pathnorm]

type
  AbsoluteFile* = distinct string
  AbsoluteDirectory* = distinct string
  AbsolutePath* = AbsoluteDirectory | AbsoluteFile
  RelativeFile* = distinct string
  RelativeDirectory* = distinct string
  RelativePath* = RelativeFile | RelativeDirectory
  AnyPath* = AbsolutePath | RelativePath

proc isEmpty*(x: AnyPath): bool {.inline.} = x.string.len == 0

proc copyFile*(source, dest: AbsoluteFile) =
  os.copyFile(source.string, dest.string)

proc removeFile*(x: AbsoluteFile) {.borrow.}

proc splitFile*(x: AbsoluteFile): tuple[dir: AbsoluteDirectory, name, ext: string] =
  let (a, b, c) = splitFile(x.string)
  result = (dir: AbsoluteDirectory(a), name: b, ext: c)

proc extractFilename*(x: AbsoluteFile): string {.borrow.}

proc fileExists*(x: AbsoluteFile): bool {.borrow.}
proc dirExists*(x: AbsoluteDirectory): bool {.borrow.}

proc quoteShell*(x: AbsoluteFile): string {.borrow.}
proc quoteShell*(x: AbsoluteDirectory): string {.borrow.}

proc cmpPaths*(x, y: AbsoluteDirectory): int {.borrow.}

proc createDir*(x: AbsoluteDirectory) {.borrow.}

proc toAbsoluteDirectory*(path: string): AbsoluteDirectory =
  result = if path.isAbsolute: AbsoluteDirectory(path)
           else: AbsoluteDirectory(getCurrentDir() / path)

proc `$`*(x: AnyPath): string = x.string

proc eqImpl(x, y: string): bool {.inline.} =
    result = cmpPaths(x, y) == 0

proc `==`*[T: AnyPath](x, y: T): bool = eqImpl(x.string, y.string)

template postProcessBase(base: AbsoluteDirectory): untyped =
  when false:
    doAssert isAbsolute(base.string), base.string
    base
  else:
    if base.isEmpty: getCurrentDir().AbsoluteDirectory else: base

proc `/`*(base: AbsoluteDirectory; f: RelativeFile): AbsoluteFile =
  let base = postProcessBase(base)
  assert(not isAbsolute(f.string), f.string)
  result = AbsoluteFile newStringOfCap(base.string.len + f.string.len)
  var state = 0
  addNormalizePath(base.string, result.string, state)
  addNormalizePath(f.string, result.string, state)

proc `/`*(base: AbsoluteDirectory; f: RelativeDirectory): AbsoluteDirectory =
  let base = postProcessBase(base)
  assert(not isAbsolute(f.string))
  result = AbsoluteDirectory newStringOfCap(base.string.len + f.string.len)
  var state = 0
  addNormalizePath(base.string, result.string, state)
  addNormalizePath(f.string, result.string, state)

proc relativeTo*(fullPath: AbsoluteFile, baseFilename: AbsoluteDirectory; sep = DirSep): RelativeFile =
  result = RelativeFile(relativePath(fullPath.string, baseFilename.string, sep))

proc toAbsolute*(file: string; base: AbsoluteDirectory): AbsoluteFile =
  if isAbsolute(file): result = AbsoluteFile(file)
  else: result = base / RelativeFile file

proc changeFileExt*(x: AbsoluteFile; ext: string): AbsoluteFile {.borrow.}
proc changeFileExt*(x: RelativeFile; ext: string): RelativeFile {.borrow.}

proc addFileExt*(x: AbsoluteFile; ext: string): AbsoluteFile {.borrow.}
proc addFileExt*(x: RelativeFile; ext: string): RelativeFile {.borrow.}

proc writeFile*(x: AbsoluteFile; content: string) {.borrow.}

