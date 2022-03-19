import ./pathlib, std/strformat


type
  FileIndex* = distinct int32
  FileEntry* = object
    index: FileIndex
    filePath: AbsoluteFile
  SourcePosition* = object
    line*, column*: uint16
    file*: FileIndex
  FileManager* = ref object
    files: seq[FileEntry]
  

const
  InvalidFileIndex* = FileIndex(-1)
  UnknownSourcePosition* = SourcePosition(line: 0, column: 0, file: InvalidFileIndex)

proc `$`*(index: FileIndex): string {.borrow.}

proc repr*(info: SourcePosition): string =
  fmt"{info.file}:{info.line}:{info.column}"

proc `$`*(info: SourcePosition): string =
  fmt"{info.line}:{info.column}"

proc initSourcePosition*(fileIndex: FileIndex, line, column: int): SourcePosition =
  SourcePosition(line: line.uint16, column: column.uint16, file: fileIndex)

proc newFileManager*(files: varargs[AbsoluteFile]): FileManager =
  result = FileManager(files: @[])
  for f in files:
    result.files.add(FileEntry(index: result.files.len().FileIndex, filePath: f))
