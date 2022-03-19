

type 
  ValueKind = enum
    vkFloat, vkInt, vkBool, vkNil, vkObj
  Value = object
    case kind*: ValueKind
    of vkBool:
      boolVal*: bool
    of vkFloat:
      floatVal*: BiggestFloat
    of vkInt:
      intVal*: BiggestInt
    else:
      discard
  