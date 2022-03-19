
type
  ArrayListObj[T] = object
    count, cap: Natural
    data: ptr UncheckedArray[T]
  ArrayList*[T] = ptr ArrayListObj[T]


const
  CapacityGrowthFactor = 2
  DefaultArrayListCapacity = 4


proc createArrayList*[T](initialCap: Positive = DefaultArrayListCapacity): ArrayList[T] =
  result = create(ArrayListObj[T])
  result.data = cast[ptr UncheckedArray[T]](alloc0(sizeof(T) * initialCap))
  result.cap = initialCap

proc resizeArray[T](a: ptr UncheckedArray[T], newSize: Natural): ptr UncheckedArray[T]=
  cast[ptr UncheckedArray[T]](realloc(a, sizeof(T) * newSize))

proc `[]`*[T](v: ArrayList[T], i: Natural): T =
  v.data[i]

proc add*[T](v: ArrayList[T], x: T)=
  if v.cap < v.count + 1:
    v.cap = v.cap * CapacityGrowthFactor
    v.data = resizeArray(v.data, v.cap)
  v.data[v.count] = x
  inc(v.count)

proc len*[T](v: ArrayList[T]): Natural =
  v.count

proc freeArrayList*[T](v: ArrayList[T])=
  dealloc(v.data)
  dealloc(v)

iterator items*[T](v: ArrayList[T]): T =
  for i in 0..<v.count:
    yield v.data[i]

iterator pairs*[T](v: ArrayList[T]): (Natural, T) =
  for i in 0..<v.count:
    yield (i, v.data[i])

proc high*[T](v: ArrayList[T]): Natural = 
  if v.count == 0:
    raise newException(IndexDefect, "ArrayList is empty")
  v.count - 1

proc pop*[T](v: ArrayList[T]): T =
  if v.count == 0:
    raise newException(IndexDefect, "Popping from empty ArrayList");
  result = v.data[v.count - 1]
  dec(v.count)

proc reset*[T](v: ArrayList[T], newCap: Natural = 0)=
  v.count = 0
  if newCap > 0:
    v.cap = newCap
    v.data = resizeArray(v.data, newCap)
