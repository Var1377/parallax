def Search(needle, haystack):
  match haystack:
    case List/Nil:
      return 0
    case List/Cons:
      if needle == haystack.head:
        return 1
      else:
        return Search(needle, haystack.tail)
      
def SearchBenchmark():
  return Search(3, [1,2,3,4])

def Merge(left, right):
  match left:
    case List/Nil:
      return right
    case List/Cons:
      match right:
        case List/Nil:
          return left
        case List/Cons:
          if left.head < right.head:
            return List/Cons(left.head, Merge(left.tail, right))
          else:
            return List/Cons(right.head, Merge(left, right.tail))

def Split(list):
  match list:
    case List/Nil:
      return (List/Nil(), List/Nil())
    case List/Cons:
      match list.tail:
        case List/Nil:
          return (list, List/Nil())
        case List/Cons:
          (left, right) = Split(list.tail.tail)
          return (List/Cons(list.head, left), List/Cons(list.tail.head, right))

def MergeSort(list):
  match list:
    case List/Nil:
      return list
    case List/Cons:
      match list.tail:
        case List/Nil:
          return list
        case List/Cons:
          (left, right) = Split(list)
          return Merge(MergeSort(left), MergeSort(right))

def MergeSortBenchmark():
  return MergeSort([4,2,3,1])

def Partition(list, pivot):
  match list:
    case List/Nil:
      return (List/Nil(), List/Nil())
    case List/Cons:
      (left, right) = Partition(list.tail, pivot)
      if list.head < pivot:
        return (List/Cons(list.head, left), right)
      else:
        return (left, List/Cons(list.head, right))

def QuickSort(list):
  match list:
    case List/Nil:
      return list
    case List/Cons:
      match list.tail:
        case List/Nil:
          return list
        case List/Cons:
          pivot = list.head
          (left, right) = Partition(list.tail, pivot)
          l = QuickSort(left)
          r = QuickSort(right)
          return Merge(l, List/Cons(pivot, r))

def QuickSortBenchmark():
  return QuickSort([4,2,3,1])

def DotProduct(v1, v2):
  match v1:
    case List/Nil:
      return 0
    case List/Cons:
      match v2:
        case List/Nil:
          return 0
        case List/Cons:
          return v1.head * v2.head + DotProduct(v1.tail, v2.tail)

def DotProductBenchmark():
  return DotProduct([1,2,3], [4,5,6])


def main():
  s = SearchBenchmark()
  ms = MergeSortBenchmark()
  qs = QuickSortBenchmark()
  dp = DotProductBenchmark()
  return (s, ms, qs, dp)
