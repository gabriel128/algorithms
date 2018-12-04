content = File.readlines './QuickSort.txt'
list = content.map(&:to_i)

$total = 0

def partition(a, pivot_index, n)

  p = a[pivot_index]
  i = pivot_index + 1

  for j in (pivot_index+1)..n
    if a[j] < p
      a[j], a[i] = a[i], a[j]
      i += 1
    end
  end
  a[pivot_index], a[i-1] = a[i-1], a[pivot_index]
  i-1
end


def quicksort(a, from = 0, n = nil)
  if n == nil
    n = a.length - 1
  end



  if from >= n
    return a
  end
  # if from < n

  #   # rand_from = from + Random.rand(n-from)
  #   # a[from], a[rand_from] = a[rand_from], a[from]

  #   p = partition(a, from, n)

  #   quicksort(a, 0, p-1)
  #   quicksort(a, p+1, n)
  # end

  # a
  # rand_from = from + Random.rand(n-from)
  rand_from = n
  a[from], a[rand_from] = a[rand_from], a[from]

  p = partition(a, from, n)

  quicksort(a, from, p-1)
  $total += p-1-from
  quicksort(a, p+1, n)
  $total += n-p+1
end

quicksort list

puts $total
