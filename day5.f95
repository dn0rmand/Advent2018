module Day5
  use tools

  implicit none
  private :: Part1, Part2
  public :: Day5Solve

  type :: t_chain
    character :: value
    type(t_chain), pointer :: next => null()
    type(t_chain), pointer :: previous => null()
  end type

contains

subroutine LoadInput(first)
  integer :: file
  integer :: i,l
  character(len=60000) :: buffer
  character(:), allocatable :: line
  type(t_chain), pointer :: next
  type(t_chain), pointer :: current => null()
  type(t_chain), pointer, intent(out) :: first

  file = openFile(5)
  read(file, '(A)') buffer
  call closeFile(file)
  ! buffer = 'dabAcCaCBAcCcaDA'
  line = trim(buffer)
  l = len(line)
  do i = 1,l    
    allocate(next)
    next%value = line(i:i)

    if (.not. associated(first)) then
      first => next
      current => next
    else
      next%previous => current
      current%next => next
      current => next
    end if
  end do
  deallocate(line)
end subroutine

logical function shouldCollapse(c1, c2)
  character :: c1, c2

  if (iachar(c1) == iachar(c2)+32 .or. iachar(c1)+32 == iachar(c2)) then
    shouldCollapse = .true.
  else
    shouldCollapse = .false.
  end if
end function

subroutine collapse(first)
  type(t_chain), pointer, intent(inout) :: first
  type(t_chain), pointer :: one, two
  type(t_chain), pointer :: current

  current => first
  do while (associated(current%next))
    if (shouldCollapse(current%value, current%next%value)) then
      one => current
      two => current%next

      if (.not. associated(current%previous)) then
        first => current%next%next
        if (associated(first)) then
          first%previous => null()
        end if
      else
        current => one%previous
        current%next => two%next
        if (associated(current%next)) then
          current%next%previous => current
        end if
      end if

      deallocate(one)
      deallocate(two)
    else
      current => current%next
    end if
  end do
end subroutine

subroutine cleanup(first)
  type(t_chain), pointer, intent(inout) :: first
  type(t_chain), pointer :: old => null()

  do while (associated(first))
    old => first
    first =>  first%next
    deallocate(old)
  end do

  first => null()
end subroutine

subroutine clone(first, destin, letter1, letter2)
  type(t_chain), pointer, intent(in) :: first
  character, intent(in) :: letter1, letter2
  type(t_chain), pointer, intent(out) :: destin
  type(t_chain), pointer :: ptr
  type(t_chain), pointer :: current => null()
  type(t_chain), pointer :: next => null()

  ptr => first
  destin => null()
  do while(associated(ptr))
    if (.not. (ptr%value == letter1 .or. ptr%value == letter2)) then 
      allocate(next)
      next%value = ptr%value
      if (.not. associated(destin)) then
        destin => next
      end if
      if (associated(current)) then
        next%previous => current
        current%next => next
      end if
      current => next
    end if
    ptr => ptr%next
  end do
end subroutine

integer function length(first)
  type(t_chain), pointer :: first
  type(t_chain), pointer :: current
  integer :: size

  size = 0;
  current => first
  do while(associated(current)) 
    size = size+1
    current => current%next
  end do
  length = size
end function


subroutine dump(first)
  type(t_chain), pointer, intent(in) :: first
  type(t_chain), pointer :: current

  current => first
  do while(associated(current)) 
    write(*, fmt='(A)', advance='no') current%value
    current => current%next
  end do
  print *,''
end subroutine

integer function Part1()
  type(t_chain), pointer :: input => null()

  call LoadInput(input)
  call collapse(input)
  Part1 = length(input)
  call cleanup(input)
end function 

integer function Part2()
  type(t_chain), pointer :: input => null()
  type(t_chain), pointer :: cloned => null()
  character :: letter1, letter2
  integer :: i, l, min

  call LoadInput(input)
  call collapse(input)

  min = length(input)
  do i = 1, 26
    letter1 = achar(i+64)
    letter2 = achar(i+64+32)
    call clone(input, cloned, letter1, letter2)
    call collapse(cloned)
    l = length(cloned)
    if (l < min) then 
      min = l 
    end if
    call cleanup(cloned)
  end do

  call cleanup(input)

  Part2 = min
end function Part2

subroutine Day5Solve()
  integer :: answer

  print *, '--- Day 5 ---'

  answer = Part1()
  print *, 'Answer to part 1 is ', answer

  answer = Part2()
  print *, 'Answer to part 2 is ', answer
end subroutine Day5Solve

end module Day5
