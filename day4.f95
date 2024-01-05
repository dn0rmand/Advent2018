module Day4

  use tools

  private :: Part1, Part2
  public :: Day4Solve

  type :: t_entry
    integer :: guard
    integer :: month
    integer :: day
    integer :: minute
    logical :: awake
  end type

  type, extends(sortable) :: t_input
    type(t_entry), dimension(1100) :: entries
    logical :: orderGuards
    contains
      procedure :: compare
      procedure :: swap
  end type

contains

integer function compare(this, i1, i2)
  class(t_input), intent(in) :: this
  integer, intent(in) :: i1, i2
  type(t_entry) :: a, b
  integer :: cmpValue

  a = this%entries(i1)
  b = this%entries(i2)

  if (this%orderGuards) then
    cmpValue = a%guard - b%guard
  else
    cmpValue = 0
  end if
  if (cmpValue == 0) then
    cmpValue = a%month - b%month
  end if
  if (cmpValue == 0) then
    cmpValue = a%day - b%day
  end if
  if (cmpValue == 0) then
    cmpValue = a%minute - b%minute
  end if
  compare = cmpValue
end function

subroutine swap(this, i1, i2)
  class(t_input), intent(inout) :: this
  integer, intent(in) :: i1, i2
  type(t_entry) :: t1, t2, t3

  t1 = this%entries(i1)
  t2 = this%entries(i2)

  this%entries(i1) =  t2
  this%entries(i2) =  t1
end subroutine

type(t_input) function LoadInput()
  integer :: file, guard, hour, minute
  character(:), allocatable :: line
  type(t_input) :: input 
  type(t_entry) :: current

  input%size = 0

  file = openFile(4)

  do while(readLine(file, line))
    idx1 =  index(line, ' @')
    idx2 =  index(line, ',')
    idx3 =  index(line, ': ')
    idx4 =  index(line, 'x')

    read(line(7:8) , *) current%month
    read(line(10:11) , *) current%day
    read(line(13:14) , *) hour
    read(line(16:17) , *) minute

    current%minute = minute + 60*hour

    if (line(20:) == 'wakes up') then
      current%awake = .true.
      current%guard = 0
    else if (line(20:) == 'falls asleep') then
      current%awake = .false.
      current%guard = 0
    else
      read(line(27:), *) current%guard
      current%awake = .true.
    end if

    deallocate(line)

    input%size = input%size+1
    input%entries(input%size) = current
  end do

  call closeFile(file)

  input%orderGuards = .false.
  call input%sort()

  ! assign the guards
  do i = 1,input%size
    current = input%entries(i)
    if (.not. current%guard == 0) then
      guard = current%guard
    else
      input%entries(i)%guard = guard
    end if
  end do

  input%orderGuards = .true.
  call input%sort()

  LoadInput = input
end function LoadInput

integer function findBestGuard(input) ! returns the start index of the best guard 
  type(t_input) :: input
  type(t_entry) :: row, row2
  integer :: i, bestGuard, startIndex, maxMinutes, minutes, guard, index

  bestGuard = 0;
  startIndex = 0
  maxMinutes = 0
  minutes = 0
  guard = 0
  index = 0
  do i = 1,input%size
    row = input%entries(i)
    if (.not. row%guard == guard) then
      if (minutes > maxMinutes) then
        bestGuard = guard
        maxMinutes = minutes
        startIndex = index
      end if
      guard = row%guard
      index = i
      minutes = 0
    else if (.not. row%awake) then
      row2 = input%entries(i+1)
      minutes = minutes + row2%minute - row%minute
    end if
  end do

  findBestGuard = startIndex
end function

integer function findBestMinute(input, start, part2)
  type(t_input) :: input
  type(t_entry) :: row, row2
  logical :: part2
  integer :: start, i, guard, m, max, bestMinute, bestGuard
  integer :: minutes(60)

  ! initialize to zero ... maybe there's a fill function
  minutes = 0
  guard = input%entries(start)%guard
  bestMinute = 0
  max = 0

  do i = start,input%size
    row = input%entries(i)
    if  (.not. row%guard == guard) then
      if  (.not. part2) then
        exit
      else
        guard = row%guard
        minutes = 0
      end  if
    end if

    if (.not. row%awake) then
      row2 = input%entries(i+1)
      do m = row%minute+1, row2%minute
        minutes(m) = minutes(m)+1
        if (minutes(m) > max) then
          max = minutes(m)
          bestMinute = m-1
          bestGuard  = guard
        endif
      end do
    end if  
  end do

  findBestMinute = bestMinute * bestGuard
end function

integer function Part1()
  type(t_input) :: input
  integer :: index

  input = LoadInput()

  index = findBestGuard(input)
  Part1 = findBestMinute(input, index, .false.)
end function 

integer function Part2()
  type(t_input) :: input

  input = LoadInput()

  Part2 = findBestMinute(input, 1, .true.)
end function Part2

subroutine Day4Solve()
  integer :: answer

  print *, '--- Day 4 ---'

  answer = Part1()
  print *, 'Answer to part 1 is ', answer

  answer = Part2()
  print *, 'Answer to part 2 is ', answer
end subroutine Day4Solve

end module Day4
