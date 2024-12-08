module Day18

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day18Solve

   integer(kind=1), parameter :: OPEN = 0
   integer(kind=1), parameter :: FOREST = 1
   integer(kind=1), parameter :: YARD = 2

   integer, parameter :: PART2_TIME = 1000000000

   type :: t_input
      integer(kind=1), dimension(50, 50) :: map
      integer :: rows, columns

   contains
      procedure :: dump => input_dump
      procedure :: get => input_get
      procedure :: transform => input_transform
      procedure :: add => input_add
      procedure :: score => input_score
   end type
contains

   subroutine input_dump(input)
      class(t_input), intent(in) :: input
      character(50) :: line
      integer :: x, y

      do y = 1, input%rows
         line = ''
         do x = 1, input%columns
            select case (input%map(x, y))
            case (OPEN)
               line(x:x) = '.'
            case (YARD)
               line(x:x) = '#'
            case (FOREST)
               line(x:x) = '|'
            end select
         end do
         print *, trim(line)
      end do
      print *, ''
   end subroutine

   type(t_input) function loadInput()
      type(t_input) :: input
      integer :: file, i
      integer(kind=1) :: value
      character(:), allocatable :: line

      input%rows = 0
      input%columns = 0
      input%map = OPEN

      file = openFile(18)

      do while (readLine(file, line))
         input%columns = max(input%columns, len(line))
         input%rows = input%rows + 1
         do i = 1, len(line)
            value = OPEN
            select case (line(i:i))
            case ('.')
               value = OPEN
            case ('|')
               value = FOREST
            case ('#')
               value = YARD
            case default
               value = OPEN
            end select
            input%map(i, input%rows) = value
         end do
         deallocate (line)
      end do

      call closeFile(file)

      loadInput = input
   end function

   integer(kind=1) function input_get(input, x, y)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x, y

      if (x < 1 .or. y < 1 .or. x > input%columns .or. y > input%rows) then
         input_get = OPEN
      else
         input_get = input%map(x, y)
      end if
   end function

   subroutine input_add(input, x, y, trees, yards)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x, y
      integer, intent(inout) :: trees, yards

      select case (input%get(x, y))
      case (FOREST)
         trees = trees + 1
      case (YARD)
         yards = yards + 1
      end select
   end subroutine

   integer function input_score(input)
      class(t_input), intent(in) :: input
      integer :: yards, trees, x, y

      yards = 0
      trees = 0

      do y = 1, input%rows
         do x = 1, input%columns
            call input%add(x, y, trees, yards)
         end do
      end do

      input_score = yards*trees
   end function

   integer function input_transform(input)
      class(t_input), intent(inout) :: input
      type(t_input) :: newInput
      integer :: x, y, xx, yy, trees, yards
      integer :: new_trees, new_yards
      integer(kind=1) :: value

      new_trees = 0
      new_yards = 0
      newInput%map = OPEN
      do y = 1, input%rows
         do x = 1, input%columns
            value = input%get(x, y)
            trees = 0
            yards = 0
            do yy = y - 1, y + 1
               do xx = x - 1, x + 1
                  if (x == xx .and. y == yy) then
                     cycle
                  end if
                  call input%add(xx, yy, trees, yards)
               end do
            end do
            select case (value)
            case (OPEN)
               if (trees >= 3) then
                  value = FOREST
                  new_trees = new_trees + 1
               end if
            case (FOREST)
               if (yards >= 3) then
                  value = YARD
                  new_yards = new_yards + 1
               else
                  new_trees = new_trees + 1
               end if
            case (YARD)
               if (yards == 0 .or. trees == 0) then
                  value = OPEN
               else
                  new_yards = new_yards + 1
               end if
            end select
            newInput%map(x, y) = value
         end do
      end do

      input%map = newInput%map

      input_transform = new_trees*new_yards
   end function

   integer function Part1()
      type(t_input) :: input
      integer :: time, score

      input = loadInput()
      do time = 1, 10
         score = input%transform()
      end do

      Part1 = score
   end function

   integer function Part2()
      type(t_input) :: input
      logical :: good
      integer :: time, score, diff, jump, i
      integer, dimension(:), allocatable :: freq
      integer, dimension(0:100) :: last

      allocate (freq(800000))

      last = 0
      freq = -1
      jump = 0
      input = loadInput()
      score = input%score()
      freq(score) = 0
      time = 1
      do while (time <= PART2_TIME)
         score = input%transform()
         if (jump == 0) then
            last = cshift(last, -1)
            last(0) = score
            if (freq(score) >= 0) then
               diff = time - freq(score)
               if (2*diff < 100) then
                  good = .true.
                  do i = 0, diff
                     if (last(i) /= last(diff + i)) then
                        good = .false.
                        exit
                     end if
                  end do
                  if (good) then
                     jump = PART2_TIME - time
                     jump = jump - modulo(jump, diff)
                     time = time + jump
                  end if
               end if
               freq(score) = time
            else
               freq(score) = time
            end if
         end if
         time = time + 1
      end do

      Part2 = score
   end function

   subroutine Day18Solve()
      integer :: answer

      print *, '--- Day 18 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
