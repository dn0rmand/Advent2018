module Day20

   use tools

   implicit none

   public :: Day20Solve

   integer, parameter :: SIZE = 55

   type :: t_section
      integer :: from, to
      logical, dimension(-SIZE:SIZE, -SIZE:SIZE) :: visited
      type(t_section), pointer :: continueWith => null()
      type(t_section), pointer :: next => null()
   end type

   type :: p_section
      type(t_section), pointer :: section => null()
   end type

   type :: t_input
      character(:), allocatable :: data
      type(t_section) :: mainSection
      integer, dimension(-SIZE:SIZE, -SIZE:SIZE) :: map
   contains
      procedure :: parse => input_parse
      procedure :: follow => input_follow
      procedure :: didMove => input_did_move
   end type

   type(p_section), dimension(225000000) :: cached_sections

contains

   type(t_input) function loadInput()
      type(p_section) :: empty
      type(t_input), allocatable :: input
      integer :: file
      logical :: dummy
      type(t_section) :: section

      allocate (input)

      cached_sections = empty
      input%map = 0

      file = openFile(20)
      dummy = readLine(file, input%data)
      call closeFile(file)

      section%from = 2
      section%to = len(input%data) - 1
      section%continueWith => null()
      section%next => null()
      section%visited = .false.
      input%mainSection = section

      loadInput = input
   end function

   subroutine input_did_move(input, x, y, doors)
      class(t_input), intent(inout) :: input
      integer, intent(in) :: x, y, doors

      if (input%map(x, y) == 0) then
         input%map(x, y) = doors
      else
         input%map(x, y) = min(input%map(x, y), doors)
      end if
   end subroutine

   subroutine free_section(current)
      type(t_section), pointer :: current
      type(t_section), pointer :: tmp, last

      if (associated(current)) then
         return
      end if
      if (associated(current%continueWith)) then
         last => current%continueWith
      else
         last => null()
      end if
      do while (associated(current))
         tmp => current
         current => current%next

         tmp%continueWith => null()
         tmp%next => null()
         deallocate (tmp)
      end do
      if (associated(last)) then
         deallocate (last)
      end if
   end subroutine

   recursive subroutine input_follow(input, section, x, y, doors)
      class(t_input), intent(inout) :: input
      type(t_section), pointer :: section
      type(t_section), pointer :: sections
      integer, value :: x, y, doors
      logical :: done
      integer :: i
      character :: c

      if (.not. associated(section)) then
         return
      end if

      if (section%visited(x, y) .and. input%map(x, y) < doors) then
         return
      end if

      section%visited(x, y) = .true.

      if (section%from <= section%to) then
         done = .false.

         do i = section%from, section%to
            c = input%data(i:i)
            select case (c)
            case ('(')
               call input%parse(section, i + 1, sections)
               do while (associated(sections))
                  call input%follow(sections, x, y, doors)
                  sections => sections%next
               end do
               done = .true.

            case ('N')
               y = y - 1
               doors = doors + 1
               call input%didMove(x, y, doors)
            case ('S')
               y = y + 1
               doors = doors + 1
               call input%didMove(x, y, doors)
            case ('E')
               x = x + 1
               doors = doors + 1
               call input%didMove(x, y, doors)
            case ('W')
               x = x - 1
               doors = doors + 1
               call input%didMove(x, y, doors)
            case default
            end select

            if (done) then
               exit
            end if
         end do
      end if

      call input%follow(section%continueWith, x, y, doors)
   end subroutine

   subroutine input_parse(input, section, index, newSection)
      class(t_input), intent(inout) :: input
      integer, intent(in) :: index
      integer :: p, i, key
      character :: c
      type(t_section), intent(in) :: section
      type(t_section), pointer :: tmp, current
      type(t_section), pointer, intent(out) :: newSection
      type(t_section), pointer :: continueWith

      key = index*15000 + section%to

      newSection => cached_sections(key)%section
      if (associated(newSection)) then
         return
      end if

      allocate (newSection)
      allocate (continueWith)

      newSection%from = index
      newSection%continueWith => continueWith
      newSection%next => null()

      i = index
      p = 1
      current => newSection
      do while (p > 0 .and. i <= section%to)
         c = input%data(i:i)
         i = i + 1
         if (p == 1 .and. c == '|') then
            allocate (tmp)

            tmp%from = i
            tmp%continueWith => continueWith
            tmp%next => null()

            current%to = i - 2
            current%visited = .false.
            current%next => tmp

            current => tmp
            tmp => null()
         else if (c == ')') then
            p = p - 1
         else if (c == '(') then
            p = p + 1
         end if
      end do
      if (p /= 0) then
         print *, "not right"
      end if

      current%to = i - 2
      current%visited = .false.

      continueWith%from = i
      continueWith%to = section%to

      cached_sections(key)%section => newSection
   end subroutine

   subroutine solve(part1, part2)
      type(t_input), allocatable, target :: input
      type(t_section), pointer :: section
      integer, dimension(-SIZE:SIZE, -SIZE:SIZE) :: remap
      integer, intent(out) :: part1, Part2

      input = loadInput()
      section => input%mainSection

      call input%follow(section, 0, 0, 0)

      where (input%map >= 1000)
         remap = 1
      else where
         remap = 0
      end where

      part1 = maxval(input%map)
      part2 = sum(remap)

      deallocate (input)
   end subroutine

   subroutine Day20Solve()
      integer :: part1, part2

      print *, '--- Day 20 ---'

      call solve(part1, part2)

      print *, 'Answer to part 1 is ', part1
      print *, 'Answer to part 2 is ', part2
   end subroutine

end module
