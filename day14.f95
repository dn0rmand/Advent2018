module Day14

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day14Solve

   integer, parameter :: maxSize = 30000000

contains

   integer function loadInput()
      integer :: file, output

      file = openFile(14)
      read (file, *) output
      call closeFile(file)

      loadInput = output
   end function

   subroutine initialize(recipes, length, elf1, elf2)
      integer(kind=1), dimension(:), allocatable, intent(out) :: recipes
      integer, intent(out) :: length, elf1, elf2

      allocate (recipes(maxSize))

      recipes(1) = 3
      recipes(2) = 7
      length = 2
      elf1 = 1
      elf2 = 2
   end subroutine

   logical function matches(recipes, length, target)
      integer(kind=1), dimension(:), allocatable, intent(in) :: recipes
      integer, intent(in) :: length, target
      integer(kind=1) :: v
      integer :: l, t

      t = target
      if (length >= 6) then
         l = length
         do while (l > 0 .and. t > 0)
            v = int(modulo(t, 10), 1)
            if (v /= recipes(l)) then
               matches = .false.
               return
            end if
            t = (t - v)/10
            l = l - 1
         end do
      end if
      matches = t == 0
   end function

   subroutine step(recipes, length, elf1, elf2)
      integer(kind=1), dimension(:), allocatable, intent(inout) :: recipes
      integer, intent(inout) :: length, elf1, elf2
      integer v, v1, v2

      v = recipes(elf1) + recipes(elf2)
      v1 = modulo(v, 10)
      v2 = (v - v1)/10

      if (v2 /= 0) then
         length = length + 1
         recipes(length) = int(v2, 1)
      end if

      length = length + 1
      recipes(length) = int(v1, 1)

      elf1 = 1 + modulo(elf1 + recipes(elf1), length)
      elf2 = 1 + modulo(elf2 + recipes(elf2), length)
   end subroutine

   integer(kind=8) function getValue(recipes, start, length)
      integer(kind=1), dimension(:), allocatable, intent(in) :: recipes
      integer, intent(in) :: start, length
      integer :: i
      integer(kind=8) :: result

      result = 0
      do i = start, start - 1 + length
         result = result*10 + recipes(i)
      end do
      getValue = result
   end function

   integer(kind=8) function Part1()
      integer :: target
      integer(kind=1), dimension(:), allocatable :: recipes
      integer :: length, elf1, elf2

      target = loadInput()

      call initialize(recipes, length, elf1, elf2)

      do while (length < target + 20)
         call step(recipes, length, elf1, elf2)
      end do

      Part1 = getValue(recipes, target + 1, 10)

      deallocate (recipes)
   end function

   integer(kind=8) function Part2()
      integer :: target
      integer(kind=1), dimension(:), allocatable :: recipes
      integer :: length, elf1, elf2

      target = loadInput()

      call initialize(recipes, length, elf1, elf2)

      Part2 = 0
      do while (length < maxSize - 1)
         call step(recipes, length, elf1, elf2)
         if (matches(recipes, length, target)) then
            Part2 = length - 6
            exit
         else if (matches(recipes, length - 1, target)) then
            Part2 = length - 7
            exit
         end if
      end do

      deallocate (recipes)
   end function

   subroutine Day14Solve()
      integer(kind=8) :: answer

      print *, '--- Day 14 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
