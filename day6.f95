module Day6

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day6Solve

   type :: t_point
      integer :: id
      integer :: x, y
      integer :: area
      logical :: infinite
   end type

   type :: t_input
      integer :: size
      integer :: max_x, max_y
      type(t_point) :: points(100)
   end type

contains

   type(t_input) function LoadInput()
      type(t_input), target :: input
      type(t_point) :: point
      integer :: file, max_x, max_y
      character(:), allocatable :: line

      input%size = 0; 
      file = openFile(6)
      max_x = 0
      max_y = 0
      do while (readLine(file, line))
         read (line, *) point%x, point%y
         deallocate (line)
         input%size = input%size + 1
         point%id = input%size
         point%infinite = .false.
         point%area = 0
         input%points(input%size) = point
         max_x = max(max_x, point%x)
         max_y = max(max_y, point%y)
      end do

      call closeFile(file)

      input%max_x = max_x
      input%max_y = max_y

      loadInput = input
   end function

   integer function getMaxAreaSize(input)
      type(t_input), target :: input
      integer :: size
      integer :: x, y, i, maxArea, minDist, minCount, dist
      type(t_point), pointer :: pt, minPoint

      do x = 0, input%max_x
         do y = 0, input%max_y
            minCount = 0
            minDist = 100000

            do i = 1, input%size
               pt => input%points(i)
               dist = abs(x - pt%x) + abs(y - pt%y)
               if (dist < minDist) then
                  minCount = 1
                  minPoint => pt
                  minDist = dist
               else if (dist == minDist) then
                  minCount = minCount + 1
               end if
            end do

            if (minCount == 1) then
               if (x == 0 .or. y == 0 .or. x == input%max_x .or. y == input%max_y) then
                  minPoint%infinite = .true.
                  minPoint%area = 0
               else if (.not. minPoint%infinite) then
                  minPoint%area = minPoint%area + 1
               end if
            end if
         end do
      end do

      maxArea = 0
      do i = 1, input%size
         pt = input%points(i)

         if (pt%area > maxArea) then
            maxArea = pt%area
         end if
      end do

      getMaxAreaSize = maxArea
   end function

   integer function getAreaSize(input, maxDistance)
      type(t_input) :: input
      integer :: size
      integer :: x, y, i, maxDistance, dist
      type(t_point) :: pt

      size = 0
      do x = 0, input%max_x
         do y = 0, input%max_y
            dist = 0
            do i = 1, input%size
               pt = input%points(i)
               dist = dist + abs(x - pt%x) + abs(y - pt%y)
               if (dist >= maxDistance) then
                  exit
               end if
            end do

            if (dist < maxDistance) then
               size = size + 1
            end if
         end do
      end do

      getAreaSize = size
   end function

   integer function Part1()
      type(t_input) :: input

      input = loadInput()
      Part1 = getMaxAreaSize(input)
   end function

   integer function Part2()
      type(t_input) :: input

      input = loadInput(); 
      Part2 = getAreaSize(input, 10000)
   end function

   subroutine Day6Solve()
      integer :: answer

      print *, '--- Day 6 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module Day6
