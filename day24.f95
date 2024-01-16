module Day24

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day24Solve

contains

   subroutine loadInput()
      integer :: file
      character(:), allocatable :: line

      file = openFile(24)

      do while (readLine(file, line))
         deallocate (line)
      end do

      call closeFile(file)
   end subroutine

   integer function Part1()
      Part1 = 0
   end function

   integer function Part2()
      Part2 = 0
   end function

   subroutine Day24Solve()
      integer :: answer

      print *, '--- Day 24 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
