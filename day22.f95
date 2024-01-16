module Day22

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day22Solve

contains

   subroutine loadInput()
      integer :: file
      character(:), allocatable :: line

      file = openFile(22)

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

   subroutine Day22Solve()
      integer :: answer

      print *, '--- Day 22 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
