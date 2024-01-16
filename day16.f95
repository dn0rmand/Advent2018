module Day16

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day16Solve

contains

   subroutine loadInput()
      integer :: file
      character(:), allocatable :: line

      file = openFile(16)

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

   subroutine Day16Solve()
      integer :: answer

      print *, '--- Day 16 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
