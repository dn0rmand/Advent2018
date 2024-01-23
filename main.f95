program Advent2018

   use Day1
   use Day2
   use Day3
   use Day4
   use Day5
   use Day6
   use Day7
   use Day8
   use Day9
   use Day10
   use Day11
   use Day12
   use Day13
   use Day14
   use Day15
   use Day16
   use Day17
   use Day18
   use Day19
   use Day20
   use Day21
   use Day22
   use Day23
   use Day24
   use Day25

   implicit none

   integer :: num_args, ix
   character(len=20) :: arg
   real :: start, finish

   !  print *, 'Press Enter to start'
   !  read (*, *)

   print *, '##################################'
   print *, '# Advent of Code 2018 in Fortran #'
   print *, '##################################'

   num_args = command_argument_count()
   if (num_args > 0) then
      do ix = 1, num_args
         call get_command_argument(ix, arg)
         if (arg == 'x') then
            call cpu_time(start)
            call Day21Solve()
            call cpu_time(finish)
            print '("Executed in ",f6.3," seconds.")', finish - start
            exit
         end if
      end do
   else
      call cpu_time(start)

      call Day1Solve()
      call Day2Solve()
      call Day3Solve()
      call Day4Solve()
      call Day5Solve()
      call Day6Solve()
      call Day7Solve()
      call Day8Solve()
      call Day9Solve()
      call Day10Solve()
      call Day11Solve()
      call Day12Solve()
      call Day13Solve()
      call Day14Solve()
      call Day15Solve()
      call Day16Solve()
      call Day17Solve()
      call Day18Solve()
      call Day19Solve()
      call Day20Solve()
      call Day21Solve()
      call Day22Solve()
      call Day23Solve()
      call Day24Solve()
      call Day25Solve()

      call cpu_time(finish)

      print '("All days executed in ",f6.3," seconds.")', finish - start
   end if

end program Advent2018
