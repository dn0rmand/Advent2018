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

   implicit none

   integer :: num_args, ix
   character(len=20) :: arg

   print *, '##################################'
   print *, '# Advent of Code 2018 in Fortran #'
   print *, '##################################'

   num_args = command_argument_count()
   if (num_args > 0) then
      do ix = 1, num_args
         call get_command_argument(ix, arg)
         select case (arg)
         case ('1')
            call Day1Solve()
         case ('2')
            call Day2Solve()
         case ('3')
            call Day3Solve()
         case ('4')
            call Day4Solve()
         case ('5')
            call Day5Solve()
         case ('6')
            call Day6Solve()
         case ('7')
            call Day7Solve()
         case ('8')
            call Day8Solve()
         case ('9')
            call Day9Solve()
         case ('x')
            call Day9Solve()
         end select
      end do
   else
      call Day1Solve()
      call Day2Solve()
      call Day3Solve()
      call Day4Solve()
      call Day5Solve()
      call Day6Solve()
      call Day7Solve()
      call Day8Solve()
      call Day9Solve()
   end if

end program Advent2018
