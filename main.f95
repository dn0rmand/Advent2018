program Advent2018

   use Day1
   use Day2
   use Day3
   use Day4
   use Day5
   use Day6

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
         case ('x')
            call Day6Solve()
         end select
      end do
   else
      call Day1Solve()
      call Day2Solve()
      call Day3Solve()
      call Day4Solve()
      call Day5Solve()
      call Day6Solve()
   end if

end program Advent2018
