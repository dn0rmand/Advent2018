module Day24

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day24Solve

   integer, parameter :: IMMUNE = 1
   integer, parameter :: INFECTION = 2

   ! Flags => can be composed
   integer, parameter :: BLUDGEONING = 1
   integer, parameter :: SLASHING = 2
   integer, parameter :: COLD = 4
   integer, parameter :: FIRE = 8
   integer, parameter :: RADIATION = 16

   type :: t_group
      integer :: original_units = 0
      integer :: unit_type
      integer :: units = 0
      integer :: hit_points = 0
      integer :: weakness = 0
      integer :: immunity = 0
      integer :: attack_type = 0
      integer :: attack_strengh = 0
      integer :: initiative = 0
      integer :: boost = 0

      type(t_group), pointer :: enemy => null()
      logical :: is_target = .false.
   contains
      procedure :: powerLevel => group_power_level
      procedure :: getDamage => group_get_damage
      procedure :: applyDamage => group_apply_damage
   end type

   type, extends(sortable)  :: t_input
      type(t_group), dimension(30) :: groups
      integer, dimension(30) :: sorted_indexes
   contains
      procedure :: compare => input_compare
      procedure :: swap => input_swap

      procedure :: doWar => input_do_war
      procedure :: selectTargets => input_select_targets
      procedure :: unitCount => input_unit_count
   end type
contains

! Group

   integer function group_apply_damage(this, attacker)
      class(t_group), intent(inout) :: this
      type(t_group), intent(in) :: attacker
      integer :: power, killed

      group_apply_damage = 0

      if (attacker%units == 0 .or. this%units == 0) then
         return
      end if

      power = this%getDamage(attacker)
      killed = power/this%hit_points
      this%units = this%units - killed

      if (this%units <= 0) then
         this%units = 0
      end if

      group_apply_damage = killed
   end function

   integer function group_get_damage(this, attacker)
      class(t_group), intent(in) :: this
      type(t_group), intent(in) :: attacker
      integer :: power

      power = attacker%powerLevel()

      if (iand(attacker%attack_type, this%immunity) /= 0) then
         power = 0
      else if (iand(attacker%attack_type, this%weakness) /= 0) then
         power = power*2
      end if

      group_get_damage = power
   end function

   integer function group_power_level(this)
      class(t_group), intent(in) :: this
      integer :: power

      power = this%units*(this%attack_strengh + this%boost)
      group_power_level = power
   end function

! Input function

   integer function input_unit_count(this, unitTypes)
      class(t_input), intent(in) :: this
      integer :: i, count, unitTypes

      count = 0
      do i = 1, this%size
         if (iand(this%groups(i)%unit_type, unitTypes) /= 0) then
            count = count + this%groups(i)%units
         end if
      end do

      input_unit_count = count
   end function

   subroutine input_select_targets(this)
      class(t_input), intent(inout), target :: this
      type(t_group), pointer :: attacker, t, best
      integer :: i, j, d, p, bestDamage, bestPower

      do i = 1, this%size
         this%sorted_indexes(i) = i
         this%groups(i)%enemy => null()
         this%groups(i)%is_target = .false.
      end do
      call this%sort()

      do i = 1, this%size
         attacker => this%groups(this%sorted_indexes(i))
         if (attacker%units == 0) then
            exit
         end if
         best => null()
         bestDamage = 0
         bestPower = 0
         do j = 1, this%size
            t => this%groups(j)
            if (t%units == 0) then
               cycle
            end if
            if (t%is_target .or. t%unit_type == attacker%unit_type) then
               cycle
            end if

            d = t%getDamage(attacker)
            p = t%powerLevel()

            if (d == 0) then
               cycle
            end if

            if (d > bestDamage .or. (d == bestDamage .and. p > bestPower)) then
               best => t
               bestDamage = d
               bestPower = p
            else if (d == bestDamage .and. p == bestPower .and. t%initiative > best%initiative) then
               best => t
               bestDamage = d
               bestPower = p
            end if
         end do

         if (associated(best)) then
            best%is_target = .true.
            attacker%enemy => best
         end if
      end do
   end subroutine

   subroutine input_dump(this, steps)
      type(t_input), intent(in) :: this
      integer, intent(in) :: steps
      integer :: i

      print *, '----- ', itoa(steps), ' -----'
      do i = this%size, 1, -1
         if (this%groups(i)%units > 0) then
            print *, itoa(this%groups(i)%initiative), ' -> ', itoa(this%groups(i)%units)
         end if
      end do
   end subroutine

   logical function input_do_war(this, boost)
      class(t_input), target, intent(inout) :: this
      integer, intent(in) :: boost
      type(t_group), pointer :: attacker, enemy
      integer :: i, immune_count, infection_count
      integer :: killed

      input_do_war = .true. ! success by default
      ! reset units counts
      immune_count = 0
      infection_count = 0
      do i = 1, this%size
         enemy => this%groups(i)
         enemy%units = enemy%original_units
         if (enemy%unit_type == INFECTION) then
            infection_count = infection_count + 1
         else
            immune_count = immune_count + 1
            enemy%boost = boost
         end if
      end do

      ! now go to war
      do while (immune_count > 0 .and. infection_count > 0)
         call this%selectTargets()

         killed = 0
         do i = this%size, 1, -1
            attacker => this%groups(i)
            if (attacker%units == 0 .or. (.not. associated(attacker%enemy))) then
               cycle
            end if

            enemy => attacker%enemy
            killed = killed + enemy%applyDamage(attacker)
            if (enemy%units == 0) then
               if (enemy%unit_type == INFECTION) then
                  infection_count = infection_count - 1
               else
                  immune_count = immune_count - 1
               end if
            end if
         end do

         if (killed == 0) then
            ! forever loop detected
            input_do_war = .false. ! error
            exit
         end if
      end do
   end function

! Sorting functions

   integer function input_compare(this, i1, i2)
      class(t_input), intent(in) :: this
      integer, intent(in) :: i1, i2
      integer :: j1, j2
      type(t_group) :: t1, t2
      integer :: diff

      j1 = this%sorted_indexes(i1)
      j2 = this%sorted_indexes(i2)

      t1 = this%groups(j1)
      t2 = this%groups(j2)

      if (t1%units /= t2%units) then
         if (t1%units == 0) then
            input_compare = 1
            return
         else if (t2%units == 0) then
            input_compare = -1
            return
         end if
      end if

      diff = t2%powerLevel() - t1%powerLevel()
      if (diff == 0) then
         diff = t2%initiative - t1%initiative
      end if

      input_compare = diff
   end function

   subroutine input_swap(this, i1, i2)
      class(t_input), intent(inout) :: this
      integer, intent(in) :: i1, i2
      integer :: t1, t2

      t1 = this%sorted_indexes(i1)
      t2 = this%sorted_indexes(i2)

      this%sorted_indexes(i1) = t2
      this%sorted_indexes(i2) = t1
   end subroutine

! input parsing / loading

   integer function parse_attack_name(attack_name)
      character(*) :: attack_name

      select case (attack_name)
      case ('bludgeoning')
         parse_attack_name = BLUDGEONING
      case ('slashing')
         parse_attack_name = SLASHING
      case ('cold')
         parse_attack_name = COLD
      case ('fire')
         parse_attack_name = FIRE
      case ('radiation')
         parse_attack_name = RADIATION
      case default
         stop 'Invalid attack name'
      end select
   end function

   subroutine parse_weakImmune(group, vals)
      type(t_group), intent(inout) :: group
      character(:), allocatable :: vals, attack
      character(11) :: tmp
      logical :: weak, immune
      integer :: val

      weak = vals(1:7) == 'weak to'
      immune = vals(1:9) == 'immune to'

      if (weak) then
         vals = vals(9:len(vals))
      else if (immune) then
         vals = vals(11:len(vals))
      else
         stop 'Has to be either immune or weak'
      end if

      vals = trim(vals)
      val = 0
      do while (len(vals) > 0)
         if (vals(1:1) == ',') then
            vals = vals(3:len(vals))
         end if
         read (vals, *) tmp
         attack = trim(tmp)
         vals = trim(vals(len(attack) + 1:len(vals)))
         val = ior(val, parse_attack_name(attack))
      end do

      if (weak) then
         group%weakness = val
      else
         group%immunity = val
      end if
   end subroutine

   subroutine loadSystem(file, input, unit_type)
      integer, intent(in) :: file
      integer :: ios, index
      type(t_input), target, intent(inout) :: input
      type(t_group) :: group
      integer :: unit_type
      character(:), allocatable :: line
      character(17) :: w_units, w_any
      character(12) :: w_points
      character(11) :: attack_name
      character(150) :: remaining, weakImmune

      if (.not. readLine(file, line)) then
         stop 'Unexpected end of input file'
      end if

      if (unit_type == IMMUNE .and. line /= "Immune System:") then
         stop 'Expected Immune System start'
      else if (unit_type == INFECTION .and. line /= 'Infection:') then
         stop 'Expected Infection start'
      else if (unit_type /= IMMUNE .and. unit_type /= INFECTION) then
         stop 'Invalid system type'
      end if
      deallocate (line)

      group%unit_type = unit_type

      do while (readLine(file, line))
         group%weakness = 0
         group%immunity = 0
         if (len(line) == 0) then
            deallocate (line)
            exit
         end if
         read (line, '(I4,A,I5,A,A)', iostat=ios) group%units, w_units, group%hit_points, w_points, remaining
         group%original_units = group%units
         deallocate (line)

         if (remaining(1:1) == '(') then
            index = scan(remaining, ')')
            weakImmune = remaining(2:index - 1)
            remaining = remaining(index + 27:len(remaining))
            index = scan(weakImmune, ';')
            if (index > 0) then
               line = weakImmune(1:index - 1)
               call parse_weakImmune(group, line)
               line = weakImmune(index + 2:len(weakImmune))
               call parse_weakImmune(group, line)
            else
               line = weakImmune
               call parse_weakImmune(group, line)
            end if
         else
            remaining = remaining(25:len(remaining))
         end if

         read (remaining, *) group%attack_strengh, attack_name, w_any, w_any, w_any, group%initiative

         group%attack_type = parse_attack_name(attack_name)
         input%size = input%size + 1
         input%groups(group%initiative) = group !initiative is unique
      end do
   end subroutine

   type(t_input) function loadInput()
      integer :: file
      type(t_input) :: input
      type(t_group) :: dummy

      input%groups = dummy

      file = openFile(24)

      call loadSystem(file, input, IMMUNE)
      call loadSystem(file, input, INFECTION)

      call closeFile(file)

      loadInput = input
   end function

! Part1 and Part 2

   integer function Part1(input)
      type(t_input), intent(inout) :: input

      if (input%doWar(0)) then
         Part1 = input%unitCount(ior(IMMUNE, INFECTION))
      else
         Part1 = -1
      end if
   end function

   integer function Part2(input)
      type(t_input), intent(inout) :: input
      integer :: min, max, boost, result

      min = 1
      max = 200

      do while (min < max)
         boost = (min + max)/2
         if (input%doWar(boost)) then
            result = input%unitCount(IMMUNE)

            if (result == 0) then
               min = boost + 1
            else
               max = boost
            end if
         else
            min = boost + 1
         end if
      end do

      Part2 = result
   end function

   subroutine Day24Solve()
      integer :: answer
      type(t_input) :: input

      input = loadInput()

      print *, '--- Day 24 ---'

      answer = Part1(input)
      print *, 'Answer to part 1 is ', answer

      answer = Part2(input)
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
