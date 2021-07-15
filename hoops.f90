!********o*********o*********o*********o*********o*********o*********o**
! This program allows to players to play a game that is intended to
! be a crude model of basketball.  The idea was originally implemented
! on programmable calculators in the early days of that technology.
! The simulation uses fortran's pseudo-random number generator.
!
! It's purpose for this class is as a demonstration of a 'menu-driven'
! user interface.
!
! Inputs
!   A series of choices from the players.
!
! Outputs
!   A series of status updates shown to the players.
!---------------------
! Author:   Bob Harris
! Class:    CMPSC 201F
! Section:  1234
! Date Due: 29 Oct 2003
!********o*********o*********o*********o*********o*********o*********o**

program hoops1977

use, intrinsic :: iso_fortran_env, only : stdin=>input_unit
implicit none

! game configuration

integer, parameter :: gameSeconds     = 40*60   ! 40 minute game
integer, parameter :: overtimeSeconds =  5*60   ! 5 minute overtime(s)

real, parameter    :: stealChance     = 0.25    ! chance pass is stolen
real, parameter    :: reboundChance   = 0.10    ! chance of rebound

! current 'state' of the game

integer :: whosBall         ! which player has the ball (1 or 2)
integer :: score1, score2   ! current score for each player
integer :: secondsLeft      ! number of seconds remaining in the game
integer :: mins, secs       ! working variables to display clock

real    :: shotChance       ! chance of making current shot (0..1)
integer :: pct              ! working variables to display chance

! player commands

character :: command          ! the latest command from the current player

character, parameter :: pass = "p",  shoot = "s"

! other variables
integer :: ierr
real    :: r                ! a random number, 0.0 <= r < 1.0

logical :: takeTheShot      ! true  => take the shot we have
                            ! false => don't
logical :: lookForShot      ! true  => pass ball to get open shot
                            ! false => don't

call random_init(.false., .false.)

!initialize the score and time
score1 = 0
score2 = 0
secondsLeft = gameSeconds
! jump ball
call RANDOM_NUMBER(r)
if (r < 0.5) then
    whosBall = 1
else
    whosBall = 2
end if
print '(1X,A,I1,A)', "Player ", whosBall, " wins the tip"
lookForShot = .true.
call random_number(shotChance)

!----------
! continually loop, processing commands until players decide to quit
!----------

main : do

  if (secondsLeft <= 0) then

    if (score1 == score2) then
      print *,       "Overtime!"
      secondsLeft    = overTimeSeconds
    else
      call gameIsOver()
    endif

  else
    mins = secondsLeft / 60
    secs = secondsLeft - (mins*60)
    pct  = nint(100 * shotChance) ! round to nearest %

    print *,                  "--------------"
    print '(1X,A,I3)',        "Player 1 score: ", score1
    print '(1X,A,I3)',        "Player 2 score: ", score2
    print '(1X,A,I3,A,I2.2)', "Time Left:      ", mins,":",secs
    print '(1X,A,I3,A)',      "You are open for a ",pct,"% shot"
    print '(1X,A,I1,A)',      "Player *", whosBall, "*, what shall we do?"
    print '(3X,A)', "q: take my ball and go home"
    print '(3X,A1,A)', pass,     ": look for another shot"
    print '(3X,A1,A)', shoot,    ": take that shot"
    read(stdin,'(A1)', iostat=ierr) command
  end if

  if (command == "q" .or. IS_IOSTAT_END(ierr)) call gameIsOver()
  if(ierr /= 0) cycle main

  !----------
  ! perform the command
  !----------

  takeTheShot = .false.
  lookForShot = .false.

  select case (command)
    case default
      cycle main
    case (pass)
      lookForShot = .true.
    case (shoot)
      takeTheShot = .true.
  end select

  !----------
  ! do the simulation
  !----------

  ! take a shot, if we're supposed to

  if (takeTheShot) then
      ! see if the shot is made
    call RANDOM_NUMBER(r)
    if (r < shotChance) then
      ! shot was made, count it on the score board

      print *, "Nice shot"
      if (whosBall == 1) then
          score1 = score1 + 2
      else
          score2 = score2 + 2
      end if

      whosBall = 3 - whosBall ! give ball to other player
    else
      ! shot was missed

      print *, "What a brick!"

      ! see if shooter gets own rebound

      call RANDOM_NUMBER(r)
      if (r < reboundChance) then
          print *, "But you got your own rebound"
      else
          whosBall = 3 - whosBall ! give ball to other player
      end if

    end if

    ! look for the next shot

    lookForShot = .true.
  end if

  ! pass the ball around until someone has an open shot

  look: do while (lookForShot)
    ! run some time off the clock

    call RANDOM_NUMBER(r)
    secondsLeft = secondsLeft - INT(12 + 33*r)
    if (secondsLeft <= 0) exit look

    ! see if the ball was stolen

    call RANDOM_NUMBER(r)
    if (r < stealChance) then
      whosBall = 3 - whosBall ! give ball to other player
      print '(1X,A,I1,A)', "Player ", whosBall, " steals the ball"
      cycle look ! go try for an open shot for this player
    end if

    ! figure out chance of making this shot

    call RANDOM_NUMBER(shotChance)
    lookForShot = .false.

  end do look

end do main

contains

subroutine gameIsOver()

print *, "(sound of buzzer)"
print '(1X,A,I3)', "Player 1 score: ", score1
print '(1X,A,I3)', "Player 2 score: ", score2
if (score1 > score2) then
  stop "Congratulations player 1"
elseif (score1 < score2) then
  stop "Congratulations player 2"
elseif (score1 == 0) then
  stop "No one scored!"
else
  stop "It's a tie!"
end if

end subroutine gameIsOver

end program hoops1977
