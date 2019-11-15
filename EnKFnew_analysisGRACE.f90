    !---------------------------------------------------------------------------------
    !  Licensing:
    !
	!Modelo de Grandes Bacias, South America version (MGB-SA). 
    !Copyright (C) 2019  Hidrologia de Grande Escala (HGE)
	!
    !This program is free software: you can redistribute it and/or modify
    !it under the terms of the GNU General Public License as published by
    !the Free Software Foundation, either version 3 of the License, or
    !any later version.
	!
    !This program is distributed in the hope that it will be useful,
    !but WITHOUT ANY WARRANTY; without even the implied warranty of
    !MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !GNU General Public License for more details.
	!
    !You should have received a copy of the GNU General Public License
    !along with this program.  If not, see <https://www.gnu.org/licenses/>.	
	!
	!---------------------------------------------------------------------------------
    !  Version/Modified: 
    !
    !    2019.03.07 - 03 July 2019 (By: Sly Wongchuig)    
    !
    !  Authors:
    !
    !    Original fortran version by Walter Collischonn
    !    Present fortran version by:
    !    * Walter Collischonn
    !    * Rodrigo Cauduro Dias de Paiva
    !    * Diogo da Costa Buarque
    !    * Paulo Pontes Rógenes
    !    * Mino  Viana Sorribas
    !    * Fernando Mainardi Fan
    !    * Juan Martin Bravo 
    !    * Vinicius Alencar Siqueira
	!	 * Ayan Santos Fleischmann
	!	 * Sly Wongchuig Correa
    !
    !  Main Reference:
    !
    !    Rodrigo Paiva,
    !    Paiva, R.C.D. Collischonn, W., Bonnet, M.P., de Goncalves, L.G.G., Calmant Stéphane, Getirana, A., da Silva J. S., 2013.
    !    Assimilating in situ and radar altimetry data into a large-scale hydrologic-hydrodynamic model for streamflow forecast 
    !    in the Amazon. Hydrol. Earth Syst. Sci., 17, 2929-2946.
    
    !    Sly Wongchuig,
    !    Thesis
    !    Porto Alegre, 2019
    !
	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !   This sub-routine updates the state variables of the model by using the EnKF equations and localization approach by using GRACE data.
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
    
	! ********************************  Description of main input files:  ***********************
    ! End of header
    !---------------------------------------------------------------------------------
    subroutine analysis_newGRACE(A, R, E, S, D, innov, rhom, rhocm, ndim, nrens, nrobs, verbose, truncation,mode,update_randrot)
! Computes the analysed ensemble for A using the EnKF or square root schemes.

   use mod_anafunc
   use m_multa
   implicit none
   integer, intent(in) :: ndim             ! dimension of model state
   integer, intent(in) :: nrens            ! number of ensemble members
   integer, intent(in) :: nrobs            ! number of observations
   
   real*8, intent(inout) :: A(ndim,nrens)    ! ensemble matrix
   real*8, intent(in)    :: R(nrobs,nrobs)   ! matrix holding R (only used if mode=?1 or ?2)
   real*8, intent(in)    :: D(nrobs,nrens)   ! matrix holding perturbed measurments
   real*8, intent(in)    :: E(nrobs,nrens)   ! matrix holding perturbations (only used if mode=?3)
   real*8, intent(in)    :: S(nrobs,nrens)   ! matrix holding HA` 
   real*8, intent(in)    :: innov(nrobs)     ! vector holding d-H*mean(A)
   real*8, intent(in)    :: rhom(ndim,nrobs)  ! matrix of correlation coeficients for Local EnKF                            !2018loc
   real*8, intent(in)    :: rhocm(nrobs,nrobs)! matrix of correlation coeficients for Local EnKF compact version            !2018loc

   logical, intent(in) :: verbose          ! Printing some diagnostic output

   real*8, intent(in)    :: truncation       ! The ratio of variaince retained in pseudo inversion (0.99)

   integer, intent(in) :: mode             ! first integer means (EnKF=1, SQRT=2)
                                           ! Second integer is pseudo inversion
                                           !  1=eigen value pseudo inversion of SS'+(N-1)R
                                           !  2=SVD subspace pseudo inversion of SS'+(N-1)R
                                           !  3=SVD subspace pseudo inversion of SS'+EE'

   logical, intent(in) :: update_randrot   ! Normally true; false for all but first grid point
                                           ! updates when using local analysis since all grid
                                           ! points need to use the same rotation.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real*8 X5(nrens,nrens)
   integer i,nrmin,iblkmax
   logical lreps
   integer j,k 


   real*8, allocatable :: eig(:)
   real*8, allocatable :: W(:,:)
   real*8, allocatable :: X2(:,:)
   real*8, allocatable :: X3(:,:)   
   real*8, allocatable :: Reps(:,:)
   real*8, allocatable :: SSt(:,:)                                                                  !2018loc
   real*8, allocatable :: C(:,:)                                                                    !2018loc
   
   real*8, allocatable :: I1N(:,:)                                                                  ! 2018
   real*8, allocatable :: Ap(:,:)                                                                   ! 2018 

   
!write(*,*) 'Calculating Ap'
    allocate (I1N(nrens,nrens))
          do j=1,nrens
                do k=1,nrens
                    if (j == k) then
                        I1N(j,k)=1-1/nrens
                    else
                        I1N(j,k)=-1/nrens
                    endif
                enddo
          enddo
   
        !! calculate A' is the ensemble perturbation matrix (A-Amean)                               ! 2018
      allocate (Ap(ndim,nrens))
      call dgemm('n','n',ndim,nrens,nrens,1.0,A,ndim,I1N,nrens,0.0,Ap,ndim) 

      
   lreps=.FALSE.
   if (verbose) print * ,'analysis: verbose is on'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Pseudo inversion of C=SS' +(N-1)*R
   print *,'      analysis: Inversion of C:'
   if (nrobs == 1) then
      nrmin=1
      allocate(W(1,1))
      allocate(SSt(1,1))                        !2018loc
      allocate(eig(1))
      eig(1)=dot_product(S(1,:),S(1,:))+real(nrens-1)*R(1,1)
      eig(1)=1.0/eig(1)
      W(1,1)=1.0

   else
      select case (mode)
      case(11,21)
          !write(*,*)'teste1'
        allocate(SSt(nrobs,nrobs))
         nrmin=nrobs
         ! Antes de evaluar C calculo St=S*S'
        !        Evaluate R= S*S` + (nrens-1)*R
                 !call dgemm('n','t',nrobs,nrobs,nrens, &
                 !              1.0, S, nrobs, &
                 !                   S, nrobs, &
                 !    real(nrens-1), R, nrobs)
         call dgemm('n','t',nrobs,nrobs,nrens, &                    !2018loc
                       1.0, S, nrobs, &
                            S, nrobs, &
                       0.0, SSt, nrobs)
         !write(*,*)'creacion de SSt OK'
         
         allocate(C(nrobs,nrobs))                                   !2018loc
        
          do j=1,nrobs                                              !2018loc
                do k=1,nrobs
                    C(j,k) = rhocm(j,k)*SSt(j,k) + real(nrens-1)*R(j,k)
                enddo
          enddo
         
!        Compute eigenvalue decomposition of R -> W*eig*W` 
         allocate(W(nrobs,nrobs))
         allocate(eig(nrobs))
         call eigC(C,nrobs,W,eig)
         call eigsign(eig,nrobs,truncation)
          !write(*,*)'teste2'
      case(12,22)
         nrmin=min(nrobs,nrens)
         allocate(W(nrobs,nrmin))
         allocate(eig(nrmin))
         call lowrankCinv(S,R,nrobs,nrens,nrmin,W,eig,truncation)

      case(13,23)
         nrmin=min(nrobs,nrens)
         allocate(W(nrobs,nrmin))
         allocate(eig(nrmin))
         call lowrankE(S,E,nrobs,nrens,nrmin,W,eig,truncation)

      case default
         print *,'analysis: Unknown mode: ',mode
         stop
      end select
   endif




 !write(*,*)'teste3'



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Generation of X5 (or representers in EnKF case with few measurements)
   print *,'      analysis: Generation of X5:'
   select case (mode)
   case(11,12,13)
      allocate(X3(nrobs,nrens))
      if (nrobs > 1) then
         call genX3(nrens,nrobs,nrmin,eig,W,D,X3)
      else
         X3=D*eig(1)
      endif

      !if (2_8*ndim*nrobs < 1_8*nrens*(nrobs+ndim)) then
      if (1==1) then
!        Code for few observations ( m<nN/(2n-N) )
         if (verbose) print * ,'analysis: Representer approach is used'
         lreps=.true.
         allocate (Reps(ndim,nrobs))
!        Reps=matmul(A,transpose(S))
         !call dgemm('n','t',ndim,nrobs,nrens,1.0,A,ndim,S,nrobs,0.0,Reps,ndim)
         call dgemm('n','t',ndim,nrobs,nrens,1.0,Ap,ndim,S,nrobs,0.0,Reps,ndim)                 ! 2018
         
         ! Parte da rotina onde se aplica o rho com dimensoes (variaveis de estado x obs) /////////////////////////////////////////////////
         write(*,*)'calculating rho ° Reps'
          do j=1,ndim
                do k=1,nrobs
                    Reps(j,k)=rhom(j,k)*Reps(j,k)
                enddo
          enddo
         
      else
         if (verbose) print * ,'analysis: X5 approach is used'
!        X5=matmul(transpose(S),X3)
         call dgemm('t','n',nrens,nrens,nrobs,1.0,S,nrobs,X3,nrobs,0.0,X5,nrens)
         do i=1,nrens
            X5(i,i)=X5(i,i)+1.0
         enddo
      endif

   case(21,22,23)
! Mean part of X5
        !write(*,*)'teste4'
      call meanX5(nrens,nrobs,nrmin,S,W,eig,innov,X5)

! Generating X2
      allocate(X2(nrmin,nrens))
      call genX2(nrens,nrobs,nrmin,S,W,eig,X2)

! Generating X5 matrix
      call X5sqrt(X2,nrobs,nrens,nrmin,X5,update_randrot,mode)

   case default
      print *,'analysis: Unknown flag for mode: ',mode
      stop
   end select


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Generation of inflation
!   call inflationTEST(X5,nrens)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Final ensemble update
   print *,'      analysis: Final ensemble update:'
   if (lreps) then
!     A=A+matmul(Reps,X3)
      call dgemm('n','n',ndim,nrens,nrobs,1.0,Reps,ndim,X3,nrobs,1.0,A,ndim)
      call dumpX3(X3,S,nrobs,nrens)
   else
      iblkmax=min(ndim,200)
      call multa_new(A, X5, ndim, nrens, iblkmax )
      call dumpX5(X5,nrens)
   endif



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if (allocated(X2))    deallocate(X2)
   if (allocated(X3))    deallocate(X3)
   if (allocated(eig))   deallocate(eig)
   if (allocated(W))     deallocate(W)
   if (allocated(Reps))  deallocate(Reps)
   if (allocated(SSt))  deallocate(SSt)             !2018loc
   if (allocated(C))  deallocate(C)                 !2018loc
   
   if (allocated(Ap))  deallocate(Ap)                ! 2018
   if (allocated(I1N))  deallocate(I1N)              ! 2018
   
end subroutine
