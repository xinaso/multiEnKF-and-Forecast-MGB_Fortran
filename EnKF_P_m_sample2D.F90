module m_sample2D
! This routine samples pseudo random fields with improved independence
! or orthogonality.  This is done by first drawing a large sample
! and construct the final sample using the dominant singular vectors
! of the large sample.

contains
subroutine sample2D(A2,nx,ny,nrens,nre,rh,samp_fix)
   use m_pseudo2D
   use m_fixsample
   implicit none
   integer, intent(in)     ::  nx,ny
   integer, intent(in)     ::  nrens
   integer, intent(in)     ::  nre
   real,    intent(in)     ::  rh
   logical, intent(in)     ::  samp_fix
   real,    intent(out)    ::  A2(nx,ny,nrens)

   integer n1,n2,n
   integer ns,msx,i,j,nsx,reclA
   integer lwork,ierr,isize,iens
   integer ishape3(3)
   character(len=3) tag3
   real summ,summ2
   real, allocatable, dimension(:,:,:) :: A,A3,A0,UU
   real, allocatable, dimension(:,:)   :: U,VT,VT1,mean,var
   real, allocatable, dimension(:)     :: sig,work

   logical :: debug=.false.

   n1=nx+2*nint(rh); if (mod(n1,2) == 1 ) n1=n1+1
   n2=ny+2*nint(rh); if (mod(n2,2) == 1 ) n2=n2+1

!#ifdef SGI
!   n1=nx+2*nint(rh); if (mod(n1,2) == 1 ) n1=n1+1
!   n2=ny+2*nint(rh); if (mod(n2,2) == 1 ) n2=n2+1
!#else
   do i=1,20
      n1=2**i
      if (n1 > nx+2*nint(rh)) exit
   enddo
   do i=1,20
      n2=2**i
      if (n2 > ny+2*nint(rh)) exit
   enddo
!#endif

   n=nx*ny
   ns=nre*nrens
   msx=min(ns,n)
   nsx=min(nrens,n)

!   print *,'sample2D: samp_fix= ',samp_fix

   if (nre == 1) then
! Standard Monte Carlo sampling
      call pseudo2D(A2,nx,ny,nrens,rh,n1,n2)
!      print *,'pseudo2d done'

   elseif (nre > 1) then
! Start with oversized ensemble of ns members
      lwork=2*max(3*ns+max(n,ns),5*ns)
      allocate(work(lwork))

      allocate(A(nx,ny,ns))
      call pseudo2D(A ,nx,ny,ns   ,rh,n1,n2)

! make an orthogonal VT1 used as linear combination for final ensemble
      allocate (A0(nsx,nsx,1), U(nsx,nsx), sig(nsx), VT1(nsx,nsx) )
!#ifdef SGI
!      n1=nsx+2*nint(rh); if (mod(n1,2) == 1 ) n1=n1+1
!      n2=nsx+2*nint(rh); if (mod(n2,2) == 1 ) n2=n2+1
!#else
      do i=1,20
         n1=2**i
         if (n1 > nsx+2*nint(rh)) exit
      enddo
      do i=1,20
         n2=2**i
         if (n2 > nsx+2*nint(rh)) exit
      enddo
!#endif
      call pseudo2D(A0(:,:,1),nsx,nsx,1,rh,n1,n2)
      call dgesvd('N', 'S', nsx, nsx, A0, nsx, sig, U, nsx, VT1, nsx, work, lwork, ierr)
      if (ierr /= 0) print *, 'ierr',ierr
      deallocate(A0, sig, U)


! Compute SVD of oversized ensemble
      allocate( U(n,msx), sig(msx), VT(msx,msx) )
      call dgesvd('S', 'N', n, ns, A, n, sig, U, n, VT, msx, work, lwork, ierr)
      if (ierr /= 0) print *, 'ierr',ierr

      write(tag3,'(i3.3)')ns

!      inquire(unit=11,opened=lopen)
!      open(11,file='sigma_'//tag3//'.dat')
!         summ=0.0
!         do i=1,ns
!            summ=summ+sig(i)**2
!            write(10,'(i4,3e12.4)')i,sig(i)/sig(1),sig(i)**2/sig(1)**2,summ/float(n*ns)
!         enddo
!      close(11)

! Generate first min(nrens,n) members
      ishape3=(/nx,ny,nsx/)
      allocate(UU(nx,ny,nsx))
      UU=reshape(U(:,1:nsx),ishape3)
      A2=0.0
      do j=1,nsx
         do i=1,nsx
            A2(:,:,j)=A2(:,:,j)+UU(:,:,i)*sig(i)/sqrt(float(nre))*VT1(i,j)
         enddo
      enddo
      deallocate(U, UU, VT, sig, VT1)


      if (debug) then
! SVD of new ensemble
         allocate (U(n,nsx))
         allocate (sig(nsx))
         allocate (VT(nsx,nsx))
         sig=0.0
         call dgesvd('S', 'S', n, nsx, A2, n, sig, U, n, VT, nsx, work, lwork, ierr)
         if (ierr /= 0) print *, 'ierr',ierr

         open(10,file='sigma2_'//tag3//'.dat')
            summ=0.0
            do i=1,nsx
               summ=summ+sig(i)**2
               write(10,'(i4,3e12.4)')i,sig(i)/sig(1),sig(i)**2/sig(1)**2,summ/float(n*ns)
            enddo
         close(10)
         deallocate(U, VT, sig)
         stop
      endif
   else
      print *,'invalid value for nre=',nre
      stop 'm_sample2D'
   endif



! subtract mean and correct variance
   
   if (samp_fix) call fixsample(A2,nx,ny,nrens)

   if (debug) then
      allocate(mean(nx,ny))
      allocate(var(nx,ny))
      mean=0.0
      do j=1,nrens
         mean(:,:)=mean(:,:)+A2(:,:,j)
      enddo
      mean=(1.0/float(nrens))*mean


      var=0.0
      do iens=1,nrens
         do j=1,ny
         do i=1,nx
            var(i,j)=var(i,j)+A2(i,j,iens)**2
         enddo
         enddo
      enddo

      open(10,file='check.dat')
         do j=1,ny
         do i=1,nx
            write(10,'(2i5,2g13.5)')i,j,mean(i,j),var(i,j)
         enddo
         enddo
      close(10)

      deallocate(mean)
      deallocate(var)

      stop
   endif

end subroutine sample2D
end module m_sample2D
