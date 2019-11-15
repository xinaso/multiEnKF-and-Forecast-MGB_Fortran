module m_fixsample
contains
subroutine fixsample(E,nx,ny,nrens)
   integer, intent(in)    :: nrens
   integer, intent(in)    :: nx,ny
   real   , intent(inout) :: E(nx,ny,nrens)

   integer iens,i
   real, allocatable :: average(:,:), variance(:,:)

   allocate(average(nx,ny), variance(nx,ny))

   average=0.0
   do iens=1,nrens
      average(:,:)=average(:,:)+E(:,:,iens)
   enddo
   average=average/float(nrens)

   do iens=1,nrens
      E(:,:,iens)=E(:,:,iens)-average(:,:)
   enddo

   variance=0.0
   do iens=1,nrens
      variance(:,:) = variance(:,:) + E(:,:,iens)**2.
   enddo

   do j=1,ny
   do i=1,nx
      variance(i,j)=1.0/sqrt( variance(i,j)/float(nrens) )
   enddo
   enddo

   do iens=1,nrens
      do j=1,ny
      do i=1,nx
         E(i,j,iens)=variance(i,j)*E(i,j,iens)
      enddo
      enddo
   enddo

   deallocate(average,variance)

end subroutine
end module
