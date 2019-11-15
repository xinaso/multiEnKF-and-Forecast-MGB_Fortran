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
    !   This module especifically updates the state variables by EnKF
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
    
	! ********************************  Description of main input files:  ***********************
    ! End of header
    !---------------------------------------------------------------------------------
    module m_multa
contains
subroutine multa_new(A, X, ndim, nrens, iblkmax)
implicit none
integer, intent(in) :: ndim
integer, intent(in) :: nrens
integer, intent(in) :: iblkmax
real*8, intent(in)    :: X(nrens,nrens)
real*8, intent(inout) :: A(ndim,nrens)
real*8 v(iblkmax,nrens)  ! Automatic work array

integer ia,ib
do ia = 1,ndim,iblkmax
  ib = min(ia+iblkmax-1,ndim)
  v(1:ib-ia+1,1:nrens) = A(ia:ib,1:nrens)
  call dgemm('n','n', ib-ia+1, nrens, nrens, &
              1.0, v(1,1), iblkmax, &
              X(1,1), nrens, &
              0.0, A(ia,1), ndim)
enddo
end subroutine multa_new
end module m_multa
