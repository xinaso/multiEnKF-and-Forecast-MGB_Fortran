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
    !   This function generates a random number following a normal distribution with mean zero and variance 1 based on Numerical Recipes in Fortran.
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
	! ********************************  Description of main input files:  ***********************
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    FUNCTION gasdev(idum)
	
	! Returns a normally distributed deviate with zero mean and unit variance, using ran1(idum)
	! as a source of uniform deviates
	INTEGER idum
	real*8:: gasdev
	INTEGER iset
	real*8:: fac, gset, rsq, v1, v2, ran1
	SAVE iset, gset
	DATA iset/0/ 
	
	if (iset.eq.0) then							!We don't have an extra deviate handly, so
1		v1=2.*ran(idum)-1.						!pick two uniform numbers in the square extending
		v2=2.*ran(idum)-1.						!from -1 to +1 in each direction.
		rsq=v1**2+v2**2							!see if they are in the unit circle,
		if (rsq.ge.1..or.rsq.eq.0.) goto 1		!and if they are not try again.
		fac=sqrt(-2.*log(rsq)/rsq)				!Now make the Box-Muller transformation to get
		gset=v1*fac								!two normal deviates. Return one and save 
		gasdev=v2*fac							!the other for next time.
		iset=1									!Set flag.	
	else										!We have an extra deviate
		gasdev=gset								!so return it,
		iset=0									!and unset flag.
	endif
	return
	END FUNCTION gasdev