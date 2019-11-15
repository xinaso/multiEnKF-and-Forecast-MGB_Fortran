	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This sub-routine generates initial conditions to MGB-IPH model (Inertial version).
    !
    !
    ! Usage:
    !
    ! *CALL HUNT or FINT
    !
    ! uses modules, functions, and subroutines
    !
    ! * USE VARS_MAIN
    ! * USE VARS_INERC (only to Inertial version)
    !
    ! opens
    !
    ! * no files are opened in this routine
    !
    ! reads
    !
    ! * no files are read in this routine
    !
    ! creates
    !
    ! * no files are created in this routine
    !
    !---------------------------------------------------------------------------------
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. - VER ISSO.
    !
    !  Version/Modified: 
    !
    ! 2015.07.06 - 07 July 2015 (by Paulo Pontes)
    !
    !  Authors:
    !
    !    Original fortran version by Walter Collischonn
    !    Present fortran version by:
    !    * Walter Collischonn
    !    * Rodrigo Cauduro Dias de Paiva
    !    * Diogo da Costa Buarque
    !    * Paulo Pontes R�genes
    !    * Mino  Viana Sorribas
    !    * Fernando Mainardi Fan
    !    * Juan Martin Bravo 
    !
    !  Main Reference:
    !
    !    Walter Collischonn,
    !    Modelo de Grandes Bacias - Thesis
    !    Porto Alegre, 2001
    !    ISBN: XXXXXXXXXXX,
    !
    !---------------------------------------------------------------------------------
    ! Variables and Parameters:
    ! *Variables declarations and routines calls are all commented below.
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------

Subroutine flood_ini

!-------------------------------------------------------------------------------------
! Variables and Parameters:
use VARS_INERC
use VARS_MAIN
implicit none

integer ii
real*8 FINT,Vol2_ini,ALFA, BETA
!-------------------------------------------------------------------------------------
dtflood0=DBLE(DTP)
Q2viz=0.0
TWS=0.0

do iC=1,nC

        ALFA = (nMan(iC)*(DBLE(BRIO(iC))**(2.0/3.0)))**0.6/((DECL(iC)**0.5)**0.6) !Parameter ALFA: ALFA*(Eq. Manning)^BETA
        BETA = 0.6 !Parameter BETA: ALFA*(Eq. Manning)^BETA
        Q2fl(iC)=QREF(IC)
        Q2face=QREF(IC)      
        Hfl(iC)=ALFA*(Q2fl(iC)**BETA)/DBLE(BRIO(iC)) + 0.00001
        Yfl(iC)=ZTAB(1,iC)+Hfl(iC)
        Vol1(iC)=Hfl(iC)*DBLE(BRIO(iC))*(SRIO(iC)*1000.)
        Vol2(iC)=Vol1(iC)
        Area2(iC) = FINT(VTAB(:,iC),ATAB(:,iC),NPFL(IC)+2,Vol2(iC))
        Area2(iC) = max(Area2(iC),ACEL(iC))
        jtab(iC)=1  

enddo 



endsubroutine