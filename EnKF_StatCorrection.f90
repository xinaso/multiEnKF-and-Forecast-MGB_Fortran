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
    !    2017.20.10 - 20 October 2017 (By: Sly Wongchuig)    
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
    !   This sub-routine is used to perturb the VBAS state variable
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
	! nStatVar = numero de variaveis de estado
	! nU = numero de blocos
	! nC = numero de minibacias
	! nX = numero de secoes do modelo hidrodinamico
	! NSUBT = numero de secoes do Muskingum Cunge
	! Vars locais:
	! W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Yfl,Area2,Vol1,TWS2 = variaveis de estado do modelo inercial.
    ! *Variables declarations and routines calls are all commented below.
    ! End of header
    !---------------------------------------------------------------------------------
    subroutine EnKF_StatCorrection

	!-----------------------------------------------------------------------------
	! Declaracao de variaveis:
    USE VARS_MAIN

    USE VARS_INERC
	USE EnKF_MOD
	implicit none
    integer:: i,j,k

    
    do iEns=1,nEns
        StateVec=Akf(:,iEns)

        call EnKF_StatVars(2,FLAGVars,nStatVar,nU,nC,NGC,NFC,StateVec,W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Hfl,Area2,Vol1,TWS2,TWS2mean,FLOODEDsum)
            
        ! Verificacoes e correcoes:       
        do iC=1,nC
            iB=IBAC(iC)
            do iU=1,nU     
                if (W(iC,iU)<0.0.or.W(iC,iU)>WM(iB,iU)) then
                    W(iC,iU)=min(WM(iB,iU),W(iC,iU))
                    W(iC,iU)=max(0.0,W(iC,iU))
                endif
            enddo
        enddo
        
        do iC=1,nC

                VSUP(iC)=max(VSUP(iC),0.0)
                VINT(iC)=max(VINT(iC),0.0)
                VBAS(iC)=max(VBAS(iC),0.0)
                
                QCEL2(iC)=max(QCEL2(iC),0.0)
   
        enddo

        do iC=1,nC
            Hfl(ic)=max(Hfl(iC),0.0)
            Vol2(iC) = max(Vol2(iC),0.0)
        enddo
        
        DO iC=1,nC
            TWS2(iC)=max(TWS2(iC),0.0)
        ENDDO
        
        DO iC=1,nC
            Area2(iC)=max(Area2(iC),0.0)
        ENDDO
        
          
        call EnKF_StatVars(1,FLAGVars,nStatVar,nU,nC,NGC,NFC,StateVec,W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Hfl,Area2,Vol1,TWS2,TWS2mean,FLOODEDsum)
        Akf(:,iEns)=StateVec
	
	enddo
	
	end subroutine EnKF_StatCorrection