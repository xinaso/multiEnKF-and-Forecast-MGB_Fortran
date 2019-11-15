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
    !   This sub-routine allocates and deallocates the state variables of the model with the vector os state variables for EnKF 
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
	!------------------------------------------------------------------------------
	! Descricao das variáveis de entrada e saida:
	! FLAG =  indica se valores vao para StatVec (1) ou o contrario (2)
    ! FLAGVars = indica as variaveis de estado que serao usados, incicados como (1) as que sim e (0) as que nao
	! nStatVar = numero de variaveis de estado
	! nU = numero de blocos
	! nC = numero de minibacias
	! W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Yfl,Area2,Vol1,TWS2 = variaveis de estado do modelo inercial.
    ! *Variables declarations and routines calls are all commented below.
    
	! ********************************  Description of main input files:  ***********************
	! EnKF_MainInfo.txt - Main informations
    ! EnKF_correlation.txt - Matrix of correlations
    ! EnKF_Perror.txt - Parameters used to perturb the rainfall
    ! EnKF_VarStaError.txt - Parameters used to perturb the state variables of the model
    ! EnKF_Obs_Info.txt - Information of the discharge stations
    ! EnKF_Obs_Q.txt - Record of discharge
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    subroutine EnKF_StatVars (FLAG,FLAGVars,nStatVar,nU,nC,NGC,NFC,StateVec,W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Yfl,Area2,Vol1,TWS2,TWS2mean,FLOODEDsum)


	implicit none
    integer:: FLAG,FLAGVars(14),nStatVar,nU,nC,NGC,NFC
    real :: StateVec(nStatVar)
	real :: W(nC,nU),VBAS(nC),VINT(nC),VSUP(nC),TA(nC),QCEL2(nC),SI(nC,nU)
    real :: Q2fl(nC),Yfl(nC),Area2(nC),Vol1(nC),TWS2(nC)
    real :: TWS2mean(NGC)
    real :: FLOODEDsum(NFC)


	! Variaveis locais:
	integer:: iC,iU,i,j
    integer:: IGC
    integer:: IFC

        ! Guarda variaveis de estado:
! Escreve variáveis de estado:

	if (FLAG==1) then
	    i=0
        if (FLAGVars(1)==1) then
        do iU=1,nU
		    do iC=1,nC
			    i=i+1
			    StateVec(i)=W(iC,iU)
		    enddo
        enddo
        
        endif
        
        if (FLAGVars(2)==1) then
	        do iC=1,nC
			        i=i+1
			        StateVec(i)=VBAS(iC)
            enddo
        endif
        
        if (FLAGVars(3)==1) then
	        do iC=1,nC
			        i=i+1
			        StateVec(i)=VINT(iC)
            enddo
        endif
        
        if (FLAGVars(4)==1) then
	        do iC=1,nC
			        i=i+1
			        StateVec(i)=VSUP(iC)
            enddo
        endif
        
        if (FLAGVars(5)==1) then
	        do iC=1,nC
			        i=i+1
			        StateVec(i)=TA(iC)
            enddo
        endif
                
        if (FLAGVars(6)==1) then
	        do iC=1,nC
			        i=i+1
			        StateVec(i)=QCEL2(iC)
            enddo
        endif
        
        if (FLAGVars(7)==1) then
        do iU=1,nU
		    do iC=1,nC
			    i=i+1
			    StateVec(i)=SI(iC,iU)
		    enddo
        enddo
        
        endif
        
        if (FLAGVars(8)==1) then
	        do iC=1,nC          
			        i=i+1
			        StateVec(i)=Q2fl(iC)
            enddo
        endif
                
        if (FLAGVars(9)==1) then
	        do iC=1,nC          
			        i=i+1
			        StateVec(i)=Yfl(iC)
            enddo
        endif
                        
        if (FLAGVars(10)==1) then
	        do iC=1,nC          
			        i=i+1
			        StateVec(i)=Area2(iC)
            enddo
        endif
        
        if (FLAGVars(11)==1) then
	        do iC=1,nC
			        i=i+1
			        StateVec(i)=Vol1(iC)
            enddo
        endif
        
        if (FLAGVars(12)==1) then
	        do iC=1,nC
			        i=i+1
			        StateVec(i)=TWS2(iC)
            enddo
        endif
        
        if (FLAGVars(13)==1) then
	        do IGC=1,NGC
			        i=i+1
			        StateVec(i)=TWS2mean(IGC)
            enddo
        endif
        
        if (FLAGVars(14)==1) then
	        do IFC=1,NFC
			        i=i+1
			        StateVec(i)=FLOODEDsum(IFC)
            enddo
        endif


    elseif (FLAG==2) then
    
			i=0
            if (FLAGVars(1)==1) then
            do iU=1,nU
				do iC=1,nC
					i=i+1
			        W(iC,iU)=StateVec(i)
				enddo
            enddo
            endif
			
            if (FLAGVars(2)==1) then
			    do iC=1,nC
					    i=i+1
					    VBAS(iC)=StateVec(i)
                enddo
            endif
            
			if (FLAGVars(3)==1) then
			    do iC=1,nC
					    i=i+1
					    VINT(iC)=StateVec(i)
                enddo
            endif
			
            if (FLAGVars(4)==1) then
			    do iC=1,nC
					    i=i+1
					    VSUP(iC)=StateVec(i)
                enddo
            endif
			
            if (FLAGVars(5)==1) then
			    do iC=1,nC
					    i=i+1
					    TA(iC)=StateVec(i)
                enddo
            endif
                        
            if (FLAGVars(6)==1) then
			    do iC=1,nC
					    i=i+1
					    QCEL2(iC)=StateVec(i)
                enddo
            endif
            
            if (FLAGVars(7)==1) then
            do iU=1,nU
		        do iC=1,nC
			        i=i+1
			        SI(iC,iU)=StateVec(i)
		        enddo
            enddo
                
            endif
            
            if (FLAGVars(8)==1) then
			    do iC=1,nC
					    i=i+1
					    Q2fl(iC)=StateVec(i)
                enddo
            endif
            
            if (FLAGVars(9)==1) then
			    do iC=1,nC
					    i=i+1
					    Yfl(iC)=StateVec(i)
                enddo
            endif

            if (FLAGVars(10)==1) then
			    do iC=1,nC
					    i=i+1
					    Area2(iC)=StateVec(i)
                enddo
            endif
                        
            
            if (FLAGVars(11)==1) then
			    do iC=1,nC
					    i=i+1
					    Vol1(iC)=StateVec(i)
                enddo
            endif
            
            if (FLAGVars(12)==1) then
			    do iC=1,nC
					    i=i+1
					    TWS2(iC)=StateVec(i)
                enddo
            endif
            
            if (FLAGVars(13)==1) then
			    do IGC=1,NGC
					    i=i+1
					    TWS2mean(IGC)=StateVec(i)
                enddo
            endif

            if (FLAGVars(14)==1) then
			    do IFC=1,NFC
					    i=i+1
					    FLOODEDsum(IFC)=StateVec(i)
                enddo
            endif
            
    endif
    
    
    
	end subroutine EnKF_StatVars