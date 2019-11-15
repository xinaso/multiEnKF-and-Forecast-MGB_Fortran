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
    !   This sub-routine does prevision using Data Assimilation technique ensemble Kalman filter (EnKF) by Evensen (2002) 
    !   Using past climate and initial conditions from EnKF
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
	! KK = Numero de previsões por ano (K):
	! Data_ESP(.,.) = Dia e mes das K previsões (K,2)
	! lead = Horizonte previsão (lead time)
	! nStatVar=numero de variaveis de estado
	! ********************************  Description of main input files:  ***********************
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    SUBROUTINE EnKF_1_Previsao
	!------------------------------------------------------------------------------------------------
	USE VARS_MAIN
	USE EnKF_MOD
    USE VARS_INERC
	IMPLICIT NONE

	integer:: iKK,flag1
	
	integer:: iTchuva2,iTclim,iTtemp2,iTvar,iTfor,iTprev
	integer julday	
	integer IMESkk,IDIAkk,iKK0
	integer ilead
	INTEGER ITTEMP
	INTEGER ITINI
	INTEGER I,J
    real*8 iprc,facprc	
    
    integer:: ii
    real*8:: trash1,trash2
    integer:: nEns_prev,nT_prev
    !*************************************************************************************
	

    ! (4) Previsão:
        nEns_prev=Anofim_ESP-Anoini_ESP+1
        JDIA=JULDAY(12,31,Anofim_ESP)
        nT_prev=JDIA-IDINI+1
		ITTEMP=IT
		iTprev=iT	!!!

    	! Verifica se é dia de previsão:
		flag1=0
		do iKK=1,KK
			if (IDIA==Data_ESP(iKK,1).and.IMES==Data_ESP(iKK,2)) then
				IMESkk=IMES
				IDIAkk=IDIA
				iKK0=iKK
				flag1=1
			endif						 
		enddo

		if (flag1==1) then
	
	        write(*,*) "(2) Previsao"
            iprev=iprev+1    	    
			! Loop do ensemble
			do iEns=1,nEns_prev
				! Recupera variaveis de estado:
                StateVec=Akf1(:,iEns)  
                call EnKF_StatVars(2,FLAGVars,nStatVar,nU,nC,NGC,NFC,StateVec,W,VBAS,VINT,VSUP,TA,QCEL2,SI,Q2fl,Hfl,Area2,Vol1,TWS2,TWS2mean,FLOODEDsum)
                				
				iTchuva2=iT
				iTclim=iT
				iTvar=iT
				iTfor=iT
				iTprev=iT
					
				! Prepara ensemble:
				! Ensemble Streamflow Prediction:
				! Corrige o iT:
				IANO=Anoini_ESP+iEns-1
				JDIA=iT+IDINI-1
				CALL CALDAT(JDIA,IMES,IDIA,j)
				JDIA=JULDAY(IMES,IDIA,IANO)
				! ESP_flag = Tipo de ensemble de forcantes:
				! 1 - Precipitação + Vars Meteorologicas
				! 2 - Precipitação
				! 3 - Vars Meteorologicas
				iTchuva2=iT
				iTclim=iT
				iTchuva2=JDIA-IDINI+1
				iTclim=JDIA-IDINI+1
				iTfor=JDIA-IDINI+1

			
				! Loop do lead time:
				ilead=0				
				do while (iTprev<iTTEMP+lead)
					iTprev=iTprev+1	!!!
					iT=iT+1
					if (iT>nT_prev) iT=1	!!!
					iTchuva2=iTchuva2+1
					if (iTchuva2>nT_prev) iTchuva2=1 ! Pega dados do primeiro ano
					iTclim=iTclim+1
					if (iTclim>nT_prev) iTclim=1 ! Pega dados do primeiro ano
					iTfor=iTfor+1
					if (iTfor>nT_prev) iTfor=1 ! Pega dados do primeiro ano
					iTvar=iTvar+1
					if (iTvar>nT_prev) iTvar=1 ! Pega dados do primeiro ano

					ilead=ilead+1
					write(*,*) iprev,iKK0,IDIAkk,IMESkk,ITTEMP,IDIA,IMES,IANO,ilead,iTprev,iTfor,iTvar,itchuva2,itclim	!!!
					! Modelo:
					JDIA=IDINI+iTchuva2-1 !VERIFICA QUAL É O DIA DO CALENDÁRIO 
					CALL CALDAT(JDIA,IMES,IDIA,IANO)
					!SUBROTINA DE LEITURA E PREPARACAO DA CHUVA
					ITCHUVA=iTchuva2
					!CALL LECHUVA
					READ(118888,REC=ITCHUVA)(P(j),j=1,NC)
					!SUBROTINA DE PREPARAÇÃO DOS DADOS CLIMATOL.
					iTtemp2=iT
					iT=iTclim
					JDIA=IDINI+iT-1 !VERIFICA QUAL É O DIA DO CALENDÁRIO 
					CALL CALDAT(JDIA,IMES,IDIA,IANO)
					TONTEM=TA
					CALL LECLIMA
					
					iT=iTtemp2
					JDIA=IDINI+iT-1 !VERIFICA QUAL É O DIA DO CALENDÁRIO 
					CALL CALDAT(JDIA,IMES,IDIA,IANO)
					!SUBROTINA DA CELULA
					CALL CELULA
					!SUBROTINA DA REDE DE DRENAGEM
					CALL REDE
					! Modelo hidrodinâmico:
                
                    ! Escreve resultados:    
                	write(9911,778) iprev,ittemp,iKK0,IDIAkk,IMESkk,ITTEMP,IDIA,IMES,IANO,ilead,iTprev,iT,iTfor,(QJ2(IHIDG(j)),j=1,NUMHIDG) 
					write(991,777) iprev,ittemp,iKK0,IDIAkk,IMESkk,ITTEMP,IDIA,IMES,IANO,ilead,iTprev,iT,iTfor,(QJ2(iC),iC=1,nC) 
					write(992,*) iprev,ittemp,iKK0,IDIAkk,IMESkk,ITTEMP,IDIA,IMES,IANO,ilead,iTprev,iT,iTfor
				enddo
				IT=ITTEMP
				iTprev=ITTEMP	
			enddo
			
		endif
		







777	FORMAT(13I7,<nC>F12.3)
778	FORMAT(13I7,<NUMHIDG>F12.3)

     
	RETURN
	END

