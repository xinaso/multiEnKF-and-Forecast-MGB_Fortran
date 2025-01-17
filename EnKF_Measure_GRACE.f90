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
    !    2019.15.01 - 15 January 2019 (By: Sly Wongchuig)    
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
    !    * Vinicius Alencar Siqueira
	!	 * Ayan Santos Fleischmann
	!	 * Sly Wongchuig Correa
    !
    !  Main Reference:
    !
    !    Rodrigo Paiva,
    !    Paiva, R.C.D. Collischonn, W., Bonnet, M.P., de Goncalves, L.G.G., Calmant St�phane, Getirana, A., da Silva J. S., 2013.
    !    Assimilating in situ and radar altimetry data into a large-scale hydrologic-hydrodynamic model for streamflow forecast 
    !    in the Amazon. Hydrol. Earth Syst. Sci., 17, 2929-2946.
    
    !    Sly Wongchuig,
    !    Thesis
    !    Porto Alegre, 2019
    !
	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !   This sub-routine creates the matrix of correlations for localization approach,
    !   perturb the observation and returns the matriz for the EnKF equations.
    !
    !
    !---------------------------------------------------------------------------------
	! ********************************  Description of main variables:  ***********************
	! iStat0=Dimension of the state vector until the correspond observed variable
    ! *Variables declarations and routines calls are all commented below.
    
	! ********************************  Description of main input files:  ***********************
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    subroutine EnKF_Measure_GRACE
	!------------------------------------------------------------------------------
	!-----------------------------------------------------------------------------
	! Declaracao de variaveis:
    USE VARS_MAIN
    use VARS_INERC
	USE EnKF_MOD
	implicit none

	! Variaveis locais:
	integer:: i,j,k,m,n,ix,jx,iStat0
    real*8:: gasdev,wran

    !----------------------------------------------------------------------

    ! Se leer� la matriz Minibacias (columnas) vs tiempo (filas):

    iStat0=FLAGVars(1)*nC*nU+FLAGVars(2)*nC+FLAGVars(3)*nC+FLAGVars(4)*nC+FLAGVars(5)*nC+FLAGVars(6)*nC+FLAGVars(7)*nC*nU+FLAGVars(8)*nC+FLAGVars(9)*nC+FLAGVars(10)*nC+FLAGVars(11)*nC+FLAGVars(12)*nC  ! Verificado 2019
    
    if (iT==1) then
        allocate(HAkf(1,1),Ykf(1),Skf(1,1),Dkf(1,1),Ekf(1,1),Erro_GRACE_t(1),D2kf(1,1),innov(1),Rkf(1,1),rho(1,1),rhoc(1,1))
    endif
    
    if (iT>=iTini_EnKF.and.iT<iTfim_EnKF) then
        
        
                if (allocated(Ykf))    deallocate(Ykf)
                if (allocated(HAkf))    deallocate(HAkf)
                if (allocated(Skf))    deallocate(Skf)
                if (allocated(Dkf))    deallocate(Dkf)
                if (allocated(Ekf))    deallocate(Ekf)
                if (allocated(Erro_GRACE_t))    deallocate(Erro_GRACE_t)
                if (allocated(D2kf))    deallocate(D2kf)
                if (allocated(innov))    deallocate(innov)
                if (allocated(Rkf))    deallocate(Rkf)
                if (allocated(rho))    deallocate(rho)
                if (allocated(rhoc))    deallocate(rhoc)
        
        
        ! Verifica postos que tem dados:
        nObs_t=0     
        do iObs=1,nObs_GRACE
            if (Obs_GRACE(iObs)/=-9999.0) then
            
                nObs_t=nObs_t+1
            endif
        enddo        
        
        if (nObs_t<2) then
            allocate(HAkf(1,nEns),Ykf(1),Skf(1,nEns),Dkf(1,nEns),Ekf(1,nEns),Erro_GRACE_t(1),D2kf(1,nEns),innov(1),Rkf(1,1),rho(nStatVar,1),rhoc(1,1))   !Julho
        else    
        
            allocate(Ykf(nObs_t),HAkf(nObs_t,nEns),Skf(nObs_t,nEns),Dkf(nObs_t,nEns),Ekf(nObs_t,nEns),Erro_GRACE_t(nObs_t),rho(nStatVar,nObs_t),rhoc(nObs_t,nObs_t))
            allocate(D2kf(nObs_t,nEns),innov(nObs_t),Rkf(nObs_t,nObs_t))
        
        endif
            
            ! Armazena dados no vetor de observacoes Y e computa HA:
        
            i=0
            do iObs=1,nObs_GRACE
                if (Obs_GRACE(iObs)/=-9999.0) then
                    i=i+1
                    Ykf(i)=Obs_GRACE(iObs) + MEAN_GRACE(iObs)             ! Se suma el promedio del OL   2019
                    iC=XminiObs_GRACE(iObs)
                    iCGRACE=XsubObs_GRACE(iObs)
                    
                    IF (Local_INDEX==1) THEN
                        do m=1,FLAGVars(1)*nU+FLAGVars(2)+FLAGVars(3)+FLAGVars(4)+FLAGVars(5)+FLAGVars(6)+FLAGVars(7)*nU+FLAGVars(8)+FLAGVars(9)+FLAGVars(10)+FLAGVars(11)+FLAGVars(12)
                            do n=1,nC
                            !rho((m-1)*nC+n,i)=Corr_EnKF(n,iC)    ! Extraccion de matriz rho (p) a partir de Corr_EnKF(yMini,xMini)               !Julho
                            rho((m-1)*nC+n,i)=1
                            enddo                                ! CONVERTIR DIMENSIONES DE "mini" PARA "nStatVar"
                        enddo
                        
                        do IGC=1,NGC
                            if (IGC==iCGRACE) THEN
                                rho((m-1)*nC+n+IGC,i)=1
                            else
                                rho((m-1)*nC+n+IGC,i)=0
                            endif
                        enddo
                        
                        ! Adicionar la ultima variable de estado FLOODEDsum 2019
                        do IFC=1,NFC
                            rho((m-1)*nC+n+IFC,i)=1
                        enddo
                        
                        
                    ENDIF
                    
                    !! Matriz de correlacion GRACE
                    !    do m=1,FLAGVars(1)*nU+FLAGVars(2)+FLAGVars(3)+FLAGVars(4)+FLAGVars(5)+FLAGVars(6)+FLAGVars(7)*nU+FLAGVars(8)+FLAGVars(9)+FLAGVars(10)+FLAGVars(11)+FLAGVars(12)
                    !        do n=1,nC
                    !            rhoGRACE((m-1)*nC+n,i)=CorrGRACE_EnKF(n,iC)    ! Extraccion de matriz rho (p) a partir de CorrGRACE_EnKF(yMini,xMini)               !2019
                    !        enddo                                ! CONVERTIR DIMENSIONES DE "mini" PARA "nStatVar"
                    !    enddo
                    !    do IGC=1,NGC
                    !        if (IGC==iCGRACE) THEN
                    !            rhoGRACE((m-1)*nC+nC+IGC,i)=1
                    !        else
                    !            rhoGRACE((m-1)*nC+nC+IGC,i)=0
                    !        endif
                    !    enddo
                        
                    
                    HAkf(i,:)=Akf(iStat0+iCGRACE,:)
                    ! Usa valores relativos, assim todas vazoes vao ter o mesmo peso:
                    Erro_GRACE_t(i)=abs(ErroGRACE)
                endif
            enddo        
                   
            
        IF (Local_INDEX==1) THEN
            i=0
            do iObs=1,nObs_GRACE
                if (Obs_GRACE(iObs)/=-9999.0) then
                    i=i+1
                    !iC=XminiObs_GRACE(iObs)
                    iCGRACE=XsubObs_GRACE(iObs)
                    ! Crear la version compacta de rho, llamada rhoc (nObs_t,nObs_t)
                    rhoc(i,:)=rho((m-1)*nC+nC+iCGRACE,:)
                ENDIF 
            ENDDO
        ENDIF
        
        !! Matriz de correlacion GRACE
        !    i=0
        !    do iObs=1,nObs_GRACE
        !        if (Obs_GRACE(iObs)/=-9999.0) then
        !            i=i+1
        !            iC=XminiObs_GRACE(iObs)
        !            iCGRACE=XsubObs_GRACE(iObs)
        !            ! Crear la version compacta de rho, llamada rhoc (nObs_t,nObs_t)
        !            rhocGRACE(i,:)=rhoGRACE((m-1)*nC+n+iCGRACE,:)
        !        ENDIF 
        !    ENDDO
        !
        !IF (Local_INDEX==1) THEN
        !  do j=1,nObs_t                                              !2019
        !        do k=1,nObs_t
        !            rhoc(j,k) = rhocGRACE(j,k)*rhoc(j,k)
        !        enddo
        !  enddo
        !  do j=1,nStatVar                                              !2019
        !        do k=1,nObs_t
        !            rho(j,k) = rhoGRACE(j,k)*rho(j,k)
        !        enddo
        !  enddo
        !ELSE
        !    rhoc=rhocGRACE
        !    rho=rhoGRACE
        !ENDIF
        
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Generate matrix of correlations
            IF (Local_INDEX==0) THEN
                do j=1,nObs_t                                               !2019
                    do k=1,nObs_t
                        rhoc(j,k) = 1
                    enddo
                enddo
                do j=1,nStatVar                                             !2019
                    do k=1,nObs_t
                        rho(j,k) = 1
                    enddo
                enddo
            endif
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>            
 
		    ! Gerar matriz de perturba��es nas observa��es:
		    do iEns=1,nEns
			    !Gera perturba�oes:
			    do iObs=1,nObs_t
				    ! Gera n�mero aleat�rio com dist. normal, m�dia zero e variancia 1. w ~ N(0,1)  
				    wran=gasdev(seed)
				    ! Gera vetor de erros
				    Ekf(iObs,iEns)=wran*Erro_GRACE_t(iObs)
			    enddo
			    ! Computa observa��es perturbadas:
			    D2kf(:,iEns)=Ykf+Ekf(:,iEns)
            enddo
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            
   		    ! Computa matriz de inova��o D':
		    Dkf=D2kf-HAkf
            
		    ! Computa matriz S=HA'=HA-media(HA) (desvios em relacao a m�dia do ensemble):
		    do iObs=1,nObs_t
		        Skf(iObs,:)=HAkf(iObs,:)-sum(HAkf(iObs,:))/nEns
		    enddo
		    
		    ! Computa innovacao m�dia:
            do iObs=1,nObs_t
		        innov(iObs)=Ykf(iObs)-sum(HAkf(iObs,:))/nEns
		    enddo
            ! Gera matriz de covariancia:
            Rkf=0.0
		    do iObs=1,nObs_t
                Rkf(iObs,iObs)=(abs(Ykf(iObs))*Erro_GRACE_t(iObs))**2
            enddo
    endif
    
	end subroutine EnKF_Measure_GRACE