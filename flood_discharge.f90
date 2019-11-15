	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This subroutine calculates the flow for each catchment from Inertial equation   (Inertial version).
    !
    !
    ! Usage:
    !
    ! *
    !
    ! uses modules, functions, and subroutines
    !
    ! * USE VARS_MAIN
    ! * USE VARS_INERC (only to Inertial version)
    !
    ! opens
    !
    ! * no files are created in this routine
    !
    ! reads
    !
    ! * no files are created in this routine
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
    ! 2015.07.06 - 07 June 2015 (by Paulo Pontes)
	! 2016.07.17 - 17 July 2016 (by Vinícius Siqueira)
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
    !    * Vinícius Alencar Siqueira 
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

	subroutine flood_discharge

	!--------------------------------------------------------------------------------------
	! Variables and parameters:
	use VARS_INERC
	use VARS_MAIN
	implicit none
    INTEGER K,KHID 					!indexes and counters
	!-------------------------------------------------------------------------------------
    
	!Start catchment loop
	!$OMP PARALLEL NUM_THREADS(8)
    !$OMP DO PRIVATE(y1,y2,z1,z2,iCJus,hflow,dxflow,bflow,xMan,q0,Sflow,q)
	do iC=1,nC

        ! Manual controls for especialized calibration	
        IB=IBAC(iC) 
		IF((IB<SUBini).OR.(IB>SUBfim))CYCLE 

            ! Bottom level and water level of IC catchment:
            z1=ZTAB(1,iC)
            y1=Hfl(iC)+z1    
            
            ! Bottom level and water level of downstream IC catchment:
            iCJus = CELJUS(iC)
            
			!Test for basin outlet (iCJus = -1)
            if(iCJus == -1)then
						                    
                !This modification is due to the (almost) even lengths resulting from the Reach-Length segmentation # VAS                
                
                if(WSslope<=0.000000000001)then !If slope = 0.00, water is reaching Oceans/Endorreic Lakes -> Fixed level condition
                    z2=z1
                    y2=Dble(HRIO(IC))+z2
                else
                    !Using downstream boundary condition from InfoMGB.sim
                    z2=z1-WSslope*DBLE(SRIO(IC)*1000.)
                    y2=y1-WSslope*DBLE(SRIO(IC)*1000.)                                
                end if
                
                
            else
			    !Computes downstream water elevation
                z2=ZTAB(1,iCJus)
                y2=Hfl(iCJus)+z2
            endif
            
            ! Calculates the hflow variable:
            hflow=max(y2,y1)-max(z2,z1)
            hflow=max(hflow,0.0)
            
           
            if(iCJus /= -1)then
                dxflow=DBLE(SRIO(IC)*1000.) + DBLE(SRIO(iCJus)*1000.)       
                dxflow=dxflow/2.
                !River width
                bflow=DBLE(BRIO(iC)) + DBLE(BRIO(iCJus))
                bflow=bflow/2.
            else
				!Water depth is computed in the center of river reach.				
				!dxflow=DBLE(SRIO(IC)*1000.) + DBLE(dxart*1000.)
                !dxflow=dxflow/2.                
                
                !This modification is due to the (almost) even lengths resulting from the Reach-Length segmentation # VAS
                !Considering the upstream length
                dxflow=DBLE(SRIO(IC)*1000.)
                bflow=DBLE(BRIO(iC))
            endif
                                
            !River manning coefficient
            xMan=nMan(iC)
            
            ! Flow in the last time-step
            q0=Q2fl(iC)/bflow ! in m2/s
                    
            ! Water table slope:
            Sflow=-(y1-y2)/dxflow
            
            ! Calculates flow from Inertial equation (m2/s) for each IC:
            if (hflow>0.0) then
                q=(q0-(g*dtflood*hflow*Sflow))/(1.+g*dtflood*hflow*xMan*xMan*abs(q0)/(hflow**(10.0/3.0)))
                q=q*bflow !(m3/s)
                
            !Froude limiter for supercritical flow conditions                
            !if ((((q/(bflow*hflow))**2.0)/(9.81*hflow))>0.81)then   !Avoid Froude values greater than 0.9 (0.81=0.9²)
                if (q>0)then                    
                    q=min(q,bflow*(hflow**1.5)*9.81**0.5) !avoid supercritical flow                    
                else
                    q=max(q,-bflow*(hflow**1.5)*9.81**0.5) !avoid supercritical flow                    
                endif
            !endif    
               
            else
                q=0.0;
            endif
            
            ! Updates variable flow inertial:
            Q2fl(iC)=q ! in m3/s
            QJ2(iC)=Q2fl(iC) ! in m3/s           
            vFlow(iC)=real(q/(bflow*hflow)) !Flow velocity
    enddo
    !$OMP END DO
	!$OMP END PARALLEL
    
    !Loop que calcula a vazão nas interconexões entre minibacias.
    if (PSEUDO_TwoD_INDEX==1) then !turn on lateral connections
                
    !PLEASE, BE CAREFUL CLEARING THESE VARIABLES! (Q2Viz and Q2face)
    Q2viz=0.0
    !Q2face=0.0

    ! !$OMP PARALLEL DO NUM_THREADS(4) DEFAULT (SHARED)
     !PRIVATE ()
    do iFACE=1,nFACE
    
            KCAT=nFACECAT1(iFACE)
            KCAT2=nFACECAT2(iFACE)
            
            ! Nível de Fundo e Nível da Água da minibacia iC:
            z1=ZTAB(1,KCAT)
            !z1=nFACEY1(iFACE)
            !z1=z1-HRIO(KCAT) 
            y1=Hfl(KCAT)+z1
            z2=ZTAB(1,KCAT2)
            !z2=nFACEY2(iFACE)
            !z2=z2-HRIO(KCAT2) 
            y2=Hfl(KCAT2)+z2
            
             !Lateral connections are set for overland flow only. Turn off to use bifurcation mode !#VAS
           if ((y2>y1).AND.(Hfl(KCAT2)>Hrio(KCAT2)))then
               !OK Water is passing through lateral connections
           elseif ((y1>y2).AND.(Hfl(KCAT)>Hrio(KCAT)))then
               !OK Water is passing through lateral connections
           else
                CYCLE !Water is not passing through lateral connections
           endif                 
            
            ! Cálculo da profundidade de escoamento:
            hflow=max(y2,y1)-max(z2,z1)
            !Limitador de fundo
!            hflow=hflow-1.0
            !Correção de valores negativos
            hflow=max(hflow,0.0)
            
            !A rotina DBLE transforma a variável de entrada em um real*8
            !Média dos dx de IC e ICJUS
            dxflow=DBLE(nFACEDX(iFACE))       !Verificar se precisa de um limitador do dx
            
            !Variables below were moved to infoMGB.sim
            !bflow_overland=10.0 !Width for connections                                    			
            !xMan_overland=0.1
                
            ! Vazão no tempo anterior:
            q0=Q2face(iFACE)/bflow_overland ! em m2/s
                    
            ! Declividade da linha de água:
            Sflow=-(y1-y2)/dxflow          
                
            ! Cálculo da vazão Inercial (por unidade de largura do rio) na face de jusante da minibacia iC:
            if (hflow>0.0) then
                q=(q0-(g*dtflood*hflow*Sflow))/(1.+g*dtflood*hflow*xMan_overland*xMan_overland*abs(q0)/(hflow**(10.0/3.0)))                
                q=q*bflow_overland
            else
                q=0.0;
            endif        
            
            ! Calcula a nova vazão no próximo intervalo de tempo:
            Q2face(iFACE)=q ! em m3/s
            Q2viz(KCAT)=Q2viz(KCAT)- Q2face(iFACE)
            Q2viz(KCAT2)=Q2viz(KCAT2)+ Q2face(iFACE)             
                        
    enddo 
!    !$OMP END PARALLEL DO
    endif

	endsubroutine


