!       ****************
!       PROGRAM RICHARDS
!       ****************
!       by G,Gottardi and M.Venutelli
!       Univesity of Bologna Italy.
!       updated January 1994
!       updated Juin 2016 to Fortran 90 using Code::Blocks (Jeff Norville)
!
!       DOS VERSION
!
!       program to integrate the three forms of Richard infiltration
!       equation.
!
!       KODE=1 (MFD)  (mixed_form finite_difference method)
!       KODE=2 (HFD)  (head_form finite_differences method)
!       KODE=3 (HFE)  (head_form finite element method)
!       KODE=4 (TFD)  (teta_form finite difference method)
!       KODE=5 (TFE)  (teta_form finite element method)
!--------------------------------------------------------------------------
!       SUBROUTINES: ASSFD, ASSFE, ASST, CMNT, TRIDAG
!       FUNCTIONS:   F_KT, FUN_K, FUN_C, FUN_H, FUN_T
!--------------------------------------------------------------------------

program main

        IMPLICIT REAL*8(A-H,O-Z)
        CHARACTER*1 ANS,COM
        CHARACTER*20 F14,F16
        CHARACTER*80 TITLE
        PARAMETER(COM=';')
        PARAMETER(NMAX=500)
        REAL*8 KM,KS
!
        INTEGER T1,T2
!
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
        COMMON/C04/KM(NMAX),IMAT(NMAX),DT,DZ,DZ2,HB1,HBN, KODE,KREST,KBOUN,KBLOCK,RATE
        DIMENSION A(NMAX),B(NMAX),C(NMAX),R(NMAX),Z(NMAX), HO(NMAX),H(NMAX),U(NMAX)
!

    1   WRITE(*,1001)
        READ(*,1010) F14
        WRITE(*,1020)
        READ(*,1010) F16
        WRITE(*,1060)
        READ(*,1010) ANS
        IF(ANS.EQ.'E') STOP
        IF(ANS.EQ.'N') GOTO 1
!
!
1001    FORMAT(/,1X,' INPUT FILE NAME    > ',$)
1010    FORMAT(A)
1020    FORMAT(/,1X,' OUTPUT FILE NAME   > ',$)
1060    FORMAT(/,1X,' OK ?  (Y/N/E)      > ',$)
!
!        CALL TIME(T1)    !Error!
!
!
!--- OPENING OF FILES
!
        OPEN(14,FILE=F14,STATUS='OLD',FORM='FORMATTED')
        OPEN(15,STATUS='SCRATCH',FORM='FORMATTED')
        OPEN(16,FILE=F16,STATUS='NEW',FORM='FORMATTED')

!
        WRITE(*,*) '**** please wait program RICHARDS is running ***'
        CALL CMNT(14,15)
!
!
!*************************
!       INPUT OF DATA
!*************************
!
      WRITE(16,2000)

2000 FORMAT(T25,'********************************'/ &
     '*     UNIVERSITY OF BOLOGNA    *', T25, &
     '*  INSTITUTE OF MINING SCIENCE *', T25, &
     '********************************'////)

     WRITE(16,2005)
2005 FORMAT(T25,'PROGRAM RICHARDS',/,T25,'****************',//)

!        WRITE(16,*)'****************'
!        WRITE(16,*)'PROGRAM RICHARDS'
!        WRITE(16,*)'****************'


        READ(15,'(A)') TITLE
        WRITE(16,'(A)') TITLE
        WRITE(16,'(//)')
        READ(15,*) NNODE,NMAT,NLAY,KODE,KREST,KBOUN,KBLOCK

        WRITE(16,1000) NNODE,NMAT,NLAY,KODE,KREST,KBOUN,KBLOCK
 1000   FORMAT(T5,'NUMBER OF NODES                 (NNODE)...:',I3,/,T5, &
     'NUMBER OF SOIL MATERIALS        (NMAT)....:',I3,/,T5, &
     'NUMBER OF LAYERS (NLAY)....:',I3,/,T5,'KODE   OF SOLUTION METHOD &
     &(KODE)....:',I3,/,T5,'KODE   FOR RESTART READING      (KREST)...:',I3,/,T5, &
     'KODE   FOR BOUNDARY CONDITIONS  (KBOUN)...:',I3,/,T5, &
     'KODE FOR COMP.INTEBLOCK COND.   (KBLOCK)..:',I3,//)

!       WRITE(16,*) 'NUMBER OF NODES                 ',NODE
!       WRITE(16,*) 'NUMBER OF SOIL MATERIALS        ',NMAT
!       WRITE(16,*) 'NUMBER OF LAYERS                ',NLAY
!       WRITE(16,*) 'KODE   OF SOLUTION METHOD       ',KODE
!       WRITE(16,*) 'KODE   FOR RESTART READING      ',KREST
!       WRITE(16,*) 'KODE   FOR BOUNDARY CONDITIONS  ',KBOUN
!       WRITE(16,*) 'KODE FOR COMP.INTEBLOCK COND.   ',KBLOCK



        WRITE(16,1110)
1110   FORMAT(T30,'SOIL PARAMETERS',/,T30,'***************',//,1X,T5,'IM',T10,'KS(IM)',T21, &
     'PA(IM)',T30,'ALFA(IM)',T40,'BETA(IM)',T50,'GAMMA(IM)',T60, &
     'TETAS(IM)',T70,'TETAR(IM)'/)
!

!       WRITE(16,*) 'SOIL PARAMETERS'
!       WRITE(16,*)



!-----READING MATERIAL PARAMETERS
!

        DO 5 I=1,NMAT
          READ(15,*)IM, KS(IM),PA(IM),ALFA(IM),BETA(IM),GAMMA(IM),TETAS(IM),TETAR(IM)
          WRITE(16,1055)IM, KS(IM),PA(IM),ALFA(IM),BETA(IM),GAMMA(IM),TETAS(IM),TETAR(IM)
5       CONTINUE

 1055   FORMAT(1X,I3,3(1P,E11.3),4(0PF9.3))


        READ(15,*) DTMIN,DTPRINT,TPRINT,ITERMX,NSTEPS,TINIZ,TFINAL,EPS

        READ(15,*)ZMIN,DZ,DT,DTMAX,DMUL,DDIV,NLIM
        WRITE(16,1200)DTMIN,DTPRINT,TPRINT,ITERMX,NSTEPS,TINIZ,TFINAL,EPS,ZMIN,DZ,DT,DTMAX,DMUL,DDIV,NLIM

  1200   FORMAT(//,T30,'RUN CONTROL DATA',/,T30,'****************',//,T5,'DTMIN:',E10.2,T25, &
       'DTPRINT:',E10.3,T45,'TPRINT:',E10.3,T66,'ITERMX:',I5,//T5,'NSTEPS:',I9,T25, &
       'TINIZ:',E10.3,T45,'TFINAL:',E10.3,T66,'EPS:',E10.3,//T5,'ZMIN: ',F8.2,T25, &
       'DZ: ',E10.2,T45,'DT:',E10.3,T66,'DTMAX: ',E10.3//T5,'DMUL:',F5.2,T25,'DDIV:',F5.2,T45,'NLIM:'I5///)
!
!------NODE COORDINATES
!
        IF(KREST.EQ.0) THEN

          Z(1)=ZMIN
          DO 10 I=2,NNODE
          Z(I)=Z(I-1)+DZ
   10   CONTINUE
!
!------INITIAL VALUES
!
        NP=0
        DO 25 N=1,NNODE
         IF(NP-N) 15,25,20
   15    READ(15,*) NP,HO(NP)
         IF(N.EQ.NP) GOTO 25
   20   HO(N)=HO(NP)
   25   CONTINUE
!
!-------BLOCK MATERIAL INDICES
!
        NP=0
        DO 40 N=1,NNODE
         IF(NP-N) 30,40,35
   30    READ(15,*) NP,IMAT(NP)
         IF(N.EQ.NP) GOTO 40
   35   IMAT(N)=IMAT(NP)
   40   CONTINUE

!
        IF(KODE.GE.4) THEN
         DO I=1,NNODE
          HO(I)=FUN_T(HO(I),IMAT(I))
         ENDDO
         ENDIF
!
           VWI=0.D0
           CWIN=0.D0
           CWOU=0.D0
        DO 50 I=1,NNODE
             IF(KODE.LE.3) VWI=VWI+FUN_T(HO(I),IMAT(I))*DZ
             IF(KODE.GE.4) VWI=VWI+HO(I)*DZ
   50   CONTINUE
!
!-------INPUT FOR RESTART
!
        ELSE IF(KREST.EQ.1) THEN

          DO 45 I=1,NNODE
                READ(15,*) NP,IMAT(NP), Z(NP),HO(NP),TET
   45    CONTINUE

           READ(15,*) VWI,CWIN, CWOU   !Error! (missing comma)

        ENDIF

         IF (KBOUN.EQ.1) READ(15,*) RATE
!
!------ PRINT INITIAL DATA
!
!        WRITE(16,1061)
! 1061   FORMAT(T30,'INITIAL CONDITION TABLE',/,
!     &         T30,'***********************',//)

        WRITE(16,*) 'INITIAL CONDITION TABLE'

        WRITE(16,1070)

        DO 55 I=1,NNODE
          H(I)=HO(I)
          IF(KODE.LE.3) THEN
          WRITE(16,1100) I,IMAT(I),Z(I),HO(I),FUN_T(HO(I),IMAT(I)),FUN_K(HO(I),IMAT(I))
        ELSE IF(KODE.GE.4) THEN
          WRITE(16,1100) I,IMAT(I),Z(I),FUN_H(HO(I),IMAT(I)),HO(I),F_KT(HO(I),IMAT(I))
        ENDIF

   55   CONTINUE
 1070   FORMAT(1X,T3,'NODE',T10,'IMAT',T26,'Z',T45,'HO',T64,'TETA',T83,'K'/)
        WRITE(16,1080) VWI
        IF(KBOUN.EQ.1) WRITE(16,1085) RATE
 1080   FORMAT(//,1X,'INITIAL VOLUME OF WATER : ',1P,E12.4)
 1085   FORMAT(   1X,'PREFIXED RATE           : ',1P,E12.4)
 1100   FORMAT(1X,2I5,4(1P,E20.6))

        HB1=0.D0
        HBN=0.D0
        NNODE1=NNODE-1
        TIME=TINIZ
        DTO=DT
        DZ2=DZ*DZ
!
!------MAIN LOOP
!
        DO 60 N=1,NSTEPS
!
!------ASSEMBLE MATRIX AND RHS VECTOR
!
        ITER=1
        IFLAG=0
   65   IF(KODE.LE.2) CALL ASSFD(HO,H,A,B,C,R,NNODE)
        IF(KODE.EQ.3) CALL ASSFE(HO,H,A,B,C,R,NNODE)
        IF(KODE.GE.4) CALL ASST(HO,H,A,B,C,R,NNODE)
!
!-------SOLVE TRIDIAGONAL SET OF EQUATIONS
!
        CALL TRIDAG(A,B,C,R,U,NNODE)
!
        DHMAX=1.D-30
        DO 70 I=1,NNODE
          UH=U(I)/H(I)
          IF(DABS(UH).GT.DHMAX) DHMAX=DABS(UH)
   70 CONTINUE
!
!-------CONVERGENCE TEST
!
        IF(DHMAX.GT.EPS.AND.ITER.LT.ITERMX) THEN
      DO 75 I=1,NNODE
         H(I)=H(I)+U(I)
   75 CONTINUE
         ITER=ITER+1
         GOTO 65

!
!-------LOWER THE TIME STEP
!
        ELSE IF(DHMAX.GT.EPS.AND.ITER.GE.ITERMX.AND.DT.GE.DTMIN) THEN
        DT=DT*DDIV
        IFLAG=IFLAG+1
        DO 80 I=1,NNODE
           H(I)=H(I)+U(I)
           H(I)=HO(I)+(H(I)-HO(I))*DT/DTO
!          H(I)=HO(I)
   80 CONTINUE
        ITER=1
        GOTO 65
!
!------ABNORMAL EXIT
!
        ELSE IF(DHMAX.GT.EPS.AND.ITER.GE.ITERMX.AND.DT.LE.DTMIN) THEN
        WRITE(*,*) 'CONVERGENCE NOT REACHED'
!        PAUSE
        STOP

!
!------CONVERGENCE REACHED
!
        ELSE IF(DHMAX.LE.EPS) THEN
!
        TIME=TIME+DT

        WRITE(*,1320) TIME, ITER,DT,DHMAX
 1320   FORMAT(1X,'TIME: ',1P,E12.3,3X,'ITER: ',I4,3X,'DT: ',1P,E12.3,3X,'DHMAX: ',1P,E12.3)
        IF(TIME.GE.TPRINT) THEN
!        CALL TIME(T2)    !Error!
        WRITE(16,1065) TIME,T2-T1
 1065   FORMAT(//1X,T15,'REPORT AT TIME: ',1P,E13.3,T50,'CPU_TIME:',I8,/,T15,'**************',//)

!
!------UPDATE DATA
!

        DO 85 I=1,NNODE
           H(I)=H(I)+U(I)
   85 CONTINUE
!
        ENDIF
!
!
!-------MASS BALANCE
!
        N1=NNODE-1
        IF(KODE.LE.3) THEN
          QWIN=KM(1)*((H(1)-H(2))/DZ+1.D0)
          QWOU=KM(N1)*((H(N1)-H(NNODE))/DZ+1.D0)
        ELSE IF (KODE.GE.4) THEN
         QWIN=KM(1)*((FUN_H(H(1),IMAT(1))- FUN_H(H(2),IMAT(2)))/DZ+1.D0)
         QWOU=KM(N1)*((FUN_H(H(N1),IMAT(N1))- FUN_H(H(NNODE),IMAT(NNODE)))/DZ+1.D0)
        ENDIF
!
!------UPDATE CUMULATIVE VOLUMES OF INPUT AND OUTPUT WATER
!
        CWIN=CWIN+QWIN*DT
        CWOU=CWOU+QWOU*DT
!
        IF(TIME.GE.TPRINT) THEN

         VWP=0.D0
       DO 90 I=1,NNODE
         IF(KODE.LE.3) VWP=VWP+FUN_T(H(I),IMAT(I))*DZ
         IF(KODE.GE.4) VWP=VWP+H(I)*DZ
   90  CONTINUE

        RATIO=(VWP-VWI)/(CWIN-CWOU)
        BALER=100.D0*(1.D0-RATIO)
        WRITE(16,1226) QWIN, CWIN,QWOU,CWOU,VWI,VWP, BALER,N
 1226  FORMAT(/1X,'WATER ENTERED IN STEP:',T25,1P,E12.4,T40, &
     'CUM. WATER ENTERED:',T65,1P,E12.4,/1X, &
     'WATER OUT IN THE STEP:',T25,1P,E12.4,T40, &
     'CUM. WATER OUT:',T65,1P,E12.4,/1X, &
     'INITIAL WATER VOLUME:',T25,1P,E12.4,T40, &
     'ACTUAL WATER VOLUME: ',T65,1P,E12.4,/1X, &
     'WATER BALANCE ERROR %:',T25,1P,E12.4,T40, &
     'NUMBER OF TIME STEPS :',T65,I9,//)
!
!-----PRINT RESULTS
!
        WRITE(16,1325)
 1325   FORMAT(1X,T8,'NODE',T14,'IMAT',T22,'Z',T38,'H',T58,'T',/)

       DO 95 I=1,NNODE

           IF(KODE.LE.3)WRITE(16,1220)I,IMAT(I), Z(I),H(I),FUN_T(H(I),IMAT(I))
           IF(KODE.GE.4)WRITE(16,1220)I,IMAT(I), Z(I),FUN_H(H(I),IMAT(I)),H(I)
   95  CONTINUE

 1220   FORMAT( 5X,I5,I5, F8.2,T25,2(1P,E20.6))


        TPRINT=TPRINT+DTPRINT
        ENDIF

        IF(TIME.GE.TFINAL) GOTO 110
        DTO=DT
        IF(ITER.LE.NLIM.AND.IFLAG.EQ.0) DT=DT*DMUL
        IF(DT.GT.DTMAX) DT=DTMAX
        IF(TIME+DT.GT.TPRINT) DT=TIME+DT-TPRINT
!
!-------ESTRAPOLATE
!
      DO 100 I=1,NNODE
               HS=H(I)+(H(I)-HO(I))*DT/DTO
               HO(I)=H(I)
               H(I)=HS
  100 CONTINUE

        ENDIF

   60   CONTINUE
!
!------ NORMAL EXIT
!
 1250   FORMAT(2X,5E12.4/)
 1260   FORMAT(2X,3(1P,E25.16))
 1300   FORMAT(2X,'STEP ='I5,5X,'X =',E16.7,/4(1P,E25.14))
  110   CLOSE(14)
        CLOSE(16)

        STOP
    END



!       *********************************
        SUBROUTINE ASSFD(HO,H,A,B,C,U,N)
!       *********************************
!
!       ASSEMBLES TRIDIAGONAL MATRIX AND RIGHT SIDE VECTOR
!       FOR  THE MFD AND HFD METHODS.
!
        PARAMETER(NMAX=500)
        IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 KS,KM
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
        COMMON/C04/KM(NMAX),IMAT(NMAX),DT,DZ,DZ2,HB1,HBN,KODE,KREST,KBOUN,KBLOCK,RATE
        DIMENSION H(1),HO(1),A(1),B(1),C(1),U(1)
        DIMENSION FK(NMAX)
!
!

        N1=N-1
        DO 10 I=1,N
          FK(I)=FUN_K(H(I),IMAT(I))
   10   CONTINUE

        DO 15 I=1,N1
          IF(KBLOCK.EQ.1) KM(I)=0.5D0*(FK(I)+FK(I+1))
          IF(KBLOCK.EQ.2) KM(I)=2.D0*(FK(I)*FK(I+1))/(FK(I)+FK(I+1))
          IF(KBLOCK.EQ.3) KM(I)=SQRT(FK(I)*FK(I+1))
          IF(KBLOCK.EQ.4) THEN
             IF(H(I).GE.H(I+1)) KM(I)=FK(I)
             IF(H(I).LT.H(I+1)) KM(I)=FK(I+1)
          ENDIF
   15   CONTINUE

        DO 20 I=2,N1
         CC=FUN_C(H(I),IMAT(I))
         A(I)=-KM(I-1)/DZ2
         B(I)=CC/DT+(KM(I-1)+KM(I))/DZ2
         C(I)=-KM(I)/DZ2
         U(I)=(KM(I-1)*(H(I-1)-H(I))+KM(I)*(H(I+1)-H(I)))/DZ2-(KM(I)-KM(I-1))/DZ
         IF(KODE.EQ.1) THEN
            FT=FUN_T(H(I),IMAT(I))
            FTO=FUN_T(HO(I),IMAT(I))
            U(I)=U(I)-(FT-FTO)/DT
          ENDIF
         IF(KODE.EQ.2) U(I)=U(I)-CC*(H(I)-HO(I))/DT
   20   CONTINUE
!
!------CONSTANT HEAD BOUNDARY CONDITIONS
!
       IF(KBOUN.EQ.0) THEN
         U(1)=HB1
         U(2)=U(2)-A(2)*HB1
         U(N1)=U(N1)-C(N1)*HBN
         U(N)=HBN
         A(2)=0.D0
         A(N)=0.D0
         B(1)=1.D0
         B(N)=1.D0
         C(1)=0.D0
         C(N1)=0.D0
         RETURN
!
!------CONSTANT RATE BOUNDARY CONDITIONS
!
        ELSE IF(KBOUN.EQ.1) THEN

           U(N)=HBN
           B(N)=1.D0
           A(N)=0.D0
           U(N1)=U(N1)-C(N1)*HBN
           C(N1)=0.D0
           CC=FUN_C(H(1),IMAT(1))
           B(1)=CC/DT+KM(1)/DZ2
           C(1)=-KM(1)/DZ2
           IF(KODE.EQ.1) THEN
                FT=FUN_T(H(1),IMAT(1))
                FTO=FUN_T(HO(1),IMAT(1))
              U(1)=KM(1)*(H(2)-H(1))/DZ2-KM(1)/DZ-(FT-FTO)/DT+RATE/DZ
           ELSE IF(KODE.EQ.2) THEN
                U(1)=KM(1)*(H(2)-H(1))/DZ2-KM(1)/DZ-CC*(H(1)-HO(1))/DT+RATE/DZ

           ENDIF

          ENDIF
         RETURN

        END
!       *********************************
        SUBROUTINE ASST(HO,H,A,B,C,U,N)
!       *********************************
!
!       ASSEMBLES TRIDIAGONAL MATRIX AND RIGHT SIDE VECTOR
!       FOR THE TFD AND TFE METHODS
!
        PARAMETER(NMAX=500)
        IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 KS,KM
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
        COMMON/C04/KM(NMAX),IMAT(NMAX),DT,DZ,DZ2,HB1,HBN,KODE,KREST,KBOUN,KBLOCK,RATE
        DIMENSION H(1),HO(1),A(1),B(1),C(1),U(1)
        DIMENSION CC(NMAX),DM(NMAX),FK(NMAX),HT(NMAX)
!
!
        US=1.D0/6.D0
        DST=2.D0/3.D0

        N1=N-1
        DO 5 I=1,N
          FK(I)=F_KT(H(I),IMAT(I))
          HT(I)=FUN_H(H(I),IMAT(I))
          CC(I)=FUN_C(HT(I),IMAT(I))
    5   CONTINUE

        DO 10 I=1,N1
          P1=FK(I)
          P2=FK(I+1)
          R1=FK(I)/CC(I)
          R2=FK(I+1)/CC(I+1)

          IF(KBLOCK.EQ.1) KM(I)=0.5D0*(P1+P2)
          IF(KBLOCK.EQ.1) DM(I)=0.5D0*(R1+R2)

          IF(KBLOCK.EQ.2) KM(I)=2.D0*P1*P2/(P1+P2)
          IF(KBLOCK.EQ.2) DM(I)=2.D0*R1*R2/(R1+R2)

          IF(KBLOCK.EQ.3) KM(I)=SQRT(P1*P2)
          IF(KBLOCK.EQ.3) DM(I)=SQRT(R1*R2)

          IF(KBLOCK.EQ.4) THEN

            IF(H(I).GE.H(I+1)) THEN
                KM(I)=P1
                DM(I)=R1
            ELSE
                KM(I)=P2
                DM(I)=R2
            ENDIF

           ENDIF

   10    CONTINUE
        IF(KODE.EQ.4) THEN
          DO 20 I=2,N1
             A(I)=-DM(I-1)/DZ2
             B(I)=1.D0/DT+(DM(I-1)+DM(I))/DZ2
             C(I)=-DM(I)/DZ2
             U(I)=(DM(I-1)*(H(I-1)-H(I))+DM(I)*(H(I+1)-H(I)))/DZ2-(KM(I)-KM(I-1))/DZ-(H(I)-HO(I))/DT
   20   CONTINUE
        ELSE IF(KODE.EQ.5) THEN
             DO 30 I=2,N1
             A(I)=US/DT-DM(I-1)/DZ2
             B(I)=DST/DT+(DM(I-1)+DM(I))/DZ2
             C(I)=US/DT-DM(I)/DZ2
             U(I)=(DM(I-1)*(H(I-1)-H(I))+DM(I)*(H(I+1)-H(I)))/DZ2- &
     &            (KM(I)-KM(I-1))/DZ-US*(H(I-1)-HO(I-1))/DT- &
     &            DST*(H(I)-HO(I))/DT-US*(H(I+1)-HO(I+1))/DT


   30   CONTINUE
        ENDIF


!
!------CONSTANT HEAD BOUNDARY CONDITIONS
!
       IF(KBOUN.EQ.0) THEN
         U(1)=HB1
         U(2)=U(2)-A(2)*HB1
         U(N1)=U(N1)-C(N1)*HBN
         U(N)=HBN
         A(2)=0.D0
         A(N)=0.D0
         B(1)=1.D0
         B(N)=1.D0
         C(1)=0.D0
         C(N1)=0.D0
         RETURN
!
!-------CONSTANT RATE BOUNDARY CONDITIONS
!
         ELSE IF(KBOUN.EQ.1) THEN

          U(N)=HBN
          B(N)=1.D0
          A(N)=0.D0
          U(N1)=U(N1)-C(N1)*HBN
          C(N1)=0.D0
          B(1)=1.D0/DT+DM(1)/DZ2
          C(1)=-DM(1)/DZ2
          IF(KODE.EQ.4)U(1)=DM(1)*(H(2)-H(1))/DZ2-KM(1)/DZ-(H(1)-HO(1))/DT+RATE/DZ
          IF(KODE.EQ.5)U(1)=DM(1)*(H(2)-H(1))/DZ2-KM(1)/DZ-DST*(H(1)-HO(1))/DT-US*(H(2)-HO(2))/DT+RATE/DZ

         RETURN
         ENDIF
        END
!       *********************************
        SUBROUTINE ASSFE(HO,H,A,B,C,U,N)
!       *********************************
!
!       ASSEMBLES TRIDIAGONAL MATRIX AND RIGHT SIDE VECTOR
!       THE HFE METHOD.
!
        PARAMETER(NMAX=500)
        IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 KS,KM,K
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
        COMMON/C04/KM(NMAX),IMAT(NMAX),DT,DZ,DZ2,HB1,HBN,KODE,KREST,KBOUN,KBLOCK,RATE
        DIMENSION H(1),HO(1),A(1),B(1),C(1),U(1)
        DIMENSION CC(NMAX),CTM(NMAX),CT(NMAX),FNK(NMAX)
!
!
        UD=1.D0/12.D0

        N1=N-1

        DO 10 I=1,N
         FNK(I)=FUN_K(H(I),IMAT(I))
         CC(I)=FUN_C(H(I),IMAT(I))
   10   CONTINUE

        DO 20 I=1,N1
          KM(I)=0.5D0*(FNK(I)+FNK(I+1))
          CTM(I)=UD*(CC(I)+CC(I+1))
   20   CONTINUE
!

        DO 30 I=2,N1
          CT(I)=UD*(CC(I-1)+6.D0*CC(I)+CC(I+1))
   30   CONTINUE
!
!------- COEFF. A(I), B(I), C(I)
!
          DO 40 I=2,N1
           A(I)=-KM(I-1)/DZ2+CTM(I-1)/DT
           B(I)=(KM(I-1)+KM(I))/DZ2+CT(I)/DT
           C(I)=-KM(I)/DZ2+CTM(I)/DT
           U(I)=(KM(I-1)*(H(I-1)-H(I))+KM(I)*(H(I+1)-H(I)))/DZ2- &
     &     (KM(I)-KM(I-1))/DZ- &
     &     CTM(I-1)*(H(I-1)-HO(I-1))/DT-CT(I)*(H(I)-HO(I))/DT- &
     &     CTM(I)*(H(I+1)-HO(I+1))/DT


   40   CONTINUE

!
!-------CONSTANT HEAD BOUNDARY CONDITIONS
!
       IF(KBOUN.EQ.0) THEN
         U(1)=HB1
         U(2)=U(2)-A(2)*HB1
         U(N1)=U(N1)-C(N1)*HBN
         U(N)=HBN
         A(2)=0.D0
         A(N)=0.D0
         B(1)=1.D0
         B(N)=1.D0
         C(1)=0.D0
         C(N1)=0.D0
         RETURN
!
!-------CONSTANT RATE BOUNDARY CONDITIONS
!
        ELSE IF(KBOUN.EQ.1) THEN

           U(N)=HBN
           B(N)=1.D0
           A(N)=0.D0
           U(N1)=U(N1)-C(N1)*HBN
           C(N1)=0.D0
           CT(1)=UD*(6.D0*CC(1)+CC(2))
           B(1)=KM(1)/DZ2+CT(1)/DT
           C(1)=-KM(2)/DZ2+CTM(1)/DT
           U(1)=KM(1)*(H(2)-H(1))/DZ2-KM(1)/DZ-CT(1)*(H(1)-HO(1))/DT-CTM(1)*(H(2)-HO(2))/DT+RATE/DZ
         RETURN
         ENDIF
        END
!       *********************
        FUNCTION F_KT (X,IM)
!       *********************
!       COMPUTES THE LINK k=k(teta)
!       X=MOISTURE CONTENT, IM=MATERIAL INDEX

        IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 KS,N,M
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
!
        IF(X.LE.TETAR(IM)) THEN
        F_KT=0.D0
        RETURN
        ELSE IF(X.GE.TETAS(IM)) THEN
        F_KT=KS(IM)
        RETURN
        ENDIF
!
        IL=IM/10+1
        GOTO (100,200,300) IL

!       SAND
!
  100   S=(X-TETAR(IM))/(TETAS(IM)-TETAR(IM))
        R=BETA(IM)/GAMMA(IM)
        A=ALFA(IM)/S-ALFA(IM)
        D=PA(IM)+A**R
        F_KT=KS(IM)*PA(IM)/D
        RETURN
!
!       YOLO CLAY
!
  200   S=(X-TETAR(IM))/(TETAS(IM)-TETAR(IM))
        BG=BETA(IM)/GAMMA(IM)
        E=(ALFA(IM)/S-ALFA(IM))**BG
        D=PA(IM)+EXP(E)
        F_KT=TETAS(IM)*PA(IM)/D
        RETURN
!
!       VAN GENUCHTEN
!
  300   N=BETA(IM)
        M=1.D0-1.D0/N
        UM=1.D0/M
        S=(X-TETAR(IM))/(TETAS(IM)-TETAR(IM))
        A=1.D0-(1.D0-S**UM)**M
        AQ=A*A
        F_KT=TETAS(IM)*S**(0.5D0)*AQ
        RETURN
        END
!       ********************
        FUNCTION FUN_K(X,IM)
!       ********************
!       COMPUTES THE LINK k=k(h)
!       X=PRESSURE HEAD, IM=MATERIAL INDEX
!
        IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 KS,M,N
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
!
!
        IF(X.GE.0.D0) THEN
        FUN_K=KS(IM)
        RETURN
        ENDIF

        IL=IM/10+1
        GOTO (100,100,200) IL

!
!       SAND AND YOLO CLAY
!
  100   FUN_K=KS(IM)*PA(IM)/(PA(IM)+DABS(X)**BETA(IM))
        RETURN
!
!       VAN GENUCHTEN
!
  200   N=BETA(IM)
        M=1.D0-1.D0/N
        B=ALFA(IM)*DABS(X)
        ALFN=B**N
        ALFN1=B**(N-1)
        RN=(1.D0-ALFN1*(1.D0+ALFN)**(-M))**2
        RD=(1.D0+ALFN)**(M/2)
        FUN_K=KS(IM)*RN/RD
        RETURN
        END
!       ********************
        FUNCTION FUN_C(X,IM)
!       ********************
!       COMPUTES THE LINK C=C(h)
!       X= PRESSURE HEAD  IM= MATERIAL INDEX
!
        IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 KS,M,N
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
!
        IF(X.GE.0.D0) THEN
        FUN_C=0.D0
        RETURN
        ELSE IF(X.LT.-1.D35) THEN
        FUN_C=0.D0
        RETURN
        ENDIF
!
        IL=IM/10+1
        GOTO (100,200,300) IL

!
!       SAND
!

  100   A=(ALFA(IM)+DABS(X)**GAMMA(IM))**2
        GAMMA1=GAMMA(IM)-1.D0
        B=(ALFA(IM)*(TETAS(IM)-TETAR(IM))*GAMMA(IM)*DABS(X)**GAMMA1)
        FUN_C=B/A
        RETURN
!
!       YOLO CLAY
!
  200   IF(X.GE.-1.D0) THEN
        FUN_C=0.D0
        RETURN
        ENDIF

        A=(ALFA(IM)+LOG(DABS(X))**GAMMA(IM))**2
        GAMMA1=GAMMA(IM)-1.D0
        B1=ALFA(IM)*(TETAS(IM)-TETAR(IM))*GAMMA(IM)
        B2=(DLOG(DABS(X)))**GAMMA1/DABS(X)
        B=B1*B2
        FUN_C=B/A
        RETURN
!
!       VAN GENUCHTEN
!
  300   N=BETA(IM)
        M=1.D0-1.D0/N
        A=ALFA(IM)*DABS(X)
        A1=A**N
        A2=A**(N-1.D0)
        RN=N*M*(TETAS(IM)-TETAR(IM))*A2*ALFA(IM)
        RD=(1.D0+A1)**(M+1.D0)
        FUN_C=RN/RD
        RETURN
        END
!       *********************
        FUNCTION FUN_H (X,IM)
!       *********************
!       COMPUTES THE LINK  h=h(teta)
!       X=MOISTURE CONTENT, IM=MATERIAL INDEX

        IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 KS,N,M
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
!
        IF(X.LE.TETAR(IM)) THEN
        FUN_H=-1.E30
        RETURN
        ELSE IF(X.GE.TETAS(IM)) THEN
        FUN_H=0.D0
        RETURN
        ENDIF
!
        IL=IM/10+1
        GOTO (100,200,300) IL
!
!       SAND
!
  100   S=(X-TETAR(IM))/(TETAS(IM)-TETAR(IM))
        A=ALFA(IM)/S-ALFA(IM)
        FUN_H=-A**(1.D0/GAMMA(IM))
        RETURN
!
!       YOLO CLAY
!
  200   S=(X-TETAR(IM))/(TETAS(IM)-TETAR(IM))
        UG=1.D0/GAMMA(IM)
        E=(ALFA(IM)/S-ALFA(IM))**UG
        FUN_H=-EXP(E)
        RETURN
!
!       VAN GENUCHTEN
!
  300   N=BETA(IM)
        M=1.D0-1.D0/N
        S=(X-TETAR(IM))/(TETAS(IM)-TETAR(IM))
        A=S**(-1.D0/M)-1.D0
        FUN_H=-A**(1.D0/N)/ALFA(IM)
        RETURN
        END

!       ********************
        FUNCTION FUN_T(X,IM)
!       ********************
!       COMPUTES THE LINK theta=theta(h)
!       X=MOISTURE CONTENT, IM=MATERIAL INDEX

        IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 KS,N,M
        COMMON/C02/KS(30),PA(30),ALFA(30),BETA(30),GAMMA(30),TETAS(30),TETAR(30)
!
        IF(X.GE.0.D0) THEN
        FUN_T=TETAS(IM)
        ELSE IF(X.LT.-1.D4) THEN
        FUN_T=TETAR(IM)
        RETURN
        ENDIF
!
        IL=IM/10+1
        GOTO (100,200,300) IL
!
!       SAND
!

  100   A=ALFA(IM)*(TETAS(IM)-TETAR(IM))
        B=ALFA(IM)+(DABS(X))**GAMMA(IM)
        FUN_T=A/B+TETAR(IM)
        RETURN
!
!       YOLO LIGHT CLAY
!
  200   IF(X.GE.-1.D0)THEN
        FUN_T=TETAS(IM)
        RETURN
        ENDIF

        A=ALFA(IM)*(TETAS(IM)-TETAR(IM))
        B=ALFA(IM)+(DLOG(DABS(X)))**GAMMA(IM)
        FUN_T=A/B+TETAR(IM)
        RETURN
!
!       VAN GENUCHTEN
!
  300   N=BETA(IM)
        M=1.D0-1.D0/N
        ALFN=(ALFA(IM)*DABS(X))**N
        RD=(1.D0+ALFN)**M
        FUN_T=(TETAS(IM)-TETAR(IM))/RD+TETAR(IM)
        RETURN
        END

!     ******************************
      SUBROUTINE TRIDAG(A,B,C,R,U,N)
!     ******************************
      PARAMETER (NMAX=500)
        IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GAM(NMAX),A(1),B(1),C(1),R(1),U(1)
      IF(B(1).EQ.0.)PAUSE
      BET=B(1)
      U(1)=R(1)/BET
      DO 11 J=2,N
        GAM(J)=C(J-1)/BET
        BET=B(J)-A(J)*GAM(J)
        IF(BET.EQ.0.)PAUSE
        U(J)=(R(J)-A(J)*U(J-1))/BET
11    CONTINUE
      DO 12 J=N-1,1,-1
        U(J)=U(J)-GAM(J+1)*U(J+1)
12    CONTINUE
      RETURN
      END
!     ******************************
       SUBROUTINE CMNT(FILEIN,FILEOU)
!     ******************************
!
!**** THIS SUBROUTINE ELIMINATES COMMENTS AND EMPTY LINES
!
        INTEGER FILEIN,FILEOU
        CHARACTER*132 LINEA
    1 READ(FILEIN,1000,END=2) LINEA
        N=1
        DO WHILE(LINEA(N:N).NE.';'.AND.N.LT.LEN(LINEA))
          N=N+1
        ENDDO
        M=N-1
        DO WHILE(LINEA(M:M).EQ.' '.AND.M.GT.0)
          M=M-1
        ENDDO
        IF(M.GT.0) WRITE(FILEOU,1000) LINEA(1:M)
        GOTO 1
    2 CLOSE(FILEIN)
        REWIND(FILEOU)
        RETURN
 1000 FORMAT(A)
        END



