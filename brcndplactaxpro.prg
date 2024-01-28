// Programa   : BRCNDPLACTAXPRO
// Fecha/Hora : 09/12/2022 07:43:39
// Propósito  : "Planificación por Prestador de Servicios"
// Creado Por : Automáticamente por BRWMAKER
// Llamado por: <DPXBASE>
// Aplicación : Gerencia
// Tabla      : <TABLA>

#INCLUDE "DPXBASE.CH"

PROCE MAIN(cWhere,cCodSuc,nPeriodo,dDesde,dHasta,cTitle, lView,lCtaEgreso,cPeriodo)
   LOCAL aData,aFechas,cFileMem:="USER\BRCNDPLACTAXPRO.MEM",V_nPeriodo:=1,cCodPar
   LOCAL V_dDesde:=CTOD(""),V_dHasta:=CTOD("")
   LOCAL cServer:=oDp:cRunServer
   LOCAL lConectar:=.F.

   oDp:cRunServer:=NIL

   IF Type("oCNDPLACTAXPRO")="O" .AND. oCNDPLACTAXPRO:oWnd:hWnd>0
      RETURN EJECUTAR("BRRUNNEW",oCNDPLACTAXPRO,GetScript())
   ENDIF

   DEFAULT cWhere:=GetWhereOr("PRO_TIPO",{"Prestador de Servicios","Servicios Públicos"})

   DEFAULT lCtaEgreso:=.T.,;
           cPeriodo  :="Mensual"

   IF !Empty(cServer)

     MsgRun("Conectando con Servidor "+cServer+" ["+ALLTRIM(SQLGET("DPSERVERBD","SBD_DOMINI","SBD_CODIGO"+GetWhere("=",cServer)))+"]",;
            "Por Favor Espere",{||lConectar:=EJECUTAR("DPSERVERDBOPEN",cServer)})

     IF !lConectar
        RETURN .F.
     ENDIF

   ENDIF


   cTitle:="Planificación por Prestador de Servicios" +IF(Empty(cTitle),"",cTitle)

   oDp:oFrm:=NIL

   IF FILE(cFileMem) .AND. nPeriodo=NIL
      RESTORE FROM (cFileMem) ADDI
      nPeriodo:=V_nPeriodo
   ENDIF

   DEFAULT cCodSuc :=oDp:cSucursal,;
           nPeriodo:=4,;
           dDesde  :=oDp:dFchInicio,;
           dHasta  :=oDp:dFchCierre,;
           lView   :=.F.


   // Obtiene el Código del Parámetro

   IF !Empty(cWhere)

      cCodPar:=ATAIL(_VECTOR(cWhere,"="))

      IF TYPE(cCodPar)="C"
        cCodPar:=SUBS(cCodPar,2,LEN(cCodPar))
        cCodPar:=LEFT(cCodPar,LEN(cCodPar)-1)
      ENDIF

   ENDIF


   IF .T. .AND. (!nPeriodo=11 .AND. (Empty(dDesde) .OR. Empty(dhasta)))

       aFechas:=EJECUTAR("DPDIARIOGET",nPeriodo)
       dDesde :=aFechas[1]
       dHasta :=aFechas[2]

   ENDIF

   EJECUTAR("DPCTAINDEF") 

   aData :=LEERDATA(HACERWHERE(dDesde,dHasta,cWhere),NIL,cServer,NIL,dDesde,dHasta,lCtaEgreso)

   IF Empty(aData)
      MensajeErr("no hay "+cTitle,"Información no Encontrada")
      RETURN .F.
   ENDIF

   ViewData(aData,cTitle,oDp:cWhere)

   oDp:oFrm:=oCNDPLACTAXPRO

RETURN .T.


FUNCTION ViewData(aData,cTitle,cWhere_)
   LOCAL oBrw,oCol,aTotal:=ATOTALES(aData)
   LOCAL oFont,oFontB
   LOCAL aPeriodos:=ACLONE(oDp:aPeriodos)
   LOCAL aCoors:=GetCoors( GetDesktopWindow() )

   DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -12
   DEFINE FONT oFontB NAME "Tahoma"   SIZE 0, -12 BOLD

   DpMdi(cTitle,"oCNDPLACTAXPRO","BRCNDPLACTAXPRO.EDT")
   oCNDPLACTAXPRO:Windows(0,0,aCoors[3]-160,MIN(3760,aCoors[4]-10),.T.) // Maximizado

   oCNDPLACTAXPRO:cCodSuc   :=cCodSuc
   oCNDPLACTAXPRO:lMsgBar   :=.F.
   oCNDPLACTAXPRO:cPeriodo  :=aPeriodos[nPeriodo]
   oCNDPLACTAXPRO:cCodSuc   :=cCodSuc
   oCNDPLACTAXPRO:nPeriodo  :=nPeriodo
   oCNDPLACTAXPRO:cNombre   :=""
   oCNDPLACTAXPRO:dDesde    :=dDesde
   oCNDPLACTAXPRO:cServer   :=cServer
   oCNDPLACTAXPRO:dHasta    :=dHasta
   oCNDPLACTAXPRO:cWhere    :=cWhere
   oCNDPLACTAXPRO:cWhere_   :=cWhere_
   oCNDPLACTAXPRO:cWhereQry :=""
   oCNDPLACTAXPRO:cSql      :=oDp:cSql
   oCNDPLACTAXPRO:oWhere    :=TWHERE():New(oCNDPLACTAXPRO)
   oCNDPLACTAXPRO:cCodPar   :=cCodPar // Código del Parámetro
   oCNDPLACTAXPRO:lWhen     :=.T.
   oCNDPLACTAXPRO:cTextTit  :="" // Texto del Titulo Heredado
   oCNDPLACTAXPRO:oDb       :=oDp:oDb
   oCNDPLACTAXPRO:cBrwCod   :="CNDPLACTAXPRO"
   oCNDPLACTAXPRO:lTmdi     :=.T.
   oCNDPLACTAXPRO:aHead     :={}
   oCNDPLACTAXPRO:lBarDef   :=.T. // Activar Modo Diseño.
   oCNDPLACTAXPRO:lView     :=lView
   oCNDPLACTAXPRO:cWhereCta :=[ CTA_CTADET AND (LEFT(CTA_CODIGO,1)="6" OR LEFT(CTA_CODIGO,1)="5")]
   oCNDPLACTAXPRO:nAno      :=YEAR(dDesde)
   oCNDPLACTAXPRO:nMes      :=MONTH(dDesde)
   oCNDPLACTAXPRO:lCtaEgreso:=lCtaEgreso
   oCNDPLACTAXPRO:cPeriodo  :=cPeriodo
   oCNDPLACTAXPRO:aIva      :=aTable("SELECT TIP_CODIGO FROM DPIVATIP WHERE TIP_ACTIVO=1 AND TIP_COMPRA=1",.T.)

   IF Empty(oCNDPLACTAXPRO:aIva)
      AADD(oCNDPLACTAXPRO:aIva,"EX")
   ENDIF

   oCNDPLACTAXPRO:aTipDoc   :=aTable("SELECT TDC_TIPO,TDC_DESCRI FROM DPTIPDOCPRO WHERE TDC_ACTIVO",.T.)
   oCNDPLACTAXPRO:aTipDocTxt:={}

   AEVAL(oCNDPLACTAXPRO:aTipDoc,{|a,n| AADD(oCNDPLACTAXPRO:aTipDocTxt,LEFT(a,3))})


   oCNDPLACTAXPRO:aTipDes   :=aTable("SELECT TDC_TIPO,TDC_DESCRI FROM DPTIPDOCPRO WHERE TDC_ACTIVO",.T.)
   AADD(oCNDPLACTAXPRO:aTipDes,SPACE(3)+" Ninguna")
   oCNDPLACTAXPRO:aTipDesTxt:={}

   AEVAL(oCNDPLACTAXPRO:aTipDes,{|a,n| AADD(oCNDPLACTAXPRO:aTipDesTxt,LEFT(a,3))})


   // Guarda los parámetros del Browse cuando cierra la ventana
   oCNDPLACTAXPRO:bValid   :={||EJECUTAR("BRWSAVEPAR",oCNDPLACTAXPRO)}

   oCNDPLACTAXPRO:lBtnRun     :=.F.
   oCNDPLACTAXPRO:lBtnMenuBrw :=.F.
   oCNDPLACTAXPRO:lBtnSave    :=.F.
   oCNDPLACTAXPRO:lBtnCrystal :=.F.
   oCNDPLACTAXPRO:lBtnRefresh :=.F.
   oCNDPLACTAXPRO:lBtnHtml    :=.T.
   oCNDPLACTAXPRO:lBtnExcel   :=.T.
   oCNDPLACTAXPRO:lBtnPreview :=.T.
   oCNDPLACTAXPRO:lBtnQuery   :=.F.
   oCNDPLACTAXPRO:lBtnOptions :=.T.
   oCNDPLACTAXPRO:lBtnPageDown:=.T.
   oCNDPLACTAXPRO:lBtnPageUp  :=.T.
   oCNDPLACTAXPRO:lBtnFilters :=.T.
   oCNDPLACTAXPRO:lBtnFind    :=.T.
   oCNDPLACTAXPRO:lBtnColor   :=.T.

   oCNDPLACTAXPRO:nClrPane1:=16775408
   oCNDPLACTAXPRO:nClrPane2:=16771797

   oCNDPLACTAXPRO:nClrText :=0
   oCNDPLACTAXPRO:nClrText1:=15890688
   oCNDPLACTAXPRO:nClrText2:=4227072
   oCNDPLACTAXPRO:nClrText3:=0

   oCNDPLACTAXPRO:oBrw:=TXBrowse():New( IF(oCNDPLACTAXPRO:lTmdi,oCNDPLACTAXPRO:oWnd,oCNDPLACTAXPRO:oDlg ))
   oCNDPLACTAXPRO:oBrw:SetArray( aData, .F. )
   oCNDPLACTAXPRO:oBrw:SetFont(oFont)

   oCNDPLACTAXPRO:oBrw:lFooter     := .T.
   oCNDPLACTAXPRO:oBrw:lHScroll    := .T.
   oCNDPLACTAXPRO:oBrw:nHeaderLines:= 2
   oCNDPLACTAXPRO:oBrw:nDataLines  := 1
   oCNDPLACTAXPRO:oBrw:nFooterLines:= 1
   oCNDPLACTAXPRO:oBrw:nFreeze     := 2

   oCNDPLACTAXPRO:aData            :=ACLONE(aData)

   AEVAL(oCNDPLACTAXPRO:oBrw:aCols,{|oCol|oCol:oHeaderFont:=oFontB})

  // Campo: PRO_CODIGO
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[1]
  oCol:cHeader      :='Código'+CRLF+"Proveedor"
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 80
  oCol:nEditType    :=IIF( lView, 0, EDIT_GET_BUTTON)
  oCol:bEditBlock   :={||oCNDPLACTAXPRO:EDITPROVEEDOR(1,.F.)}
  oCol:bOnPostEdit  :={|oCol,uValue,nKey|oCNDPLACTAXPRO:VALPROVEEDOR(oCol,uValue,1,nKey)}

  oCol:=oCNDPLACTAXPRO:oBrw:aCols[2]
  oCol:cHeader      :='Nombre'
  oCol:bLClickHeader:= {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 300
  oCol:bOnPostEdit  :={|oCol,uValue,nKey|oCNDPLACTAXPRO:VALNOMBREPRO(oCol,uValue,2,nKey,NIL,.T.)}
  oCol:nEditType    :=IIF( lView, 0, 1)


  // Campo: PRO_TIPO
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[3]
  oCol:cHeader      :='Tipo'+CRLF+'Relación'
  oCol:bLClickHeader:= {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 120
  oCol:bClrStd      := {|nClrText,uValue|uValue:=oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,3],;
                     nClrText:=COLOR_OPTIONS("DPPROVEEDOR ","PRO_TIPO",uValue),;
                     {nClrText,iif( oCNDPLACTAXPRO:oBrw:nArrayAt%2=0, oCNDPLACTAXPRO:nClrPane1, oCNDPLACTAXPRO:nClrPane2 ) } } 

  // Campo: PGC_REFERE
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[4]
  oCol:cHeader      :='Referencia'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 280
  oCol:nEditType    :=IIF( lView, 0, 1)
  oCol:bOnPostEdit  :={|oCol,uValue,nKey|  oCNDPLACTAXPRO:VALREFERE(uValue,4),oCNDPLACTAXPRO:PUTFIELDVALUE(oCol,uValue,4,nKey,NIL,.T.)}

IF !oCNDPLACTAXPRO:lCtaEgreso

  // Campo: PGC_CODCTA
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[5]
  oCol:cHeader      :='Cuenta'+CRLF+'Contable'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 160
  oCol:nEditType    :=IIF( lView, 0, EDIT_GET_BUTTON)
  oCol:bEditBlock   :={||oCNDPLACTAXPRO:EDITCTA(5,.F.)}
  oCol:bOnPostEdit  :={|oCol,uValue,nKey|oCNDPLACTAXPRO:VALCTA(oCol,uValue,5,nKey)}
  oCol:lButton      :=.F.

  // Campo: CTA_DESCRI
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[6]
  oCol:cHeader      :='Descripción'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 280

ELSE

  oCol:=oCNDPLACTAXPRO:oBrw:aCols[5]
  oCol:cHeader      :='Cuenta'+CRLF+'Egreso'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 160
  oCol:nEditType    :=IIF( lView, 0, EDIT_GET_BUTTON)
  oCol:bEditBlock   :={||oCNDPLACTAXPRO:EDITCTA(5,.F.)}
  oCol:bOnPostEdit  :={|oCol,uValue,nKey|oCNDPLACTAXPRO:VALCTAEGR(oCol,uValue,5,nKey)}

  oCol:lButton      :=.F.

  // Campo: CTA_DESCRI
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[6]
  oCol:cHeader      :='Descripción'+CRLF+"Cuenta Auxiliar"
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 320
  oCol:bOnPostEdit  :={|oCol,uValue,nKey|oCNDPLACTAXPRO:VALNOMBREEGR(oCol,uValue,6,nKey,NIL,.T.)}

ENDIF


  // Campo: PGC_IVA
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[7]
  oCol:cHeader      :='IVA'+CRLF+'Aplicado'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 40

  oCol:aEditListTxt  :=oCNDPLACTAXPRO:aIva
  oCol:aEditListBound:=oCNDPLACTAXPRO:aIva
  oCol:nEditType     :=EDIT_LISTBOX
  oCol:bOnPostEdit   :={|oCol,uValue,nKey|oCNDPLACTAXPRO:PUTFIELDVALUE(oCol,uValue,7,nKey,NIL,.T.,"PGC_IVA")}

  // Campo: PGC_DESCRI
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[8]
  oCol:cHeader      :='Descripción'+CRLF+"Del Servicio"
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 240
  oCol:nEditType    :=IIF( lView, 0, 1)
  oCol:bOnPostEdit  :={|oCol,uValue,nKey|oCNDPLACTAXPRO:PUTFIELDVALUE(oCol,uValue,8,nKey,NIL,.T.)}

  // Campo: PGC_TIPDOC
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[09]
  oCol:cHeader       :='Tipo'+CRLF+'Doc.'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth        := 24
  oCol:aEditListTxt  :=oCNDPLACTAXPRO:aTipDoc
  oCol:aEditListBound:=oCNDPLACTAXPRO:aTipDocTxt
  oCol:nEditType     :=EDIT_LISTBOX
  oCol:bOnPostEdit   :={|oCol,uValue,nKey|oCNDPLACTAXPRO:PUTFIELDVALUE(oCol,uValue,09,nKey,NIL,.T.,"PGC_TIPDOC")}


  // Campo: PLP_MTODIV
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[10]
  oCol:cHeader      :='Monto Mensual'+CRLF+'Divisa '+oDp:cMonedaExt
  oCol:bLClickHeader:= {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 090
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999,999,999,999,999.99'
  oCol:bStrData     :={|nMonto,oCol|nMonto:= oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,10],;
                                    oCol  := oCNDPLACTAXPRO:oBrw:aCols[10],;
                                    FDP(nMonto,oCol:cEditPicture)}
  oCol:cFooter      :=FDP(aTotal[10],oCol:cEditPicture)

  IF !lView
     oCol:nEditType    :=1
     oCol:bOnPostEdit  :={|oCol,uValue,nLastKey,nCol|oCNDPLACTAXPRO:PUTMTODIV(uValue,10)}
  ENDIF

  // Campo: PGC_TIPDES
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[11]
  oCol:cHeader       :='Doc.'+CRLF+'Destino'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth        := 40
  oCol:aEditListTxt  :=oCNDPLACTAXPRO:aTipDes
  oCol:aEditListBound:=oCNDPLACTAXPRO:aTipDesTxt
  oCol:nEditType     :=EDIT_LISTBOX
  oCol:bOnPostEdit   :={|oCol,uValue,nKey|oCNDPLACTAXPRO:PUTFIELDVALUE(oCol,uValue,11,nKey,NIL,.T.,"PGC_TIPDES")}


  // Campo: CUANTOS
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[12]
  oCol:cHeader      :='Cant.'+CRLF+'Reg'
  oCol:bLClickHeader := {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 144
  oCol:nDataStrAlign:= AL_RIGHT 
  oCol:nHeadStrAlign:= AL_RIGHT 
  oCol:nFootStrAlign:= AL_RIGHT 
  oCol:cEditPicture :='9,999'
  oCol:bStrData:={|nMonto,oCol|nMonto:= oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,12],;
                               oCol  := oCNDPLACTAXPRO:oBrw:aCols[12],;
                               FDP(nMonto,oCol:cEditPicture)}
  oCol:cFooter      :=FDP(aTotal[12],oCol:cEditPicture)


  // Campo: CUANTOS
  oCol:=oCNDPLACTAXPRO:oBrw:aCols[13]
  oCol:cHeader      :='Reg.'+CRLF+'Planif.'
  oCol:bLClickHeader:= {|r,c,f,o| SortArray( o, oCNDPLACTAXPRO:oBrw:aArrayData ) } 
  oCol:nWidth       := 70

  oCNDPLACTAXPRO:oBrw:aCols[1]:cFooter:=" #"+LSTR(LEN(aData))
  oCNDPLACTAXPRO:oBrw:bClrStd  := {|oBrw,nClrText,aLine|oBrw:=oCNDPLACTAXPRO:oBrw,aLine:=oBrw:aArrayData[oBrw:nArrayAt],;
                                                 nClrText:=oCNDPLACTAXPRO:nClrText,;
                                                 nClrText:=IF(aLine[10]>0,oCNDPLACTAXPRO:nClrText1,nClrText),;
                                                 nClrText:=IF(.F.,oCNDPLACTAXPRO:nClrText2,nClrText),;
                                                 {nClrText,iif( oBrw:nArrayAt%2=0, oCNDPLACTAXPRO:nClrPane1, oCNDPLACTAXPRO:nClrPane2 ) } }

  oCNDPLACTAXPRO:oBrw:bClrHeader          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
  oCNDPLACTAXPRO:oBrw:bClrFooter          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}

  oCNDPLACTAXPRO:oBrw:bLDblClick:={|oBrw|oCNDPLACTAXPRO:RUNCLICK() }

  oCNDPLACTAXPRO:oBrw:bChange:={||oCNDPLACTAXPRO:BRWCHANGE()}

//  oCNDPLACTAXPRO:oBrw:bKeyDown:={|nKey| IF(nKey>64,oCNDPLACTAXPRO:oBrw:aCols[1]:Edit(),nil)}

  oCNDPLACTAXPRO:oBrw:CreateFromCode()

  oCNDPLACTAXPRO:oWnd:oClient := oCNDPLACTAXPRO:oBrw

  oCNDPLACTAXPRO:Activate({||oCNDPLACTAXPRO:ViewDatBar()})

  oCNDPLACTAXPRO:BRWRESTOREPAR()

  @ oCNDPLACTAXPRO:oAno:nBottom-oCNDPLACTAXPRO:oAno:nHeight(),oCNDPLACTAXPRO:oAno:nLeft+oCNDPLACTAXPRO:oAno:nWidth();
     BUTTON oBtn PROMPT ">" PIXEL;
     SIZE 22,22   FONT oFont OF oCNDPLACTAXPRO:oDesde:oWnd CANCEL FONT oFont ACTION oCNDPLACTAXPRO:FCHHASTA(.T.)

RETURN .T.

/*
// Barra de Botones
*/
FUNCTION ViewDatBar()
   LOCAL oCursor,oBar,oBtn,oFont,oCol
   LOCAL oDlg:=IF(oCNDPLACTAXPRO:lTmdi,oCNDPLACTAXPRO:oWnd,oCNDPLACTAXPRO:oDlg)
   LOCAL nLin:=2,nCol:=0
   LOCAL nWidth:=oCNDPLACTAXPRO:oBrw:nWidth()

   oCNDPLACTAXPRO:oBrw:GoBottom(.T.)
   oCNDPLACTAXPRO:oBrw:Refresh(.T.)

   DEFINE CURSOR oCursor HAND

   IF !oDp:lBtnText 
     DEFINE BUTTONBAR oBar SIZE 52-15,60-15 OF oDlg 3D CURSOR oCursor
   ELSE 
     DEFINE BUTTONBAR oBar SIZE oDp:nBtnWidth,oDp:nBarnHeight+6 OF oDlg 3D CURSOR oCursor 
   ENDIF 

   DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -12 BOLD

 // Emanager no Incluye consulta de Vinculos


   IF .F. .AND. Empty(oCNDPLACTAXPRO:cServer)

   oCNDPLACTAXPRO:oFontBtn   :=oFont    
   oCNDPLACTAXPRO:nClrPaneBar:=oDp:nGris
   oCNDPLACTAXPRO:oBrw:oLbx  :=oCNDPLACTAXPRO

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\VIEW.BMP";
          TOP PROMPT "Consulta"; 
          ACTION  EJECUTAR("BRWRUNLINK",oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO:cSql)

     oBtn:cToolTip:="Consultar Vinculos"


   ENDIF

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XNEW.BMP";
          TOP PROMPT "Incluir"; 
          ACTION oCNDPLACTAXPRO:CNDNEWLINE()

// ADDPROVEEDOR()

    oBtn:cToolTip:="Incluir Registro"

    oCNDPLACTAXPRO:oBtnNew:=oBtn


   DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\XBROWSE.BMP";
            TOP PROMPT "Detalles"; 
            ACTION  EJECUTAR("BRPROVCONPLA",NIL,NIL,oDp:nEjercicio,oDp:dFchIniPla,oDp:dFchFinPla)

    oBtn:cToolTip:="Ver Planificación detallada "

    oCNDPLACTAXPRO:oBtnPla:=oBtn

    DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\PROVEEDORPROG.BMP";
            TOP PROMPT "Planif.";
            ACTION EJECUTAR("DPPROVPROGLBX",NIL,oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,1])

   oBtn:cToolTip:="Planificación por Prestador de Servicios"

   oCNDPLACTAXPRO:oBtnPla:=oBtn


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\ZOOM.BMP";
          TOP PROMPT "Zoom";
          ACTION IF(oCNDPLACTAXPRO:oWnd:IsZoomed(),oCNDPLACTAXPRO:oWnd:Restore(),oCNDPLACTAXPRO:oWnd:Maximize())

   oBtn:cToolTip:="Maximizar"

/*
   IF Empty(oCNDPLACTAXPRO:cServer) .AND. !Empty(SQLGET("DPBRWLNK","EBR_CODIGO","EBR_CODIGO"+GetWhere("=","CNDPLACTAXPRO")))
*/

   IF ISSQLFIND("DPBRWLNKCONCAT","BRC_CODIGO"+GetWhere("=","CNDPLACTAXPRO"))

       DEFINE BUTTON oBtn;
       OF oBar;
       NOBORDER;
       FONT oFont;
       FILENAME "BITMAPS\XBROWSE.BMP";
       ACTION EJECUTAR("BRWRUNBRWLINK",oCNDPLACTAXPRO:oBrw,"CNDPLACTAXPRO",oCNDPLACTAXPRO:cSql,oCNDPLACTAXPRO:nPeriodo,oCNDPLACTAXPRO:dDesde,oCNDPLACTAXPRO:dHasta,oCNDPLACTAXPRO)

       oBtn:cToolTip:="Ejecutar Browse Vinculado(s)"
       oCNDPLACTAXPRO:oBtnRun:=oBtn

       oCNDPLACTAXPRO:oBrw:bLDblClick:={||EVAL(oCNDPLACTAXPRO:oBtnRun:bAction) }

   ENDIF


IF oCNDPLACTAXPRO:lBtnRun

     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            MENU EJECUTAR("BRBTNMENU",{"Opcion 1",;
                                       "Opcion 2",;
                                       "Opcion 3"},;
                                       "oCNDPLACTAXPRO");
            FILENAME "BITMAPS\RUN.BMP";
            ACTION oCNDPLACTAXPRO:BTNRUN()

      oBtn:cToolTip:="Opciones de Ejecucion"

ENDIF

IF oCNDPLACTAXPRO:lBtnColor

     oCNDPLACTAXPRO:oBtnColor:=NIL

     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\COLORS.BMP";
            TOP PROMPT "Color";
            MENU EJECUTAR("BRBTNMENUCOLOR",oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO,oCNDPLACTAXPRO:oBtnColor,{||EJECUTAR("BRWCAMPOSOPC",oCNDPLACTAXPRO,.T.)});
            ACTION EJECUTAR("BRWSELCOLORFIELD",oCNDPLACTAXPRO,.T.)

    oBtn:cToolTip:="Personalizar Colores en los Campos"

    oCNDPLACTAXPRO:oBtnColor:=oBtn

ENDIF



IF oCNDPLACTAXPRO:lBtnSave
/*
      DEFINE BITMAP OF OUTLOOK oBRWMENURUN:oOut ;
             BITMAP "BITMAPS\XSAVE.BMP";
             PROMPT "Guardar Consulta";
               TOP PROMPT "Grabar"; 
              ACTION  EJECUTAR("DPBRWSAVE",oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO:oFrm)
*/
ENDIF

IF oCNDPLACTAXPRO:lBtnMenuBrw

 DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\BRWMENU.BMP",NIL,"BITMAPS\BRWMENUG.BMP";
          TOP PROMPT "Menú"; 
          ACTION  (EJECUTAR("BRWBUILDHEAD",oCNDPLACTAXPRO),;
                   EJECUTAR("DPBRWMENURUN",oCNDPLACTAXPRO,oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO:cBrwCod,oCNDPLACTAXPRO:cTitle,oCNDPLACTAXPRO:aHead));
          WHEN !Empty(oCNDPLACTAXPRO:oBrw:aArrayData[1,1])

   oBtn:cToolTip:="Menú de Opciones"

ENDIF


IF oCNDPLACTAXPRO:lBtnFind

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XFIND.BMP";
          TOP PROMPT "Buscar"; 
          ACTION  EJECUTAR("BRWSETFIND",oCNDPLACTAXPRO:oBrw)

   oBtn:cToolTip:="Buscar"

ENDIF

IF oCNDPLACTAXPRO:lBtnFilters

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\FILTRAR.BMP";
          MENU EJECUTAR("BRBTNMENUFILTER",oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO);
          TOP PROMPT "Filtrar"; 
          ACTION  EJECUTAR("BRWSETFILTER",oCNDPLACTAXPRO:oBrw)

   oBtn:cToolTip:="Filtrar Registros"

ENDIF

IF oCNDPLACTAXPRO:lBtnOptions

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\OPTIONS.BMP",NIL,"BITMAPS\OPTIONSG.BMP";
            TOP PROMPT "Opciones"; 
              ACTION  EJECUTAR("BRWSETOPTIONS",oCNDPLACTAXPRO:oBrw);
          WHEN LEN(oCNDPLACTAXPRO:oBrw:aArrayData)>1

   oBtn:cToolTip:="Filtrar según Valores Comunes"

ENDIF

IF oCNDPLACTAXPRO:lBtnRefresh

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\REFRESH.BMP";
          TOP PROMPT "Refrescar"; 
          ACTION  oCNDPLACTAXPRO:BRWREFRESCAR()

   oBtn:cToolTip:="Refrescar"

ENDIF

IF oCNDPLACTAXPRO:lBtnCrystal

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\CRYSTAL.BMP";
          TOP PROMPT "Crystal"; 
          ACTION  EJECUTAR("BRWTODBF",oCNDPLACTAXPRO)

   oBtn:cToolTip:="Visualizar Mediante Crystal Report"

ENDIF

IF oCNDPLACTAXPRO:lBtnExcel


     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\EXCEL.BMP";
            TOP PROMPT "Excel"; 
            ACTION  (EJECUTAR("BRWTOEXCEL",oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO:cTitle,oCNDPLACTAXPRO:cNombre))

     oBtn:cToolTip:="Exportar hacia Excel"

     oCNDPLACTAXPRO:oBtnXls:=oBtn

ENDIF

IF oCNDPLACTAXPRO:lBtnHtml

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\html.BMP";
          TOP PROMPT "Html"; 
          ACTION  (oCNDPLACTAXPRO:HTMLHEAD(),EJECUTAR("BRWTOHTML",oCNDPLACTAXPRO:oBrw,NIL,oCNDPLACTAXPRO:cTitle,oCNDPLACTAXPRO:aHead))

   oBtn:cToolTip:="Generar Archivo html"

   oCNDPLACTAXPRO:oBtnHtml:=oBtn

ENDIF


IF oCNDPLACTAXPRO:lBtnPreview

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\PREVIEW.BMP";
          TOP PROMPT "Preview"; 
          ACTION  (EJECUTAR("BRWPREVIEW",oCNDPLACTAXPRO:oBrw))

   oBtn:cToolTip:="Previsualización"

   oCNDPLACTAXPRO:oBtnPreview:=oBtn

ENDIF

   IF ISSQLGET("DPREPORTES","REP_CODIGO","BRCNDPLACTAXPRO")

     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\XPRINT.BMP";
            TOP PROMPT "Imprimir"; 
            ACTION  oCNDPLACTAXPRO:IMPRIMIR()

      oBtn:cToolTip:="Imprimir"

     oCNDPLACTAXPRO:oBtnPrint:=oBtn

   ENDIF

IF oCNDPLACTAXPRO:lBtnQuery


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\QUERY.BMP";
          ACTION oCNDPLACTAXPRO:BRWQUERY()

   oBtn:cToolTip:="Imprimir"

ENDIF

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xTOP.BMP";
          TOP PROMPT "Primero"; 
          ACTION  (oCNDPLACTAXPRO:oBrw:GoTop(),oCNDPLACTAXPRO:oBrw:Setfocus())

IF nWidth>800 .OR. nWidth=0

   IF oCNDPLACTAXPRO:lBtnPageDown

     DEFINE BUTTON oBtn;
            OF oBar;
            NOBORDER;
            FONT oFont;
            FILENAME "BITMAPS\xSIG.BMP";
            TOP PROMPT "Avance"; 
            ACTION  (oCNDPLACTAXPRO:oBrw:PageDown(),oCNDPLACTAXPRO:oBrw:Setfocus())
  ENDIF

  IF  oCNDPLACTAXPRO:lBtnPageUp

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           FILENAME "BITMAPS\xANT.BMP";
           TOP PROMPT "Anterior"; 
           ACTION  (oCNDPLACTAXPRO:oBrw:PageUp(),oCNDPLACTAXPRO:oBrw:Setfocus())
  ENDIF

ENDIF

  DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xFIN.BMP";
          TOP PROMPT "Ultimo"; 
          ACTION  (oCNDPLACTAXPRO:oBrw:GoBottom(),oCNDPLACTAXPRO:oBrw:Setfocus())

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XSALIR.BMP";
          TOP PROMPT "Cerrar"; 
          ACTION  oCNDPLACTAXPRO:Close()

  oCNDPLACTAXPRO:oBrw:SetColor(0,oCNDPLACTAXPRO:nClrPane1)

//oCNDPLACTAXPRO:SETBTNBAR(40,40,oBar)

  EVAL(oCNDPLACTAXPRO:oBrw:bChange)

  oBar:SetColor(CLR_BLACK,oDp:nGris)

  nCol:=32
  AEVAL(oBar:aControls,{|o,n|nCol:=nCol+o:nWidth(),o:SetColor(CLR_BLACK,oDp:nGris)})

  @ 5,nCol+11 SAY "Año" OF oBar PIXEL SIZE 32,20 BORDER FONT oFont;
                  COLOR oDp:nClrYellowText,oDp:nClrYellow RIGHT

  @ 5,nCol+44 GET oCNDPLACTAXPRO:oAno VAR oCNDPLACTAXPRO:nAno PICTURE "9999" SPINNER;
              VALID oCNDPLACTAXPRO:FCHHASTA(.T.); 
              WHEN .T. OF oBar PIXEL SIZE 50,20

  oCNDPLACTAXPRO:oBar:=oBar

  @ 5,nCol+140    SAY oCNDPLACTAXPRO:oDesde PROMPT DTOC(oCNDPLACTAXPRO:dDesde) OF oBar PIXEL SIZE 82,20 BORDER FONT oFont;
                  COLOR oDp:nClrYellowText,oDp:nClrYellow

  @ 5,nCol+140+90 SAY oCNDPLACTAXPRO:oHasta PROMPT DTOC(oCNDPLACTAXPRO:dHasta) OF oBar PIXEL SIZE 82,20 BORDER FONT oFont;
                  COLOR oDp:nClrYellowText,oDp:nClrYellow

RETURN .T.

FUNCTION FCHHASTA(lRefresh)

    DEFAULT lRefresh:=.T.

    oCNDPLACTAXPRO:dDesde:=LEFT(DTOC(oCNDPLACTAXPRO:dDesde,"C"),5)+"/"+STRZERO(oCNDPLACTAXPRO:nAno,4)
    oCNDPLACTAXPRO:dDesde:=CTOO(oCNDPLACTAXPRO:dDesde,"D")

    oCNDPLACTAXPRO:dHasta:="01/"+STRZERO(oCNDPLACTAXPRO:nMes,2)+"/"+STRZERO(oCNDPLACTAXPRO:nAno,4)
    oCNDPLACTAXPRO:dHasta:=FCHFINMES(CTOO(oCNDPLACTAXPRO:dHasta,"D"))

    oCNDPLACTAXPRO:cAno1 :=STRZERO(YEAR(oCNDPLACTAXPRO:dDesde-1),4)
    oCNDPLACTAXPRO:cAno2 :=STRZERO(YEAR(oCNDPLACTAXPRO:dHasta-0),4)

    IF lRefresh

      oCNDPLACTAXPRO:oDesde:Refresh(.T.)
      oCNDPLACTAXPRO:oHasta:Refresh(.T.)

      oCNDPLACTAXPRO:LEERDATA(oCNDPLACTAXPRO:cWhere,oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO:cServer,oCNDPLACTAXPRO)

    ENDIF

RETURN .T.

/*
// Evento para presionar CLICK
*/
FUNCTION RUNCLICK()


RETURN .T.


/*
// Imprimir
*/
FUNCTION IMPRIMIR()
  LOCAL oRep,cWhere

  oRep:=REPORTE("BRCNDPLACTAXPRO",cWhere)
  oRep:cSql  :=oCNDPLACTAXPRO:cSql
  oRep:cTitle:=oCNDPLACTAXPRO:cTitle

RETURN .T.

FUNCTION LEEFECHAS()
  LOCAL nPeriodo:=oCNDPLACTAXPRO:oPeriodo:nAt,cWhere

  oCNDPLACTAXPRO:nPeriodo:=nPeriodo


  IF oCNDPLACTAXPRO:oPeriodo:nAt=LEN(oCNDPLACTAXPRO:oPeriodo:aItems)

     oCNDPLACTAXPRO:oDesde:ForWhen(.T.)
     oCNDPLACTAXPRO:oHasta:ForWhen(.T.)
     oCNDPLACTAXPRO:oBtn  :ForWhen(.T.)

     DPFOCUS(oCNDPLACTAXPRO:oDesde)

  ELSE

     oCNDPLACTAXPRO:aFechas:=EJECUTAR("DPDIARIOGET",nPeriodo)

     oCNDPLACTAXPRO:oDesde:VarPut(oCNDPLACTAXPRO:aFechas[1] , .T. )
     oCNDPLACTAXPRO:oHasta:VarPut(oCNDPLACTAXPRO:aFechas[2] , .T. )

     oCNDPLACTAXPRO:dDesde:=oCNDPLACTAXPRO:aFechas[1]
     oCNDPLACTAXPRO:dHasta:=oCNDPLACTAXPRO:aFechas[2]

     cWhere:=oCNDPLACTAXPRO:HACERWHERE(oCNDPLACTAXPRO:dDesde,oCNDPLACTAXPRO:dHasta,oCNDPLACTAXPRO:cWhere,.T.)

     oCNDPLACTAXPRO:LEERDATA(cWhere,oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO:cServer,oCNDPLACTAXPRO)

  ENDIF

  oCNDPLACTAXPRO:SAVEPERIODO()

RETURN .T.


FUNCTION HACERWHERE(dDesde,dHasta,cWhere_,lRun)
   LOCAL cWhere:=""

   DEFAULT lRun:=.F.

   // Campo fecha no puede estar en la nueva clausula
   IF ""$cWhere
     RETURN ""
   ENDIF

   IF !Empty(dDesde)
       
   ELSE
     IF !Empty(dHasta)
       
     ENDIF
   ENDIF


   IF !Empty(cWhere_)
      cWhere:=cWhere + IIF( Empty(cWhere),""," AND ") +cWhere_
   ENDIF

   IF lRun

     IF !Empty(oCNDPLACTAXPRO:cWhereQry)
       cWhere:=cWhere + oCNDPLACTAXPRO:cWhereQry
     ENDIF

     oCNDPLACTAXPRO:LEERDATA(cWhere,oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO:cServer,oCNDPLACTAXPRO)

   ENDIF


RETURN cWhere


FUNCTION LEERDATA(cWhere,oBrw,cServer,oCNDPLACTAXPRO,dDesde,dHasta,lCtaEgreso)
   LOCAL aData:={},aTotal:={},oCol,cSql,aLines:={}
   LOCAL oDb
   LOCAL nAt,nRowSel,cJoinCta,cCtaDescri

   DEFAULT cWhere    :="",;
           lCtaEgreso:=.T.

   IF !Empty(cServer)

     IF !EJECUTAR("DPSERVERDBOPEN",cServer)
        RETURN .F.
     ENDIF

     oDb:=oDp:oDb

   ENDIF

   IF TYPE("oCNDPLACTAXPRO")="O"
      dDesde    :=oCNDPLACTAXPRO:dDesde
      dHasta    :=oCNDPLACTAXPRO:dHasta
      lCtaEgreso:=oCNDPLACTAXPRO:lCtaEgreso
   ENDIF

   cWhere:=IIF(Empty(cWhere),"",ALLTRIM(cWhere))

   IF !Empty(cWhere) .AND. LEFT(cWhere,5)="WHERE"
      cWhere:=SUBS(cWhere,6,LEN(cWhere))
   ENDIF


   IF lCtaEgreso

      cJoinCta  :="  LEFT JOIN DPCTAEGRESO        ON PGC_CTAEGR=CEG_CODIGO"
      cCtaDescri:="  PGC_CTAEGR,CEG_DESCRI,"

   ELSE

      cJoinCta  :="  LEFT JOIN DPCTA              ON PPC_CTAMOD=CTA_CODMOD AND PPC_CODCTA=CTA_CODIGO "
      cCtaDescri:="  PGC_CODCTA,CTA_DESCRI,"

   ENDIF

   cSql:=" SELECT     "+;
          "  PRO_CODIGO,   "+;
          "  PRO_NOMBRE,"+;
          "  PRO_TIPO, "+;
          "  PGC_REFERE,"+;
          cCtaDescri+;
          "  PGC_IVA,   "+;
          "  PGC_DESCRI,"+;
          "  PGC_TIPDOC,"+;
          "  PGC_MTODIV,"+;
          "  PGC_TIPDES,"+;
          "  COUNT(*) AS CUANTOS,PGC_NUMERO"+;
          "  FROM DPPROVEEDOR  "+;
          "  LEFT JOIN DPPROVEEDORPROG   ON PGC_CODIGO=PRO_CODIGO "+;
          "  LEFT JOIN dptipdocpro       ON PGC_TIPDOC=TDC_TIPO   AND TDC_TRIBUT=0  "+;   
          cJoinCta+;
          "  WHERE LEFT(PRO_SITUAC,1)"+GetWhere("=","A")+;
          "  GROUP BY PRO_CODIGO,PRO_NOMBRE,PRO_TIPO"+;
          "  ORDER BY PRO_TIPO"+;
          ""

/*
   IF Empty(cWhere)
     cSql:=STRTRAN(cSql,"<WHERE>","")
   ELSE
     cSql:=STRTRAN(cSql,"<WHERE>"," WHERE "+cWhere)
   ENDIF
*/
   IF !Empty(cWhere)
      cSql:=EJECUTAR("SQLINSERTWHERE",cSql,cWhere)
   ENDIF

   cSql:=EJECUTAR("WHERE_VAR",cSql)


   oDp:lExcluye:=.T.

   DPWRITE("TEMP\BRCNDPLACTAXPRO.SQL",cSql)


   aData:=ASQL(cSql,oDb)

   oDp:cWhere:=cWhere

   IF EMPTY(aData)
      aData:=EJECUTAR("SQLARRAYEMPTY",cSql,oDb)
   ENDIF

   AEVAL(aData,{|a,n|aData[n,07]:=IF(Empty(a[07]),"GN"     ,a[07]),;
                     aData[n,09]:=IF(Empty(a[09]),"FAC"    ,a[09])})

   AEVAL(aData,{|a,n|aData[n,3]:=SAYOPTIONS("DPPROVEEDOR","PRO_TIPO",a[3]) })

   IF ValType(oBrw)="O"

      oCNDPLACTAXPRO:cSql   :=cSql
      oCNDPLACTAXPRO:cWhere_:=cWhere

      aTotal:=ATOTALES(aData)

      oBrw:aArrayData:=ACLONE(aData)
      // oBrw:nArrayAt  :=1
      // oBrw:nRowSel   :=1

      // JN 15/03/2020 Sustituido por BRWCALTOTALES
      EJECUTAR("BRWCALTOTALES",oBrw,.F.)

      nAt    :=oBrw:nArrayAt
      nRowSel:=oBrw:nRowSel

      oBrw:Refresh(.F.)
      oBrw:nArrayAt  :=MIN(nAt,LEN(aData))
      oBrw:nRowSel   :=MIN(nRowSel,oBrw:nRowSel)
      AEVAL(oCNDPLACTAXPRO:oBar:aControls,{|o,n| o:ForWhen(.T.)})

      oCNDPLACTAXPRO:SAVEPERIODO()

   ENDIF

RETURN aData


FUNCTION SAVEPERIODO()
  LOCAL cFileMem:="USER\BRCNDPLACTAXPRO.MEM",V_nPeriodo:=oCNDPLACTAXPRO:nPeriodo
  LOCAL V_dDesde:=oCNDPLACTAXPRO:dDesde
  LOCAL V_dHasta:=oCNDPLACTAXPRO:dHasta

  SAVE TO (cFileMem) ALL LIKE "V_*"

RETURN .T.

/*
// Permite Crear Filtros para las Búquedas
*/
FUNCTION BRWQUERY()
     EJECUTAR("BRWQUERY",oCNDPLACTAXPRO)
RETURN .T.

/*
// Ejecución Cambio de Linea
*/
FUNCTION BRWCHANGE()
RETURN NIL

/*
// Refrescar Browse
*/
FUNCTION BRWREFRESCAR()
    LOCAL cWhere


    IF Type("oCNDPLACTAXPRO")="O" .AND. oCNDPLACTAXPRO:oWnd:hWnd>0

      cWhere:=" "+IIF(!Empty(oCNDPLACTAXPRO:cWhere_),oCNDPLACTAXPRO:cWhere_,oCNDPLACTAXPRO:cWhere)
      cWhere:=STRTRAN(cWhere," WHERE ","")

      oCNDPLACTAXPRO:LEERDATA(oCNDPLACTAXPRO:cWhere_,oCNDPLACTAXPRO:oBrw,oCNDPLACTAXPRO:cServer)
      oCNDPLACTAXPRO:oWnd:Show()
      oCNDPLACTAXPRO:oWnd:Restore()

    ENDIF

RETURN NIL

FUNCTION BTNRUN()
    ? "PERSONALIZA FUNCTION DE BTNRUN"
RETURN .T.

FUNCTION BTNMENU(nOption,cOption)

   ? nOption,cOption,"PESONALIZA LAS SUB-OPCIONES"

   IF nOption=1
   ENDIF

   IF nOption=2
   ENDIF

   IF nOption=3
   ENDIF

RETURN .T.

FUNCTION HTMLHEAD()

   oCNDPLACTAXPRO:aHead:=EJECUTAR("HTMLHEAD",oCNDPLACTAXPRO)

// Ejemplo para Agregar mas Parámetros
//   AADD(oDOCPROISLR:aHead,{"Consulta",oDOCPROISLR:oWnd:cTitle})

RETURN

// Restaurar Parametros
FUNCTION BRWRESTOREPAR()
  EJECUTAR("BRWRESTOREPAR",oCNDPLACTAXPRO)
RETURN .T.


FUNCTION EDITCTA(nCol,lSave)
   LOCAL oBrw  :=oCNDPLACTAXPRO:oBrw,oLbx
   LOCAL nAt   :=oBrw:nArrayAt
   LOCAL uValue:=oBrw:aArrayData[oBrw:nArrayAt,nCol]

   IF !oCNDPLACTAXPRO:lCtaEgreso

     oLbx:=DpLbx("DPCTAUTILIZACION.LBX",NIL,"CTA_CODMOD"+GetWhere("=",oDp:cCtaMod)+" AND "+oCNDPLACTAXPRO:cWhereCta)
     oLbx:GetValue("CTA_CODIGO",oBrw:aCols[nCol],,,uValue)

   ELSE

     oLbx:=DpLbx("DPCTAEGRESO.LBX")
     oLbx:GetValue("CEG_CODIGO",oBrw:aCols[nCol],,,uValue)

   ENDIF

   oCNDPLACTAXPRO:lAcction  :=.T.
   oBrw:nArrayAt:=nAt

   SysRefresh(.t.)

RETURN uValue

FUNCTION VALCTA(oCol,uValue,nCol,nKey)
  LOCAL cTipDoc,oTable,cWhere:="",cCtaOld:="",cDescri,aLine:={},cWhere

  DEFAULT nKey:=0

  DEFAULT oCol:lButton:=.F.

  IF oCol:lButton=.T.
    oCol:lButton:=.F.
    RETURN .T.
  ENDIF

  IF !SQLGET("DPCTA","CTA_CODIGO,CTA_DESCRI","CTA_CODIGO"+GetWhere("=",uValue))==uValue
    // MensajeErr("Cuenta Contable no Existe")
    EJECUTAR("XSCGMSGERR",oCNDPLACTAXPRO:oBrw,"Cuenta Contable no Existe ","Validación")
    EVAL(oCol:bEditBlock)  
    RETURN .F.
  ENDIF

  cDescri:=oDp:aRow[2]

  IF !EJECUTAR("ISCTADET",uValue,.T.)
    EVAL(oCol:bEditBlock)  
    RETURN .F.
  ENDIF

  oCNDPLACTAXPRO:lAcction:=.F.

  oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol]:=uValue
  oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol+1]:=cDescri

  IF !Empty(uValue)
    oCNDPLACTAXPRO:PUTFIELDVALUE(oCol,uValue,nCol,nKey,NIL,.T.,"PGC_CODCTA")
  ENDIF

  oCol:oBrw:DrawLine(.T.)

RETURN .T.


FUNCTION PUTFIELDVALUE(oCol,uValue,nCol,nKey,NIL,.T.,cField)
   LOCAL cWhere
   LOCAL cNumero:=oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,13]
   LOCAL cCodPro:=oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,01]

   oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol]:=uValue
   oCol:oBrw:DrawLine(.T.)

   IF !Empty(cNumero) .AND. !Empty(cField)

      cWhere:="PGC_CODIGO"+GetWhere("=",cCodPro)+" AND "+;
              "PGC_NUMERO"+GetWhere("=",cNumero)

      SQLUPDATE("DPPROVEEDORPROG",cField,uValue,cWhere)

   ENDIF

RETURN .T.

/*
// Modificar el valor de la Divisa en el Documento de Origen
*/
FUNCTION PUTMTODIV(nMonto,nCol)
   LOCAL aLine:=oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt]
   LOCAL aTotal,oCol
   LOCAL cTipo   :=NIL
   LOCAL nValCam :=EJECUTAR("DPGETVALCAM",oDp:cMonedaExt,oDp:dFecha)
   LOCAL nMtoBs  :=ROUND(nMonto*nValCam,2)
   LOCAL cTipDoc :=aLine[09]
   LOCAL cCodPro :=aLine[01]
   LOCAL cCtaEgr :=oDp:cCtaIndef
   LOCAL cNumero :=aLine[13] 
   LOCAL cCodCta :=aLine[05]
   LOCAL cRefere :=aLine[04]
  
   oDp:dFchIni:=oCNDPLACTAXPRO:dDesde
   oDp:dFchFin:=oCNDPLACTAXPRO:dHasta

   IF oCNDPLACTAXPRO:lCtaEgreso

      cCtaEgr:=aLine[05]
      cCodCta:=EJECUTAR("DPGETCTAMOD","DPCTAEGRESO_CTA",cCtaEgr,NIL,"CUENTA")

   ENDIF

   cCtaEgr:=IF(Empty(cCtaEgr),oDp:cCtaIndef,cCtaEgr)

   CursorWait()

   DEFAULT nCol:=11

   oCol:=oCNDPLACTAXPRO:oBrw:aCols[nCol]

   IF !oCNDPLACTAXPRO:lCtaEgreso
     EJECUTAR("SETCTAINTMOD","DPPROVEEDORPROG_CTA",cCodPro,cRefere,"CUENTA",cCodCta,.T.)
   ENDIF

   oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,nCol  ]:=nMonto

   EJECUTAR("DPPROVEETORIF",cCodPro)

   cNumero:=EJECUTAR("DPPROVEEDORPROGCREA",cTipDoc,cCodPro,cRefere,oCNDPLACTAXPRO:cPeriodo,;
                     oCNDPLACTAXPRO:dDesde,oCNDPLACTAXPRO:dHasta,cTipo,cCtaEgr,nMtoBs	,cNumero,cCodCta,nMonto,nValCam,oCNDPLACTAXPRO:cCodSuc,.T.)

   oCNDPLACTAXPRO:oBrw:Drawline(.t.)

   oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,13]:=cNumero

   aTotal:=ATOTALES(oCNDPLACTAXPRO:oBrw:aArrayData)

   AEVAL(oCNDPLACTAXPRO:oBrw:aCols,{|oCol,n|  IF(n>10,oCol:cFooter:=FDP(aTotal[n],oCol:cEditPicture),NIL)})

   oCNDPLACTAXPRO:oBrw:RefreshFooters()

RETURN .T.

FUNCTION ADDPROVEEDOR()
  LOCAL aLine  :=ACLONE(oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt])
  LOCAL nRowSel:=oCNDPLACTAXPRO:oBrw:nRowSel,nAt 
  
  EJECUTAR("DPCREAPROVEE",oCNDPLACTAXPRO:oBtnNew)

  IF !Empty(oDp:cCodigo)

    SQLUPDATE("DPPROVEEDOR","PRO_TIPO",aLine[3],"PRO_CODIGO"+GetWhere("=",oDp:cCodigo))

    aLine[01]:=SQLGET("DPPROVEEDOR","PRO_RIF,PRO_NOMBRE","PRO_CODIGO"+GetWhere("=",oDp:cCodigo))
    aLine[02]:=DPSQLROW(2)
    aLine[04]:=CTOEMPTY(aLine[4])
    aLine[05]:=CTOEMPTY(aLine[5])
    aLine[09]:=CTOEMPTY(aLine[9])
    aLine[11]:=0.00
 

    nAt:=oCNDPLACTAXPRO:oBrw:nArrayAt
    AINSERTAR(oCNDPLACTAXPRO:oBrw:aArrayData,nAt,aLine)
    oCNDPLACTAXPRO:oBrw:aArrayData[nAt]:=ACLONE(aLine)
    
    // oCNDPLACTAXPRO:oBrw:GoTop()
    oCNDPLACTAXPRO:oBrw:Refresh(.T.)
    oCNDPLACTAXPRO:oBrw:nArrayAt:=nAt // LEN(oCNDPLACTAXPRO:oBrw:aArrayData)
    oCNDPLACTAXPRO:oBrw:nRowSel :=nAt // nRowSel
    oCNDPLACTAXPRO:oBrw:nColSel:=4
   
  ENDIF

RETURN .T.

/*
// Agregar Nueva Linea
*/
FUNCTION CNDNEWLINE()
  LOCAL aLine  :=ACLONE(ATAIL(oCNDPLACTAXPRO:oBrw:aArrayData))

  IF !Empty(aLine[1])  
 
    AEVAL(aLine,{|a,n| aLine[n]:=CTOEMPTY(a)})

    aLine[07]:="GN"
    aLine[09]:="FAC"

    AADD(oCNDPLACTAXPRO:oBrw:aArrayData,aLine)

  ENDIF

//  aLine[08]:="GN"
//  aLine[09]:="Mensual"
//  aLine[11]:="FAC"

  oCNDPLACTAXPRO:oBrw:GoBottom()
  oCNDPLACTAXPRO:oBrw:nArrayAt:=LEN(oCNDPLACTAXPRO:oBrw:aArrayData)
  oCNDPLACTAXPRO:oBrw:nColSel:=1
  oCNDPLACTAXPRO:oBrw:DrawLine(.T.)
  DPFOCUS(oCNDPLACTAXPRO:oBrw)

RETURN .T.

FUNCTION EDITPROVEEDOR(nCol,lSave)
   LOCAL oBrw  :=oCNDPLACTAXPRO:oBrw,oLbx
   LOCAL nAt   :=oBrw:nArrayAt
   LOCAL uValue:=oBrw:aArrayData[oBrw:nArrayAt,nCol]

   oLbx:=DpLbx("DPPROVEEDOR.LBX") // ,NIL,"CTA_CODMOD"+GetWhere("=",oDp:cCtaMod)+" AND "+oCNDPLACTAXPRO:cWhereCta)
   oLbx:GetValue("PRO_CODIGO",oBrw:aCols[nCol],,,uValue)
   oCNDPLACTAXPRO:lAcction  :=.T.
   oBrw:nArrayAt:=nAt

   SysRefresh(.t.)

RETURN uValue

FUNCTION VALPROVEEDOR(oCol,uValue,nCol,nKey)
 LOCAL oBrw  :=oCNDPLACTAXPRO:oBrw
 LOCAL cTipDoc,oTable,cWhere:="",cCtaOld:="",cDescri
 LOCAL aLine:=oBrw:aArrayData[oBrw:nArrayAt],cWhere

 DEFAULT nKey:=0

 DEFAULT oCol:lButton:=.F.

 IF oCol:lButton=.T.
   oCol:lButton:=.F.
   RETURN .T.
 ENDIF

 // Activa el Campo para editar el Nombre
 oCol:oBrw:aCols[oCol:oBrw:nArrayAt,nCol+1]:nEditType:=0

 IF !ISSQLFIND("DPPROVEEDOR","PRO_CODIGO"+GetWhere("=",uValue))
    oCol:oBrw:aCols[nCol+1]:nEditType    :=IIF( oCNDPLACTAXPRO:lView, 0, 1)
    oCol:oBrw:nColSel:=nCol+1
    // RETURN .T.
 ENDIF

 cDescri:=SQLGET("DPPROVEEDOR","PRO_NOMBRE","PRO_CODIGO"+GetWhere("=",uValue))
 cDescri:=IF(Empty(cDescri),aLine[2],cDescri)

 oCNDPLACTAXPRO:lAcction:=.F.

 oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol  ]:=uValue
 oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol+1]:=cDescri
 oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol+2]:="Prestador de Servicios"
 oCol:oBrw:DrawLine(.T.)

RETURN .T.

/*
// Validar Nombre del Proveedor y lo guarda
*/
FUNCTION VALNOMBREPRO(oCol,uValue,nCol,nKey,NIL,lRefresh)
   LOCAL aLine  :=oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt]
   LOCAL cCodigo:=aLine[1],cCodPro:="",cTipo,nCant,cWhere,cTipPer:="N"

   IF Empty(uValue)
      RETURN .F.
   ENDIF

   // Si el codigo del proveedor esta vacio, busca el proveedor por el nombre
   cCodPro:=SQLGET("DPPROVEEDOR","PRO_CODIGO,PRO_TIPO","PRO_NOMBRE"+GetWhere("=",uValue))
   cTipo  :=DPSQLROW(2)

   // debo buscar segun nombre
   IF Empty(cCodigo) .AND. Empty(cCodPro)

     cWhere:=EJECUTAR("GETWHERELIKE","DPPROVEEDOR","PRO_NOMBRE",uValue,"")

     IF !Empty(cWhere)

        nCant  := COUNT("DPPROVEEDOR",cWhere)

        IF nCant=0 .OR. nCant=1

           cCodPro:=SQLINCREMENTAL("DPPROVEEDOR","PRO_CODIGO",[LEFT(PRO_CODIGO,1)<>"G"],NIL,NIL,NIL,10)
           cCodPro:=PADR(cCodPro,10)
           cTipo  :="Prestador de Servicios"
           cCodigo:=cCodPro

        ELSE

          cCodPro:=EJECUTAR("REPBDLIST","DPPROVEEDOR","PRO_CODIGO,PRO_NOMBRE",.F.,cWhere,NIL,NIL,uValue,NIL,NIL,"PRO_CODIGO") 

          IF !Empty(cCodPro)
            uValue :=SQLGET("DPPROVEEDOR","PRO_NOMBRE,PRO_TIPO","PRO_CODIGO"+GetWhere("=",cCodPro))
            cTipo  :=DPSQLROW(2)
            cTipo  :=IF(Empty(cTipo),"Prestador de Servicios",cTipo)
          ENDIF

        ENDIF

     ENDIF
   
   ENDIF

   IF !Empty(cCodPro)
      oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,nCol-1]:=cCodPro
      oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,nCol]  :=uValue
      oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,nCol+1]:=cTipo
      oCNDPLACTAXPRO:oBrw:nColSel:=oCNDPLACTAXPRO:oBrw:nColSel+2
   ELSE
     oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,nCol]:=uValue
   ENDIF

   oCNDPLACTAXPRO:oBrw:DrawLine(.T.)

   oCNDPLACTAXPRO:oBrw:nColSel:=oCNDPLACTAXPRO:oBrw:nColSel+1

   IF !Empty(cCodigo)

     aLine[3]:="Prestador de Servicios"
     cTipPer :=LEFT(cCodigo,1)
     cTipPer :=IF(cTipPer="V" .OR. ISDIGIT(cTipPer),"N",cTipPer)

     EJECUTAR("CREATERECORD","DPPROVEEDOR",{"PRO_CODIGO","PRO_NOMBRE" ,"PRO_SITUAC","PRO_TIPO","PRO_TIPPER","PRO_ZONANL"},;
                                           {cCodigo     ,uValue       ,"A"         ,aLine[3]  ,cTipPer     ,"N"},;
                                            NIL,.T.,"PRO_CODIGO"+GetWhere("=",cCodigo))

     // agrega en la tabla DPRIF
     EJECUTAR("DPPROVEETORIF",cCodigo)

   ENDIF

   oCNDPLACTAXPRO:PUTMTODIV(aLine[10],10) // Inserta el registro con valor Cero

RETURN .T.

FUNCTION VALCTAEGR(oCol,uValue,nCol,nKey)
  LOCAL cTipDoc,oTable,cWhere:="",cCtaOld:="",cDescri,aLine:={},cWhere

  DEFAULT nKey:=0

  DEFAULT oCol:lButton:=.F.

  IF oCol:lButton=.T.
    oCol:lButton:=.F.
    RETURN .T.
  ENDIF

  oCol:oBrw:aCols[nCol+1]:nEditType    :=0

  IF !ISSQLFIND("DPCTAEGRESO","CEG_CODIGO"+GetWhere("=",uValue))
    oCol:oBrw:aCols[nCol+1]:nEditType    :=1
    oCol:oBrw:nColSel:=nCol+1
  ENDIF

  cDescri:=SQLGET("DPCTAEGRESO","CEG_DESCRI","CEG_CODIGO"+GetWhere("=",uValue))

  oCNDPLACTAXPRO:lAcction:=.F.

  oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol]:=uValue
  oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol+1]:=cDescri
  oCol:oBrw:DrawLine(.T.)

RETURN .T.

/*
// Validar Nombre del Proveedor y lo guarda
*/
FUNCTION VALNOMBREEGR(oCol,uValue,nCol,nKey,NIL,lRefresh)
   LOCAL aLine  :=oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt]
   LOCAL cCodigo:=aLine[nCol-1]

   IF Empty(uValue)
      RETURN .F.
   ENDIF

   oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,nCol]:=uValue
   oCNDPLACTAXPRO:oBrw:DrawLine(.T.)
   oCNDPLACTAXPRO:oBrw:nColSel++

   EJECUTAR("CREATERECORD","DPCTAEGRESO",{"CEG_CODIGO","CEG_DESCRI" ,"CEG_CUENTA"   ,"CEG_ACTIVO","CEG_EGRES"},;
                                         {cCodigo   ,uValue     ,oDp:cCtaIndef,.T.       ,.T.      },;
                                           NIL,.T.,"CEG_CODIGO"+GetWhere("=",cCodigo))

   oCNDPLACTAXPRO:PUTMTODIV(aLine[10],10) // Inserta el registro con valor Cero

RETURN .T.

FUNCTION VALREFERE(uValue,nCol)
   LOCAL aLine  :=oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt]
   LOCAL cCodigo:=aLine[nCol-1]
   LOCAL cCtaEgr

   IF !Empty(uValue) .AND. Empty(aLine[nCol+1]) .AND. oCNDPLACTAXPRO:lCtaEgreso
      cCtaEgr:=UPPER(ALLTRIM(uValue))
      cCtaEgr:=PADR(STRTRAN(cCtaEgr," ","_"),20)
      oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,nCol+1]:=cCtaEgr
      oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,nCol+2]:=uValue
      oCNDPLACTAXPRO:oBrw:aArrayData[oCNDPLACTAXPRO:oBrw:nArrayAt,08    ]:=uValue
   ENDIF

RETURN .T.


// EOF

