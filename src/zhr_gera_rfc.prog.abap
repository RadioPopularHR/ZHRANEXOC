*&---------------------------------------------------------------------*
*& Report  YHR00128REPPG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zhr_gera_rfc.
**********************************************************************
* Variáveis auxiliares
**********************************************************************
TABLES: t001.

DATA: t_anexoc   TYPE TABLE OF zanexo_rfc_xml WITH HEADER LINE,
      t_anexorfc TYPE TABLE OF zanexo_rfc WITH HEADER LINE,
      lt_anexoc  TYPE zanexo_rfc_xml.

DATA: BEGIN OF t_forma OCCURS 0        ,
        area    TYPE zanexo_rfc-area,
        modform TYPE zanexo_rfc-modform,
        horas   TYPE zanexo_rfc-horas,
        entform TYPE zanexo_rfc-entform,
        nivqual TYPE zanexo_rfc-nivqual,
        registo TYPE numc5,
      END OF t_forma.

DATA: BEGIN OF t_regemp OCCURS 0,
        nissemp  TYPE zanexo_rfc-nissemp,
        situacao TYPE zanexo_rfc-situacao,
        area     TYPE zanexo_rfc-area,
        modform  TYPE zanexo_rfc-modform,
        horas    TYPE zanexo_rfc-horas,
        entform  TYPE zanexo_rfc-entform,
        nivqual  TYPE zanexo_rfc-nivqual,
        numordem TYPE zanexo_rfc-numordem,
        iniform  TYPE zanexo_rfc-iniform,
        horform  TYPE zanexo_rfc-horform,
        tpcert   TYPE zanexo_rfc-tpcert,
        periodo  TYPE zanexo_rfc-periodo,
      END OF t_regemp.

DATA: BEGIN OF t_pernr OCCURS 0,
        nissemp  TYPE zanexo_rfc-nissemp,
        regime   TYPE zanexo_rfc-regime,
        niss     TYPE zanexo_rfc-niss,
        nome     TYPE zanexo_rfc-nome,
        situacao TYPE zanexo_rfc-situacao,
      END OF t_pernr.

DATA: BEGIN OF t_tabint OCCURS 0,
        nif      LIKE zanexo_rfc-nif,
        niss     LIKE zanexo_rfc-niss,
        ano      LIKE zanexo_rfc-ano,
        msg(100),
      END OF t_tabint.

DATA: BEGIN OF t_xml OCCURS 0,
        linha(255),
      END OF t_xml.

DATA: BEGIN OF t_output OCCURS 0,
        bukrs    LIKE p0001-bukrs,
        ano      LIKE zanexo_rfc-ano,
        msg(200),
      END OF t_output.

FIELD-SYMBOLS : <campo> TYPE ANY.
DATA: auxnome(30).

DATA: feature      LIKE t549d-namen VALUE 'PENTT',
      entty        LIKE t5pfd-entty,
      umsretcode   TYPE i,
      lv_entid(8),
      lv_compa(50),
      lv_codta(8).

**********************************************************************
* VARIÁVEIS PARA ALV
**********************************************************************
DATA:
  grid_control TYPE REF TO cl_gui_alv_grid,
  container    TYPE REF TO cl_gui_custom_container,
  control      TYPE REF TO i_oi_container_control,
  gv_variant   LIKE disvariant,
  gt_header    TYPE kkblo_t_listheader,
  gt_events    TYPE slis_t_event,
  gs_layout    TYPE slis_layout_alv,
  gt_fieldcat  TYPE slis_t_fieldcat_alv,
  gs_variant   TYPE disvariant,
  gs_sort      TYPE slis_t_sortinfo_alv.
**********************************************************************
* Variaveis especiais
**********************************************************************
DATA: l_action   TYPE i,                " Benutzeraktion
      l_filetab  TYPE filetable,        " Tabelle mit Dateinamen
      lw_filetab TYPE LINE OF filetable,
      l_rc       TYPE i,                " Anzahl gefundener
      file_path  TYPE string.
TYPE-POOLS truxs.
DATA fw_tab_raw  TYPE truxs_t_text_data.
**********************************************************************
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-tit.
PARAMETERS : p_bukrs TYPE p0001-bukrs OBLIGATORY.
PARAMETERS : p_ano TYPE zanexo_rfc-ano OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS : p_xml TYPE string OBLIGATORY.
SELECTION-SCREEN : END OF BLOCK b1.

AT SELECTION-SCREEN.
  IF p_bukrs IS NOT INITIAL.
    SELECT SINGLE * FROM t001 WHERE
      bukrs = p_bukrs.
    IF sy-subrc <> 0.
      MESSAGE e001(zpg) WITH 'Empresa não existe'.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_xml.
  IF p_xml IS NOT INITIAL.
    MOVE p_xml TO file_path.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Importar registro de file local'
      default_filename        = file_path
      default_extension       = 'xml'
      file_filter             = '(*.xml)|*.xml|(*.*)|*.*'
      initial_directory       = file_path
      multiselection          = abap_false
    CHANGING
      file_table              = l_filetab
      rc                      = l_rc
      user_action             = l_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.
  IF sy-subrc = 0.
    READ TABLE l_filetab INDEX 1 INTO lw_filetab.
    p_xml = lw_filetab-filename.
  ENDIF.

START-OF-SELECTION.
  REFRESH: t_output, t_xml, t_forma.

  CLEAR : lv_entid, lv_compa, lv_codta.
  PERFORM entidade CHANGING lv_entid
                            lv_compa.
* Cabeçalho

  PERFORM cabecalho.

  PERFORM dados CHANGING lv_codta.

  PERFORM corpo.
*
  PERFORM footer.

  IF t_xml[] IS INITIAL.
    MESSAGE i001(ypg) WITH 'Não existem dados'.
  ENDIF.

  CHECK t_xml[] IS NOT INITIAL.

* * * ROFF SAM MGA/MN 7000052499-->
*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      filename = p_xml
**      filetype = 'BIN'
*      filetype = 'ASC'
*      dat_mode = 'X'
*    TABLES
*      data_tab = t_xml.

  DATA: line TYPE string.
  CLEAR line.
  LOOP AT t_xml.
    CONCATENATE line t_xml INTO line.
  ENDLOOP.

  DATA:
    fich   TYPE localfile,
    ld_rc  TYPE i,
    lo_doc TYPE REF TO cl_xml_document.

  CREATE OBJECT lo_doc.

  " Parse XML string
  CALL METHOD lo_doc->parse_string
    EXPORTING
      stream  = line
    RECEIVING
      retcode = ld_rc.

  fich = p_xml.

  CALL METHOD lo_doc->export_to_file
    EXPORTING
      filename = fich
    RECEIVING
      retcode  = ld_rc.

* * * ROFF SAM MGA/MN 7000052499<--
  IF sy-subrc = 0.
    PERFORM listagem.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cabecalho .

  SELECT * INTO TABLE t_anexoc FROM zanexo_rfc_xml WHERE
    tipo = 'H' AND campo = p_ano.
*** ROFF RS/Debora Franco
*** 20130412
*** Obrigatorio preencher a tabela com as linhas todas
*** relativas ao ano em analise
*** nao vai lerlinhas genericas ( sem o ano )
***  IF sy-subrc NE 0.
***    SELECT * INTO TABLE t_anexoc FROM zanexo_rfc_xml WHERE
***    tipo = 'H'.
***  ENDIF.
  SORT t_anexoc BY tipo seqnum ASCENDING.

  LOOP AT t_anexoc INTO lt_anexoc.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDLOOP.

ENDFORM.                    " CABECALHO
*&---------------------------------------------------------------------*
*&      Form  CORPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM corpo .

  SELECT * INTO TABLE t_anexoc FROM zanexo_rfc_xml WHERE
    tipo = 'B'.
* >>> INI ROFF SAM HR MN/EMP 7000052847 12.04.2018
*  SORT t_anexoc BY tipo seqnum ASCENDING.
  SORT t_anexoc BY tipo seqnum campo ASCENDING.
* <<< END ROFF SAM HR MN/EMP 7000052847 12.04.2018

* Inicio
  PERFORM inicio_corpo.
* Accoes formação
  PERFORM cae.
* Formacoes
  PERFORM formacoes.
* Empregados
  PERFORM empregados.
* Fim
  PERFORM fim_corpo.

ENDFORM.                    " CORPO
*&---------------------------------------------------------------------*
*&      Form  FOOTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM footer .

  SELECT * INTO TABLE t_anexoc FROM zanexo_rfc_xml WHERE
    tipo = 'F'.
  SORT t_anexoc BY tipo seqnum ASCENDING.

  LOOP AT t_anexoc INTO lt_anexoc.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDLOOP.

ENDFORM.                    " FOOTER
*&---------------------------------------------------------------------*
*&      Form  CARACTERES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_VAL2  text
*----------------------------------------------------------------------*
FORM caracteres  CHANGING p_descricao.

  REPLACE ALL OCCURRENCES OF 'á' IN p_descricao WITH 'a'.
  REPLACE ALL OCCURRENCES OF 'à' IN p_descricao WITH 'a'.
  REPLACE ALL OCCURRENCES OF 'ã' IN p_descricao WITH 'a'.
  REPLACE ALL OCCURRENCES OF 'â' IN p_descricao WITH 'a'.
  REPLACE ALL OCCURRENCES OF 'ä' IN p_descricao WITH 'a'.
  REPLACE ALL OCCURRENCES OF 'é' IN p_descricao WITH 'e'.
  REPLACE ALL OCCURRENCES OF 'è' IN p_descricao WITH 'e'.
  REPLACE ALL OCCURRENCES OF 'ê' IN p_descricao WITH 'e'.
  REPLACE ALL OCCURRENCES OF 'ë' IN p_descricao WITH 'e'.
  REPLACE ALL OCCURRENCES OF 'í' IN p_descricao WITH 'i'.
  REPLACE ALL OCCURRENCES OF 'ì' IN p_descricao WITH 'i'.
  REPLACE ALL OCCURRENCES OF 'î' IN p_descricao WITH 'i'.
  REPLACE ALL OCCURRENCES OF 'ï' IN p_descricao WITH 'i'.
  REPLACE ALL OCCURRENCES OF 'ó' IN p_descricao WITH 'o'.
  REPLACE ALL OCCURRENCES OF 'ò' IN p_descricao WITH 'o'.
  REPLACE ALL OCCURRENCES OF 'ô' IN p_descricao WITH 'o'.
  REPLACE ALL OCCURRENCES OF 'õ' IN p_descricao WITH 'o'.
  REPLACE ALL OCCURRENCES OF 'ö' IN p_descricao WITH 'o'.
  REPLACE ALL OCCURRENCES OF 'ú' IN p_descricao WITH 'u'.
  REPLACE ALL OCCURRENCES OF 'ù' IN p_descricao WITH 'u'.
  REPLACE ALL OCCURRENCES OF 'û' IN p_descricao WITH 'u'.
  REPLACE ALL OCCURRENCES OF 'ü' IN p_descricao WITH 'u'.
  REPLACE ALL OCCURRENCES OF 'Á' IN p_descricao WITH 'A'.
  REPLACE ALL OCCURRENCES OF 'À' IN p_descricao WITH 'A'.
  REPLACE ALL OCCURRENCES OF 'Ã' IN p_descricao WITH 'A'.
  REPLACE ALL OCCURRENCES OF 'Â' IN p_descricao WITH 'A'.
  REPLACE ALL OCCURRENCES OF 'Ä' IN p_descricao WITH 'A'.
  REPLACE ALL OCCURRENCES OF 'É' IN p_descricao WITH 'E'.
  REPLACE ALL OCCURRENCES OF 'È' IN p_descricao WITH 'E'.
  REPLACE ALL OCCURRENCES OF 'Ê' IN p_descricao WITH 'E'.
  REPLACE ALL OCCURRENCES OF 'Ë' IN p_descricao WITH 'E'.
  REPLACE ALL OCCURRENCES OF 'Í' IN p_descricao WITH 'I'.
  REPLACE ALL OCCURRENCES OF 'Ì' IN p_descricao WITH 'I'.
  REPLACE ALL OCCURRENCES OF 'Î' IN p_descricao WITH 'I'.
  REPLACE ALL OCCURRENCES OF 'Ï' IN p_descricao WITH 'I'.
  REPLACE ALL OCCURRENCES OF 'Ó' IN p_descricao WITH 'O'.
  REPLACE ALL OCCURRENCES OF 'Ò' IN p_descricao WITH 'O'.
  REPLACE ALL OCCURRENCES OF 'Ô' IN p_descricao WITH 'O'.
  REPLACE ALL OCCURRENCES OF 'Õ' IN p_descricao WITH 'O'.
  REPLACE ALL OCCURRENCES OF 'Ö' IN p_descricao WITH 'O'.
  REPLACE ALL OCCURRENCES OF 'Ú' IN p_descricao WITH 'U'.
  REPLACE ALL OCCURRENCES OF 'Ù' IN p_descricao WITH 'U'.
  REPLACE ALL OCCURRENCES OF 'Û' IN p_descricao WITH 'U'.
  REPLACE ALL OCCURRENCES OF 'Ü' IN p_descricao WITH 'U'.
  REPLACE ALL OCCURRENCES OF 'ç' IN p_descricao WITH 'c'.
  REPLACE ALL OCCURRENCES OF 'Ç' IN p_descricao WITH 'C'.
  REPLACE ALL OCCURRENCES OF '''' IN p_descricao WITH ' '.
  REPLACE ALL OCCURRENCES OF '´' IN p_descricao WITH ' '.
  REPLACE ALL OCCURRENCES OF '`' IN p_descricao WITH ' '.
  REPLACE ALL OCCURRENCES OF '#' IN p_descricao WITH 'E'.
  REPLACE ALL OCCURRENCES OF 'º' IN p_descricao WITH ' '.
  REPLACE ALL OCCURRENCES OF 'ª' IN p_descricao WITH ' '.

ENDFORM.                    " CARACTERES
*&---------------------------------------------------------------------*
*&      Form  DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dados CHANGING p_codta.
  DATA: lv_stceg TYPE t001-stceg,
        lv_nif   TYPE zanexo_rfc-nif,
        lv_cont  TYPE numc3.

  CLEAR : lv_stceg, lv_nif.
  SELECT SINGLE stceg INTO lv_stceg FROM t001 WHERE
    bukrs = p_bukrs.
  CHECK sy-subrc = 0.

  lv_nif = lv_stceg+2(9).

  SELECT * INTO TABLE t_anexorfc FROM zanexo_rfc WHERE
    nif = lv_nif AND
    ano = p_ano.
  CHECK sy-subrc = 0.

  READ TABLE t_anexorfc INDEX 1.
  p_codta = t_anexorfc-cae.

  SORT t_anexorfc BY nif niss ano colab nissemp situacao numordem.

  REFRESH : t_forma, t_regemp, t_pernr.
  LOOP AT t_anexorfc.
*    IF t_anexorfc-area IS NOT INITIAL.
*      MOVE-CORRESPONDING t_anexorfc TO t_forma.
*      APPEND t_forma.
*    ENDIF.

    MOVE-CORRESPONDING t_anexorfc TO t_regemp.
    APPEND t_regemp.

    MOVE-CORRESPONDING t_anexorfc TO t_pernr.
    APPEND t_pernr.
  ENDLOOP.

*  SORT t_forma.
*  DELETE ADJACENT DUPLICATES FROM t_forma.
*  lv_cont = 1.
*  LOOP AT t_forma.
*    t_forma-registo = lv_cont.
*    MODIFY t_forma.
*    lv_cont = lv_cont + 1.
*  ENDLOOP.

*  SORT t_regemp.
*  DELETE ADJACENT DUPLICATES FROM t_regemp.

  SORT t_pernr.
  DELETE ADJACENT DUPLICATES FROM t_pernr.

  SORT t_anexorfc.

ENDFORM.                    " DADOS
*&---------------------------------------------------------------------*
*&      Form  INICIO_CORPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inicio_corpo .

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 1 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 2 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 3 campo = p_ano BINARY SEARCH.
  IF sy-subrc = 0.
    CONDENSE lv_entid.
    CONCATENATE lt_anexoc-abrir lv_entid lt_anexoc-fechar INTO
      t_xml-linha.
    APPEND t_xml.
  ENDIF.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 4 BINARY SEARCH.
  IF sy-subrc = 0.
    PERFORM caracteres CHANGING lv_compa.
    CONDENSE lv_compa.
    CONCATENATE lt_anexoc-abrir lv_compa lt_anexoc-fechar INTO
      t_xml-linha.
    APPEND t_xml.
  ENDIF.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 5 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 6 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

ENDFORM.                    " INICIO_CORPO
*&---------------------------------------------------------------------*
*&      Form  FIM_CORPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fim_corpo .

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 9996 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 9997 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 9998 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 9999 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

ENDFORM.                    " FIM_CORPO
*&---------------------------------------------------------------------*
*&      Form  FORMACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM formacoes .
  DATA: lv_horas(10),
        lv_horas1(8),
        lv_horas2(3),
        lc_formacao LIKE t_forma.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 8 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

  DATA: form_index LIKE sy-tabix.
  form_index = 0.
*  LOOP AT t_forma.
  LOOP AT t_regemp.

    MOVE-CORRESPONDING t_regemp TO t_forma.
    CHECK t_forma-area IS NOT INITIAL.

* FJHU - Maio 2012
* Validar se a formação já foi inserida
    READ TABLE t_forma INTO lc_formacao
                       WITH KEY area    = t_regemp-area
                                modform = t_regemp-modform
                                horas   = t_regemp-horas
                                entform = t_regemp-entform
                                nivqual = t_regemp-nivqual
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      CONTINUE.
    ELSE.
      ADD 1 TO form_index.
      t_forma-registo = form_index.

      APPEND t_forma.
      SORT t_forma BY area modform horas entform nivqual.
* Inicio Formacoes
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
          tipo = 'B' seqnum = 9 BINARY SEARCH.
      IF sy-subrc = 0.
        t_xml-linha = lt_anexoc-linha.
        APPEND t_xml.
      ENDIF.

      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
          tipo = 'B' seqnum = 10 BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE lt_anexoc-abrir t_forma-area lt_anexoc-fechar INTO
          t_xml-linha.
        APPEND t_xml.
      ENDIF.

      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
          tipo = 'B' seqnum = 11 BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE lt_anexoc-abrir t_forma-modform lt_anexoc-fechar
          INTO t_xml-linha.
        APPEND t_xml.
      ENDIF.

      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
          tipo = 'B' seqnum = 12 BINARY SEARCH.
      IF sy-subrc = 0.
        CLEAR lv_horas.
        lv_horas = t_forma-horas.
        SPLIT lv_horas AT '.' INTO lv_horas1 lv_horas2.

        DATA: xnhour TYPE nhours.
        xnhour = t_forma-horas.
        IF lv_horas1 > 0.
          WRITE xnhour TO lv_horas DECIMALS 0 LEFT-JUSTIFIED.
*        lv_horas = lv_horas1.
        ELSE.
          lv_horas = '1'.
        ENDIF.
        CONDENSE lv_horas.
        CONCATENATE lt_anexoc-abrir lv_horas lt_anexoc-fechar INTO
          t_xml-linha.
        APPEND t_xml.
      ENDIF.

      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
          tipo = 'B' seqnum = 13 BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE lt_anexoc-abrir t_forma-entform lt_anexoc-fechar
        INTO
          t_xml-linha.
        APPEND t_xml.
      ENDIF.

      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
          tipo = 'B' seqnum = 14 BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE lt_anexoc-abrir t_forma-nivqual lt_anexoc-fechar
        INTO
          t_xml-linha.
        APPEND t_xml.
      ENDIF.

* Fim Formações
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
          tipo = 'B' seqnum = 15 BINARY SEARCH.
      IF sy-subrc = 0.
        t_xml-linha = lt_anexoc-linha.
        APPEND t_xml.
      ENDIF.
    ENDIF.
  ENDLOOP.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 16 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

ENDFORM.                    " FORMACOES
*&---------------------------------------------------------------------*
*&      Form  CAE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cae .

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 7 BINARY SEARCH.
  IF sy-subrc = 0.
    CONDENSE lv_codta.
    CONCATENATE lt_anexoc-abrir lv_codta lt_anexoc-fechar INTO
      t_xml-linha.
    APPEND t_xml.
  ENDIF.

ENDFORM.                    " CAE
*&---------------------------------------------------------------------*
*&      Form  EMPREGADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM empregados .
  DATA: lv_ssnum       TYPE p0332-ssnum,
        lv_numaccao(5).

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 17 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

  SORT t_forma BY area ASCENDING
                  modform ASCENDING
                  horas ASCENDING
                  entform ASCENDING
                  nivqual ASCENDING.

  LOOP AT t_pernr.
* Inicio
    READ TABLE t_anexoc INTO lt_anexoc WITH KEY
        tipo = 'B' seqnum = 18 BINARY SEARCH.
    IF sy-subrc = 0.
      t_xml-linha = lt_anexoc-linha.
      APPEND t_xml.
    ENDIF.
*
    READ TABLE t_anexoc INTO lt_anexoc WITH KEY
        tipo = 'B' seqnum = 19 BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE lt_anexoc-abrir t_pernr-regime lt_anexoc-fechar
      INTO
        t_xml-linha.
      APPEND t_xml.
    ENDIF.
*
    READ TABLE t_anexoc INTO lt_anexoc WITH KEY
        tipo = 'B' seqnum = 20 BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE lt_anexoc-abrir t_pernr-nissemp lt_anexoc-fechar
      INTO
        t_xml-linha.
      APPEND t_xml.
    ENDIF.
*
    READ TABLE t_anexoc INTO lt_anexoc WITH KEY
        tipo = 'B' seqnum = 21 BINARY SEARCH.
    IF sy-subrc = 0.
      PERFORM caracteres CHANGING t_pernr-nome.
      CONDENSE t_anexorfc-nome.
      CONCATENATE lt_anexoc-abrir t_pernr-nome lt_anexoc-fechar INTO
        t_xml-linha.
      APPEND t_xml.
    ENDIF.
*
    READ TABLE t_anexoc INTO lt_anexoc WITH KEY
        tipo = 'B' seqnum = 22 BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE lt_anexoc-abrir t_pernr-situacao lt_anexoc-fechar
      INTO
        t_xml-linha.
      APPEND t_xml.
    ENDIF.
* Registos
    READ TABLE t_anexoc INTO lt_anexoc WITH KEY
        tipo = 'B' seqnum = 23 BINARY SEARCH.
    IF sy-subrc = 0.
      t_xml-linha = lt_anexoc-linha.
      APPEND t_xml.
    ENDIF.
*
    DATA: form_index LIKE sy-tabix.
*    form_index = 0.
    LOOP AT t_regemp WHERE nissemp  = t_pernr-nissemp AND
                           situacao = t_pernr-situacao.

      IF t_regemp-area IS INITIAL.
        CONTINUE.
      ENDIF.
*      t_forma-registo = sy-tabix.
      ADD 1 TO form_index.
*      t_forma-registo = form_index.

      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 24 BINARY SEARCH.
      IF sy-subrc = 0.
        t_xml-linha = lt_anexoc-linha.
        APPEND t_xml.
      ENDIF.
* FJHU - Novamente activação da busca
      READ TABLE t_forma WITH KEY area    = t_regemp-area
                                  modform = t_regemp-modform
                                  horas   = t_regemp-horas
                                  entform = t_regemp-entform
                                  nivqual = t_regemp-nivqual
                                  BINARY SEARCH.

      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 25 BINARY SEARCH.
      IF sy-subrc = 0.
        WRITE t_forma-registo TO lv_numaccao NO-ZERO.
*        lv_numaccao = t_regemp-numordem.
        CONDENSE lv_numaccao.
        CONCATENATE lt_anexoc-abrir lv_numaccao lt_anexoc-fechar INTO
          t_xml-linha.
        APPEND t_xml.
      ENDIF.
*
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 26 BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE lt_anexoc-abrir t_regemp-iniform lt_anexoc-fechar
         INTO t_xml-linha.
        APPEND t_xml.
      ENDIF.
*
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 27 BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE lt_anexoc-abrir t_regemp-horform lt_anexoc-fechar
        INTO
          t_xml-linha.
        APPEND t_xml.
      ENDIF.
*
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 28 BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE lt_anexoc-abrir t_regemp-tpcert lt_anexoc-fechar
        INTO
          t_xml-linha.
        APPEND t_xml.
      ENDIF.
*
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 29 BINARY SEARCH.
      IF sy-subrc = 0.
        t_xml-linha = lt_anexoc-linha.
        APPEND t_xml.
      ENDIF.
*
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 30 BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE lt_anexoc-abrir t_regemp-periodo lt_anexoc-fechar
        INTO
          t_xml-linha.
        APPEND t_xml.
      ENDIF.
*
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 31 BINARY SEARCH.
      IF sy-subrc = 0.
        t_xml-linha = lt_anexoc-linha.
        APPEND t_xml.
      ENDIF.
*
      READ TABLE t_anexoc INTO lt_anexoc WITH KEY
            tipo = 'B' seqnum = 32 BINARY SEARCH.
      IF sy-subrc = 0.
        t_xml-linha = lt_anexoc-linha.
        APPEND t_xml.
      ENDIF.
    ENDLOOP.
*
    READ TABLE t_anexoc INTO lt_anexoc WITH KEY
        tipo = 'B' seqnum = 33 BINARY SEARCH.
    IF sy-subrc = 0.
      t_xml-linha = lt_anexoc-linha.
      APPEND t_xml.
    ENDIF.
* Fim
    READ TABLE t_anexoc INTO lt_anexoc WITH KEY
        tipo = 'B' seqnum = 34 BINARY SEARCH.
    IF sy-subrc = 0.
      t_xml-linha = lt_anexoc-linha.
      APPEND t_xml.
    ENDIF.
  ENDLOOP.

  READ TABLE t_anexoc INTO lt_anexoc WITH KEY
    tipo = 'B' seqnum = 35 BINARY SEARCH.
  IF sy-subrc = 0.
    t_xml-linha = lt_anexoc-linha.
    APPEND t_xml.
  ENDIF.

ENDFORM.                    " EMPREGADOS
*&---------------------------------------------------------------------*
*&      Form  ENTIDADE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM entidade CHANGING p_entid
                       p_compa.

  DATA: t_5pfd TYPE TABLE OF t5pfd WITH HEADER LINE.
  TABLES: pme02.

  CLEAR pme02.
  pme02-bukrs = p_bukrs.
  PERFORM re549d USING feature space entty umsretcode.

  SELECT * FROM t5pfd INTO TABLE t_5pfd WHERE
    entty EQ entty AND
    begda LE sy-datum   AND
    endda GE sy-datum.

  LOOP AT t_5pfd.
    IF t_5pfd-field = 'ENTID'.
      p_entid = t_5pfd-fdata.
    ELSEIF t_5pfd-field = 'COMPA'.
      p_compa = t_5pfd-fdata.
*    ELSEIF t_5pfd-field = 'MACTC'.
*      p_codta = t_5pfd-fdata.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ENTIDADE
*&---------------------------------------------------------------------*
*&      Form  RE549D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FEATURE  text
*      -->P_SPACE  text
*      -->P_ENTTY  text
*      -->P_UMSRETCODE  text
*----------------------------------------------------------------------*
FORM re549d  USING
            merkmal      TYPE c
            kind_of_error
            back
            status.
*---------------------------------------------------------------------*
  DATA          struc(5).              " VLDAHRK006111
  DATA          feature LIKE t549b-namen.               " VLDAHRK006111
  FIELD-SYMBOLS <struc_content>.       " VLDAHRK006111
*---------------------------------------------------------------------*
  feature = merkmal.
  SELECT SINGLE struc FROM  t549d INTO struc
         WHERE  namen       = merkmal.
  IF sy-subrc NE 0.
*-- Merkmal existiert nicht; d.h. es ist nicht generiert
    status = 4.
  ELSE.
    ASSIGN (struc) TO <struc_content>.
    IF sy-subrc NE 0 AND ( kind_of_error = space OR
                           kind_of_error = 1 ).
      status = 8.
    ELSEIF sy-subrc NE 0 AND kind_of_error = 2.
      MESSAGE i568(p0).
*   Merkmalstruktur nicht bekannt, bitte Langdokumentation lesen.
    ELSEIF sy-subrc NE 0 AND kind_of_error = 3.
      MESSAGE s568(p0).
    ELSEIF sy-subrc NE 0 AND kind_of_error = 4.
      MESSAGE e568(p0).
    ELSE.
      CLEAR back.                                           "VLDN185055
      CALL FUNCTION 'HR_FEATURE_BACKFIELD'
        EXPORTING
          feature       = feature
          struc_content = <struc_content>
          kind_of_error = kind_of_error
        IMPORTING
          back          = back
        CHANGING
          status        = status
        EXCEPTIONS
*         DUMMY         = 1          "VLDAHRK039762
*         ERROR_OPERATION             = 2          "VLDAHRK039762
*         NO_BACKVALUE  = 3          "VLDAHRK039762
*         FEATURE_NOT_GENERATED       = 4          "VLDAHRK039762
*         INVALID_SIGN_IN_FUNID       = 5          "VLDAHRK039762
*         FIELD_IN_REPORT_TAB_IN_PE03 = 6          "VLDAHRK039762
*         OTHERS        = 0.    "VLDAL0K095544 (Checkman)
          OTHERS        = 1.    "VLDAL0K095544 (Checkman)
      IF sy-subrc <> 0.
*     wg. SLIN
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                                                    " RE549D
*&---------------------------------------------------------------------*
*&      Form  LISTAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM listagem .

  t_output-bukrs = p_bukrs.
  t_output-ano   = p_ano.
  CONCATENATE 'Foi gerado o ficheiro' p_xml INTO t_output-msg.
  APPEND t_output.

* Eventos
  PERFORM get_event CHANGING gt_events.
* Cabeçalho
  PERFORM header.
* Catalogação dos campos
  PERFORM get_fields CHANGING gt_fieldcat.
* Ordenação
  PERFORM get_layout_sort CHANGING gs_layout gs_sort.
* Imprime dados
  PERFORM print_data.

ENDFORM.                    " LISTAGEM
*&---------------------------------------------------------------------*
*&      Form  GET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EVENTS  text
*--------------------------------------------q--------------------------
**
FORM get_event  CHANGING ie_events TYPE slis_t_event.

  DATA ls_events TYPE slis_alv_event.

  ls_events-name = slis_ev_user_command.
  ls_events-form = 'USER_COMMAND'.
  APPEND ls_events TO ie_events.

  ls_events-name = slis_ev_pf_status_set.
  ls_events-form = 'ALV_ST'.
  APPEND ls_events TO ie_events.

ENDFORM.                    " GET_EVENT
*&---------------------------------------------------------------------*
*&      Form  HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header .
  DATA : lv_tit(60),
         lv_tit1(60),
         lv_tit2(60),
         lv_auxdt(10),
         lv_auxdt2(10).

  DATA: fs_list_commentary TYPE slis_listheader,
        fv_datum(10),
        fv_uzeit(8),
        fv_info            TYPE slis_entry.

  REFRESH gt_header.
  CLEAR   gt_header.

*  WRITE g_begda TO lv_auxdt DD/MM/YYYY.
*  WRITE g_endda TO lv_auxdt2 DD/MM/YYYY.
  lv_tit1 = 'Geração Anexo C - XML'.

  CLEAR fs_list_commentary.
  fs_list_commentary-typ  = 'H'.
  fs_list_commentary-info = lv_tit1.
  APPEND fs_list_commentary TO gt_header.


ENDFORM.                    " HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM get_fields  CHANGING t_fieldcat TYPE slis_t_fieldcat_alv.
  FIELD-SYMBOLS <ffs_fieldcat> TYPE slis_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_internal_tabname     = 'T_OUTPUT'
      i_inclname             = sy-cprog
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.


  LOOP AT t_fieldcat ASSIGNING <ffs_fieldcat>.
    CASE <ffs_fieldcat>-fieldname.

      WHEN 'BUKRS'.
        <ffs_fieldcat>-seltext_l    = 'Empresa'.
        <ffs_fieldcat>-seltext_m    = 'Empresa'.
        <ffs_fieldcat>-seltext_s    = 'Empresa'.
        <ffs_fieldcat>-reptext_ddic = 'Empresa'.
        <ffs_fieldcat>-col_pos = sy-tabix.

      WHEN 'ANO'.
        <ffs_fieldcat>-seltext_l    = 'Ano'.
        <ffs_fieldcat>-seltext_m    = 'Ano'.
        <ffs_fieldcat>-seltext_s    = 'Ano'.
        <ffs_fieldcat>-reptext_ddic = 'Ano'.
        <ffs_fieldcat>-col_pos = sy-tabix.

      WHEN 'MSG'.
        <ffs_fieldcat>-seltext_l    = 'Observações'.
        <ffs_fieldcat>-seltext_m    = 'Observações'.
        <ffs_fieldcat>-seltext_s    = 'Observações'.
        <ffs_fieldcat>-reptext_ddic = 'Observações'.
        <ffs_fieldcat>-col_pos = sy-tabix.

      WHEN OTHERS.
        <ffs_fieldcat>-no_out = 'X'.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    " GET_FIELDS
*&---------------------------------------------------------------------*
*&      Form  GET_LAYOUT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*      <--P_GS_SORT  text
*----------------------------------------------------------------------*
FORM get_layout_sort  CHANGING s_layout TYPE slis_layout_alv
                               s_sort   TYPE slis_t_sortinfo_alv.

  DATA l_sort TYPE slis_sortinfo_alv.

** Layout
  s_layout-get_selinfos        = 'X'.
  s_layout-key_hotspot         = 'X'.
  s_layout-totals_before_items = ' '.
  s_layout-group_change_edit   = 'X'.
  s_layout-detail_popup        = 'X'.
  s_layout-zebra               = 'X'.
  s_layout-colwidth_optimize   = 'X'.
  s_layout-numc_sum            = 'X'.

*  l_sort-spos = 1.
*  l_sort-fieldname = 'PERNR'.
*  l_sort-up = 'X'.
*  APPEND l_sort TO s_sort.
*
*  l_sort-spos = 2.
*  l_sort-fieldname = 'BEGDA'.
*  l_sort-up = 'X'.
*  APPEND l_sort TO s_sort.
*  s_layout-colwidth_optimize   = 'X'.
*  s_layout-coltab_fieldname    = 'COLOR'.  "Cor da linha
*  s_layout-box_fieldname       = 'CHK'.    "Checkbox



ENDFORM.                    " GET_LAYOUT_SORT
*&---------------------------------------------------------------------*
*&      Form  PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_data .

  DATA: ft_event_exit TYPE slis_t_event_exit,
        fs_event_exit TYPE slis_event_exit.

*  fs_event_exit-ucomm = 'REF'.
*  fs_event_exit-after = 'X'.
*  APPEND fs_event_exit TO ft_event_exit.

  gv_variant-report = sy-cprog.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-cprog
      i_callback_pf_status_set = 'ALV_ST'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      i_background_id          = 'ALV_BACKGROUND'
      it_sort                  = gs_sort
      i_save                   = 'A'
      is_variant               = gs_variant
      it_events                = gt_events
      it_event_exit            = ft_event_exit
    TABLES
      t_outtab                 = t_output[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " PRINT_DATA

*---------------------------------------------------------------------*
*       FORM STANDARD_FULLSCREEN                                      *
*---------------------------------------------------------------------*
*
*  -->  EXTAB                                                         *
*---------------------------------------------------------------------*
FORM alv_st USING extab TYPE slis_t_extab.

  SET PF-STATUS 'ALV_ST'.

ENDFORM.                    "STANDARD_FULLSCREEN
*--------------------------------------------------------------
*       FORM USER_COMMAND
*--------------------------------------------------------------
*
*--------------------------------------------------------------
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

*  CASE r_ucomm.
*
*    WHEN 'TRF'.
**     Transferência de mercadoria
*      PERFORM transferencia.
*      rs_selfield-refresh = 'X'.
**      SET USER-COMMAND '&OPT'.         " Optimize columns width
*
*  ENDCASE.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&       FORM TOP_OF_PAGE                                              *
*&---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_header.

ENDFORM.                    "top_of_page
*}   REPLACE
