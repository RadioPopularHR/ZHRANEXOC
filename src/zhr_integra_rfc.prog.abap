*&---------------------------------------------------------------------*
*& Report  YHR00127INTPG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zhr_integra_rfc.

TABLES: zanexo_rfc.
**********************************************************************
* Variáveis auxiliares
**********************************************************************
DATA: t_excel1 TYPE TABLE OF zanexoc_est WITH HEADER LINE.
DATA: BEGIN OF t_tabint OCCURS 0,
        nif LIKE zanexo_rfc-nif,
        niss LIKE zanexo_rfc-niss,
        ano LIKE zanexo_rfc-ano,
        cae LIKE zanexo_rfc-cae,
        nissemp LIKE zanexo_rfc-nissemp,
        nome LIKE zanexo_rfc-nome,
        msg(100),
      END OF t_tabint.
**********************************************************************
* Variaveis Globais
**********************************************************************
DATA: G_NIF TYPE ZANEXO_RFC-NIF,
      G_NISS TYPE ZANEXO_RFC-NISS,
      G_ANO TYPE ZANEXO_RFC-ANO,
      G_COLAB TYPE ZANEXO_RFC-COLAB,
      G_CAE TYPE ZANEXO_RFC-CAE,
      G_REGIME TYPE ZANEXO_RFC-REGIME,
      G_NISSEMP TYPE ZANEXO_RFC-NISSEMP,
      G_NOME TYPE ZANEXO_RFC-NOME,
      G_SITUACAO TYPE ZANEXO_RFC-SITUACAO.

*********************************************************************
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
DATA: l_action    TYPE i,                " Benutzeraktion
      l_filetab   TYPE filetable,        " Tabelle mit Dateinamen
      lw_filetab  TYPE LINE OF filetable,
      l_rc        TYPE i,                " Anzahl gefundener
      file_path   TYPE string.
TYPE-POOLS truxs.
DATA fw_tab_raw  TYPE truxs_t_text_data.
**********************************************************************
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-tit.
PARAMETERS : p_file1 TYPE RLGRAP-FILENAME.
SELECTION-SCREEN : END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file1.
  if p_file1 IS NOT INITIAL.
    move p_file1 to file_path.
  endif.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Importar registro de file local'
      default_filename        = file_path
      default_extension       = 'xls'
      file_filter             = '(*.xls)|*.xls|(*.*)|*.*'
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
  if sy-subrc = 0.
    read TABLE l_filetab index 1 INTO lw_filetab.
    p_file1 = lw_filetab-filename.
  endif.

START-OF-SELECTION.

  FREE: t_excel1, t_tabint.

  IF p_file1 IS NOT INITIAL.

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_field_seperator    = 'X'
*      i_line_header        = line_header
        i_tab_raw_data       = fw_tab_raw
        i_filename           = p_file1
      TABLES
        i_tab_converted_data = t_excel1[]
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE e001(zpg) WITH 'Erro de conversão'.
      WHEN 2.
        MESSAGE e001(zpg) WITH 'Erro desconhecido'.
    ENDCASE.
  ENDIF.

END-OF-SELECTION.

  IF t_excel1[] IS NOT INITIAL.
    PERFORM trata_dados.
  ENDIF.

  CHECK t_excel1[] IS NOT INITIAL.

  IF t_tabint[] IS INITIAL.
    MESSAGE i001(ypg) WITH 'Não foram seleccionados dados'.
  ELSE.
    PERFORM gera_listagem.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GERA_LISTAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_listagem .

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

ENDFORM.                    " GERA_LISTAGEM
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
        fv_info TYPE slis_entry.

  REFRESH gt_header.
  CLEAR   gt_header.

*  WRITE g_begda TO lv_auxdt DD/MM/YYYY.
*  WRITE g_endda TO lv_auxdt2 DD/MM/YYYY.
  lv_tit1 = 'Integração dados Anexo C'.

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
      i_internal_tabname     = 'T_TABINT'
      i_inclname             = sy-cprog
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.


  LOOP AT t_fieldcat ASSIGNING <ffs_fieldcat>.
    CASE <ffs_fieldcat>-fieldname.

      WHEN 'NIF'.
        <ffs_fieldcat>-seltext_l    = 'NIF'.
        <ffs_fieldcat>-seltext_m    = 'NIF'.
        <ffs_fieldcat>-seltext_s    = 'NIF'.
        <ffs_fieldcat>-reptext_ddic = 'NIF'.
        <ffs_fieldcat>-col_pos = sy-tabix.

      WHEN 'NISS'.
        <ffs_fieldcat>-seltext_l    = 'NISS'.
        <ffs_fieldcat>-seltext_m    = 'NISS'.
        <ffs_fieldcat>-seltext_s    = 'NISS'.
        <ffs_fieldcat>-reptext_ddic = 'NISS'.
        <ffs_fieldcat>-col_pos = sy-tabix.

      WHEN 'ANO'.
        <ffs_fieldcat>-seltext_l    = 'Ano'.
        <ffs_fieldcat>-seltext_m    = 'Ano'.
        <ffs_fieldcat>-seltext_s    = 'Ano'.
        <ffs_fieldcat>-reptext_ddic = 'Ano'.
        <ffs_fieldcat>-col_pos = sy-tabix.

      WHEN 'CAE'.
        <ffs_fieldcat>-seltext_l    = 'CAE'.
        <ffs_fieldcat>-seltext_m    = 'CAE'.
        <ffs_fieldcat>-seltext_s    = 'CAE'.
        <ffs_fieldcat>-reptext_ddic = 'CAE'.
        <ffs_fieldcat>-col_pos = sy-tabix.

      WHEN 'NISSEMP'.
        <ffs_fieldcat>-seltext_l    = 'NISS Empregado'.
        <ffs_fieldcat>-seltext_m    = 'NISS Empregado'.
        <ffs_fieldcat>-seltext_s    = 'NISS Empregado'.
        <ffs_fieldcat>-reptext_ddic = 'NISS Empregado'.
        <ffs_fieldcat>-col_pos = sy-tabix.

      WHEN 'NOME'.
        <ffs_fieldcat>-seltext_l    = 'Nome'.
        <ffs_fieldcat>-seltext_m    = 'Nome'.
        <ffs_fieldcat>-seltext_s    = 'Nome'.
        <ffs_fieldcat>-reptext_ddic = 'Nome'.
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
      t_outtab                 = t_tabint[]
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
*&---------------------------------------------------------------------*
*&      Form  TRATA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trata_dados .
  DATA: lt_rfc TYPE ZANEXO_RFC.

  LOOP AT t_excel1.
    if t_excel1-NIF IS NOT INITIAL.
      G_NIF = t_excel1-NIF.
      G_NISS = t_excel1-NISS.
      G_ANO = t_excel1-ANO.
      G_COLAB = t_excel1-COLAB.
      G_CAE = t_excel1-CAE.
    else.
      t_excel1-NIF = G_NIF.
      t_excel1-NISS = G_NISS.
      t_excel1-ANO = G_ANO.
      t_excel1-COLAB = G_COLAB.
      t_excel1-CAE = G_CAE.
    ENDIF.
    if t_excel1-NISSEMP IS NOT INITIAL.
      G_REGIME = t_excel1-REGIME.
      G_NISSEMP = t_excel1-NISSEMP.
      G_NOME = t_excel1-NOME.
      G_SITUACAO = t_excel1-SITUACAO.
    else.
      t_excel1-REGIME = G_REGIME.
      t_excel1-NISSEMP = G_NISSEMP.
      t_excel1-NOME = G_NOME.
      t_excel1-SITUACAO = G_SITUACAO.
    endif.
    CLEAR t_tabint.
    MOVE-CORRESPONDING t_excel1 TO t_tabint.
    MOVE-CORRESPONDING t_excel1 to lt_rfc.
    insert ZANEXO_RFC FROM lt_rfc.
    IF sy-subrc = 0.
      t_tabint-msg = 'Registo foi criado correctamente'.
    ELSE.
      UPDATE zanexo_rfc FROM lt_rfc.
      t_tabint-msg = 'Registo foi actualizado'.
    ENDIF.
    APPEND t_tabint.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " TRATA_DADOS
