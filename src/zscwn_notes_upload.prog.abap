*&---------------------------------------------------------------------*
*& Report  ZSCWN_NOTES_UPLOAD
*& Load multiple notes files into SNOTE
*&---------------------------------------------------------------------*
*&
*& Reference:
*& Security Patch Process FAQ
*& http://scn.sap.com/community/security/blog/2012/03/27/security-patch-process-faq#23_What_should_I_do_if_I_cannot_download_a_note_into_SNOTE
*& 23. What should I do if I cannot download a note into SNOTE?
*&
*& Sometimes you run into trouble while downloading large notes in transaction SNOTE, like for the security note 1826162 from July 2013. (In addition this note requires another large note 1674132, too.)
*&
*& In such a case use the download basket to get the note:
*&
*& 1. Show the note on SMP, e.g. https://service.sap.com/sap/support/notes/1826162
*& 2. Use the button "Download Corrections". You get a new window showing a log.
*& 3. Repeat 1. and 2. for more notes, e.g. note 1826162 requires other note 1674132
*& 4. Use the button "Download Basket" on the log window to show your basket
*& 5. Click on every link for the selected notes to download the file via the internet browser (You could try to use the SAP Download Manager, however, this might not work as it uses the same interface like SNOTE.)
*& 6. Un-zip the archive files which you have downloaded
*& 7. In transaction SNOTE use the menu path Goto->Upload note to load the note(s) one by one
*& 8. Implement the note as usual
*&
*& Another advantage is, that you can use the same files for uploading notes into several development systems
*&
*& 19.08.2013 Initial version based on function SCWN_NOTE_UPLOAD_INTERNAL which allows the upload of a single file
*&
*&---------------------------------------------------------------------*

REPORT  ZSCWN_NOTES_UPLOAD.

constants: c_program_version(15) type c value '19.08.2013'.

selection-screen begin of line.
selection-screen comment 1(32) T_PATH for field s_path.
parameters: s_path type string LOWER CASE.
selection-screen end of line.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

*----------------------------------------------------------------------*

INITIALIZATION.
* Authorization check according to report SCWN_ENTRY_TREE (=SNOTE)
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'SNOTE'
    EXCEPTIONS
      ok     = 0
      not_ok = 1
      OTHERS = 2.

  IF sy-subrc <> 0.
*     All messages raised from authority_check_tcode means:
*     'This transaction can or should not be started.'
    MESSAGE i775(scwn) WITH 'SNOTE'.
    LEAVE PROGRAM.
  ENDIF.

  T_PATH  = 'Local path with text files'(000).

  concatenate 'Program version from'(000) c_program_version into SS_VERS
    SEPARATED BY SPACE.

* Get default download path
  data l_UPLOAD_PATH type string.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_UPLOAD_DOWNLOAD_PATH
    CHANGING
      UPLOAD_PATH                 = l_UPLOAD_PATH
      DOWNLOAD_PATH               = s_path
    EXCEPTIONS
      CNTL_ERROR                  = 1
      ERROR_NO_GUI                = 2
      NOT_SUPPORTED_BY_GUI        = 3
      GUI_UPLOAD_DOWNLOAD_PATH    = 4
      UPLOAD_DOWNLOAD_PATH_FAILED = 5
      others                      = 6.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*----------------------------------------------------------------------*

START-OF-SELECTION.

  data: lv_file_filter    TYPE string,
        lv_filename       TYPE string,
        lv_file_table     TYPE filetable,
        lv_rc             TYPE i,
        lv_user_action    TYPE i,
        lv_file           TYPE LINE OF filetable,
        lv_title          TYPE string.
  data: lt_cont           LIKE cwbdata OCCURS 0,
        lv_FILELENGTH     type i.

* dialog for file
  CLASS cl_gui_frontend_services DEFINITION LOAD.
* Definition of file types see text elements of class CL_GUI_FRONTEND_SERVICES
  lv_file_filter = cl_gui_frontend_services=>FILETYPE_TEXT.
  lv_title = 'Upload SAP Note'(100).

* Ask for list of files
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = lv_title
      INITIAL_DIRECTORY = s_path
*     default_filename  =  lv_filename
      file_filter       = lv_file_filter
      multiselection    = 'X'
    CHANGING
      file_table        = lv_file_table
      rc                = lv_rc
      user_action       = lv_user_action
    EXCEPTIONS
      OTHERS            = 1.

  IF sy-subrc <> 0 OR lv_rc <= 0.
    EXIT.
  ENDIF.

  IF lv_user_action = cl_gui_frontend_services=>action_cancel.
    MESSAGE s020(scwn).
    EXIT.
  ENDIF.

* process files
  data: lv_lines   type i,
        lv_percent type i,
        lv_message(132).
  describe table lv_file_table lines lv_lines.
  loop at lv_file_table into lv_filename.

    lv_percent = 100 * sy-tabix / lv_lines.
    concatenate 'Read file'(002) lv_filename into lv_message SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = lv_percent
        TEXT       = lv_message.

*   Read file
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_filename
        FILETYPE                = 'ASC'
      IMPORTING
        FILELENGTH              = lv_FILELENGTH
      TABLES
        data_tab                = lt_cont
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   Process file
    perform scwn_note_upload_internal
      tables lt_cont.

  endloop.

  message 'Files loaded'(003) type 'S'.

*FUNCTION scwn_note_upload_internal.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  EXCEPTIONS
*"      INCOMPATIBLE_VERSIONS
*"      CORRUPT_DATA_FILE
*"----------------------------------------------------------------------
constants: true type  BCWBN_BOOL value 'X',
           false type BCWBN_BOOL value space.
*
FORM scwn_note_upload_internal
    tables lt_cont. "       LIKE cwbdata OCCURS 0.

  FIELD-SYMBOLS: <ls_cwbnthead> TYPE cwbnthead.

  DATA: lv_cancel,
        lv_filetype   LIKE rlgrap-filetype,
*        lt_cont       LIKE cwbdata OCCURS 0,
        lt_cwbnthead  LIKE cwbnthead OCCURS 0,
        lt_cwbntstxt  LIKE cwbntstxt OCCURS 0,
        lt_cwbntdata  TYPE bcwbn_note_text OCCURS 0,
        lt_cwbntvalid LIKE cwbntvalid OCCURS 0,
        lt_cwbntci    LIKE cwbntci OCCURS 0,
        lt_cwbntfixed LIKE cwbntfixed OCCURS 0,
        lt_cwbntgattr LIKE cwbntgattr OCCURS 0,
        lt_cwbcihead  LIKE cwbcihead OCCURS 0,
        lt_cwbcidata  TYPE bcwbn_cinst_delta OCCURS 0,
        lt_cwbcivalid LIKE cwbcivalid OCCURS 0,
        lt_cwbciinvld LIKE cwbciinvld OCCURS 0,
        lt_cwbcifixed LIKE cwbcifixed OCCURS 0,
        lt_cwbcidpndc LIKE cwbcidpndc OCCURS 0,
        lt_cwbciobj   LIKE cwbciobj OCCURS 0,
        lt_cwbcmpnt   LIKE cwbcmpnt OCCURS 0,
        lt_cwbcmtext  LIKE cwbcmtext OCCURS 0,
        lt_cwbcmlast  LIKE cwbcmlast OCCURS 0,
        lt_cwbdehead  LIKE cwbdehead OCCURS 0,
        lt_cwbdeprdc  LIKE cwbdeprdc OCCURS 0,
        lt_cwbdetrack LIKE cwbdetrack OCCURS 0,
        lt_cwbdeequiv LIKE cwbdeequiv OCCURS 0,
        lt_cwbcidata_ref TYPE cwb_deltas,
*        lv_file_filter   TYPE string,
*        lv_filename TYPE string,
*        lv_file_table TYPE filetable,
*        lv_rc TYPE i,
*        lv_user_action TYPE i,
        lv_file TYPE LINE OF filetable,
*        lv_title TYPE string,
        ls_note TYPE bcwbn_note,
        lt_notes TYPE bcwbn_notes,
        ls_cwbnthead LIKE cwbnthead.

  DATA: lv_data_bin        TYPE xstring,
        lv_code_delta_bin  TYPE xstring,
        lt_object_data_bin TYPE cwbci_t_objdelta,
        ls_numm_versno     TYPE cwbntkeyvs.

** dialog for file
*  CLASS cl_gui_frontend_services DEFINITION LOAD.
*  lv_file_filter = cl_gui_frontend_services=>filetype_all.
*  lv_title = text-100.
*
*  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*    EXPORTING
*      window_title     = lv_title
*      default_filename = lv_filename
*      file_filter      = lv_file_filter
*    CHANGING
*      file_table       = lv_file_table
*      rc               = lv_rc
*      user_action      = lv_user_action
*    EXCEPTIONS
*      OTHERS           = 1.
*
*  IF sy-subrc <> 0 OR lv_rc <= 0.
*    EXIT.
*  ENDIF.
*
*  IF lv_user_action = cl_gui_frontend_services=>action_cancel.
*    MESSAGE s020(scwn).
*    EXIT.
*  ENDIF.
*
** upload file
*  READ TABLE lv_file_table INTO lv_file INDEX 1.
*  lv_filename = lv_file-filename.
*
*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      filename                = lv_filename
*    TABLES
*      data_tab                = lt_cont_bin
*    EXCEPTIONS
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      OTHERS                  = 17.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

  CALL FUNCTION 'SCWN_NOTE_UNPACK'
    IMPORTING
      ev_data_bin           = lv_data_bin
      ev_code_delta_bin     = lv_code_delta_bin
      et_object_data_bin    = lt_object_data_bin
    TABLES
      tt_cont               = lt_cont
    EXCEPTIONS
      incompatible_versions = 1
      corrupt_data_file     = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'SCWN_NOTE_UNPACK_XML'
    EXPORTING
      iv_data_bin           = lv_data_bin
      iv_code_delta_bin     = lv_code_delta_bin
      it_object_data_bin    = lt_object_data_bin
    IMPORTING
      et_cwbnthead          = lt_cwbnthead
      et_cwbntstxt          = lt_cwbntstxt
      et_cwbntdata          = lt_cwbntdata
      et_cwbntvalid         = lt_cwbntvalid
      et_cwbntci            = lt_cwbntci
      et_cwbntfixed         = lt_cwbntfixed
      et_cwbntgattr         = lt_cwbntgattr
      et_cwbcihead          = lt_cwbcihead
      et_cwbcidata          = lt_cwbcidata
      et_cwbcidata_ref      = lt_cwbcidata_ref
      et_cwbcivalid         = lt_cwbcivalid
      et_cwbciinvld         = lt_cwbciinvld
      et_cwbcifixed         = lt_cwbcifixed
      et_cwbcidpndc         = lt_cwbcidpndc
      et_cwbciobj           = lt_cwbciobj
      et_cwbcmpnt           = lt_cwbcmpnt
      et_cwbcmtext          = lt_cwbcmtext
      et_cwbcmlast          = lt_cwbcmlast
      et_cwbdehead          = lt_cwbdehead
      et_cwbdeprdc          = lt_cwbdeprdc
      et_cwbdetrack         = lt_cwbdetrack
      et_cwbdeequiv         = lt_cwbdeequiv
    EXCEPTIONS
      corrupt_data_file     = 1
      incompatible_versions = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING corrupt_data_file.
  ENDIF.

* store note
  CALL FUNCTION 'SCWN_NOTE_STORE'
    EXPORTING
      it_cwbcmpnt      = lt_cwbcmpnt
      it_cwbdetrack    = lt_cwbdetrack
      it_cwbdehead     = lt_cwbdehead
      it_cwbdeequiv    = lt_cwbdeequiv
    TABLES
      tt_cwbnthead     = lt_cwbnthead
      tt_cwbntstxt     = lt_cwbntstxt
      tt_cwbntdata     = lt_cwbntdata
      tt_cwbntvalid    = lt_cwbntvalid
      tt_cwbntci       = lt_cwbntci
      tt_cwbntfixed    = lt_cwbntfixed
      tt_cwbntgattr    = lt_cwbntgattr
      tt_cwbcihead     = lt_cwbcihead
      tt_cwbcidata     = lt_cwbcidata
      tt_cwbcidata_ref = lt_cwbcidata_ref
      tt_cwbcivalid    = lt_cwbcivalid
      tt_cwbciinvld    = lt_cwbciinvld
      tt_cwbcifixed    = lt_cwbcifixed
      tt_cwbcidpndc    = lt_cwbcidpndc
      tt_cwbciobj      = lt_cwbciobj
    EXCEPTIONS
      failure          = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
*   store each stored note in download history
    LOOP AT lt_cwbnthead ASSIGNING <ls_cwbnthead>.
      ls_numm_versno-numm = <ls_cwbnthead>-numm.
      ls_numm_versno-versno = <ls_cwbnthead>-versno.
      CALL FUNCTION 'SCWN_NOTE_DOWNLOAD_HIST'
        EXPORTING
          is_note_version = ls_numm_versno
          iv_mode_write   = true
        EXCEPTIONS
          OTHERS          = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.
  ENDIF.

* update software component if necessary
  CALL FUNCTION 'SCWN_UPDATE_SOFTWARE_COMPONENT'
    TABLES
      tt_cwbcmpnt   = lt_cwbcmpnt
      tt_cwbcmtext  = lt_cwbcmtext
      tt_cwbcmlast  = lt_cwbcmlast
      tt_cwbdehead  = lt_cwbdehead
      tt_cwbdeprdc  = lt_cwbdeprdc
      tt_cwbdetrack = lt_cwbdetrack
      tt_cwbdeequiv = lt_cwbdeequiv
    EXCEPTIONS
      failure       = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* classify notes
  LOOP AT lt_cwbnthead INTO ls_cwbnthead.
    ls_note-key-numm = ls_cwbnthead-numm.
    ls_note-key-versno = ls_cwbnthead-versno.
    APPEND ls_note TO lt_notes.
  ENDLOOP.

  CALL FUNCTION 'SCWB_NOTES_CLASSIFY'
    EXPORTING
      it_notes        = lt_notes
      iv_set_ntstatus = ' '.

ENDFORM.
*ENDFUNCTION.
