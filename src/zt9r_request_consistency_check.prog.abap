REPORT zt9r_request_consistency_check.
" https://github.com/tricktresor/request-consistency-check

DATA request TYPE e070.

SELECT-OPTIONS so_nam FOR request-as4user    DEFAULT sy-uname.
SELECT-OPTIONS so_dat FOR request-as4date    DEFAULT '20200101' TO sy-datum.
SELECT-OPTIONS so_req FOR request-trkorr.
SELECT-OPTIONS so_fnc FOR request-trfunction DEFAULT 'K'.
SELECT-OPTIONS so_sta FOR request-trstatus   DEFAULT 'D'.

CLASS app DEFINITION.
  PUBLIC SECTION.
    METHODS start.
  PRIVATE SECTION.
    TYPES: BEGIN OF _data,
             trkorr   TYPE trkorr,
             as4user  TYPE e070-as4user,
             as4date  TYPE e070-as4date,
             as4text  TYPE e07t-as4text,
             strkorr  TYPE strkorr,
             message  TYPE text100,
             pgmid    TYPE e071-pgmid,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
           END OF _data,
           _data_tab TYPE STANDARD TABLE OF _data WITH DEFAULT KEY.
    DATA result_table TYPE _data_tab.
    DATA requests TYPE SORTED TABLE OF e070 WITH UNIQUE KEY trkorr.
    DATA tasks TYPE SORTED TABLE OF e070 WITH UNIQUE KEY trkorr.
    DATA messages TYPE ctsgerrmsgs.
    METHODS select.
    METHODS prepare.
    METHODS display.
ENDCLASS.


CLASS app IMPLEMENTATION.
  METHOD start.
    select( ).
    prepare( ).
    display( ).
  ENDMETHOD.
  METHOD select.
    SELECT * FROM e070 INTO TABLE @requests
      WHERE trkorr     IN @so_req
        AND trfunction IN @so_fnc
        AND trstatus   IN @so_sta
        AND as4user    IN @so_nam
        AND as4date    IN @so_dat
      ORDER BY PRIMARY KEY.

    IF sy-subrc = 0 AND lines( requests  ) > 0.
      SELECT * FROM e070 INTO TABLE @tasks
         FOR ALL ENTRIES IN @requests
        WHERE strkorr = @requests-trkorr
        ORDER BY PRIMARY KEY.
    ENDIF.

  ENDMETHOD.

  METHOD prepare.

    DATA full_request  TYPE trwbo_request.

    LOOP AT requests INTO DATA(request).

      LOOP AT tasks INTO DATA(task) WHERE strkorr = request-trkorr.
        CLEAR full_request.
        full_request-h = CORRESPONDING #( task ).

        CALL FUNCTION 'TR_READ_REQUEST'
          EXPORTING
            iv_read_e070       = 'X'
            iv_read_e07t       = 'X'
            iv_read_e070c      = ' '
            iv_read_e070m      = ' '
            iv_read_objs_keys  = 'X'
            iv_read_attributes = ' '
            iv_trkorr          = task-trkorr
          CHANGING
            cs_request         = full_request
          EXCEPTIONS
            error_occured      = 1
            no_authorization   = 2
            OTHERS             = 3.
        IF sy-subrc <> 0.
          APPEND VALUE #(
            trkorr   = request-trkorr
            as4user  = request-as4user
            as4date  = request-as4date
            as4text  = full_request-h-as4text
            strkorr  = task-trkorr
            message  = 'Error TR_READ_REQUEST' ##no_Text
           ) TO result_table.
          CONTINUE.
        ENDIF.

        CLEAR messages.
        CALL FUNCTION 'TR_CHECK_REQUEST'
          EXPORTING
            is_request           = full_request
            iv_check_lockability = space
            iv_collect_mode      = 'X'
          IMPORTING
            et_messages          = messages.

        LOOP AT messages INTO DATA(message).
          MESSAGE ID message-msgid
                TYPE message-msgty
              NUMBER message-msgno
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
                INTO DATA(message_text).
          DATA(err) = VALUE #( full_request-objects[ message-pos ] OPTIONAL ).
          APPEND VALUE #(
            trkorr   = request-trkorr
            as4user  = request-as4user
            as4date  = request-as4date
            as4text  = full_request-h-as4text
            strkorr  = task-trkorr
            pgmid    = err-pgmid
            object   = err-object
            obj_name = err-obj_name
            message  = message_text
           ) TO result_table.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD display.

    IF result_table IS INITIAL.
      MESSAGE s477(tk) WITH 'selected'.
    ELSE.

      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table   = DATA(salv)
            CHANGING
              t_table        = result_table ).
          salv->get_functions( )->set_all( ).
          salv->get_columns( )->set_optimize( ).
          salv->display( ).
        CATCH cx_salv_msg INTO DATA(salv_error).
          MESSAGE salv_error TYPE 'I'.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  NEW app( )->start( ).
