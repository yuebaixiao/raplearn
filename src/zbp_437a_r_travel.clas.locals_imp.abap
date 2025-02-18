CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS setcancel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~setcancel RESULT rs.
    METHODS allAc FOR MODIFY
      IMPORTING keys FOR ACTION Travel~allAc.
    METHODS copyLine FOR MODIFY
      IMPORTING keys FOR ACTION Travel~copyLine.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Travel RESULT result.
    METHODS validateStatus FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateStatus.
    METHODS validateDelete FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDelete.
    METHODS determineKey FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~determineKey.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR travel RESULT result.

    METHODS is_allowed
      RETURNING VALUE(rec) TYPE abap_bool.
ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD setcancel.

    DATA lo_msg TYPE REF TO zcm_2463_travel.
    DATA ls_reported_travel LIKE LINE OF reported-travel.

    DATA ls_failed_check LIKE LINE OF failed-travel.

    DATA lt_ri TYPE TABLE FOR READ IMPORT z437A_R_TRAVEL.
    DATA ls_read_in LIKE LINE OF lt_ri.

    DATA lt_travel TYPE TABLE FOR READ RESULT z437A_R_TRAVEL.

    DATA lt_up TYPE TABLE FOR UPDATE z437A_R_TRAVEL.
    DATA ls_up LIKE LINE OF lt_up.

    DATA ls_failed TYPE RESPONSE FOR FAILED z437A_R_TRAVEL.

    FIELD-SYMBOLS <ls_key> LIKE LINE OF keys.
    FIELD-SYMBOLS <ls_travel> LIKE LINE OF lt_travel.

    DATA ls_key LIKE LINE OF keys.

    LOOP AT keys ASSIGNING <ls_key>.
      MOVE-CORRESPONDING <ls_key> TO ls_read_in.
      APPEND ls_read_in TO lt_ri.
    ENDLOOP.

    READ ENTITY IN LOCAL MODE z437A_R_TRAVEL
      ALL FIELDS WITH lt_ri
      RESULT lt_travel.

*    READ ENTITIES OF z437A_R_TRAVEL IN LOCAL MODE
*    ENTITY travel
*    ALL FIELDS WITH lt_ri
*    RESULT lt_travel.

    LOOP AT lt_travel ASSIGNING <ls_travel>.
      IF <ls_travel>-Status = 'C'.
        CREATE OBJECT lo_msg
          EXPORTING
            textid   = zcm_2463_travel=>already_cancelled
            severity = if_abap_behv_message=>severity-error.
        ls_reported_travel-%tky = <ls_travel>-%tky.
        ls_reported_travel-%msg = lo_msg.

        APPEND ls_reported_travel TO reported-travel.

        ls_failed_check-%tky = <ls_travel>-%tky.
        APPEND ls_failed_check TO failed-travel.


      ELSE.
        CLEAR lt_up.
        ls_up-%tky = <ls_travel>-%tky.
        ls_up-status = 'C'.
        APPEND ls_up TO lt_up.

        MODIFY ENTITY IN LOCAL MODE z437A_R_TRAVEL
          UPDATE FIELDS ( Status ) WITH lt_up
          FAILED ls_failed.

*        MODIFY ENTITIES OF z437A_R_TRAVEL IN LOCAL MODE
*        ENTITY travel
*        UPDATE FIELDS ( Status ) WITH lt_up
*        FAILED ls_failed.

        IF ls_failed IS INITIAL.
          CREATE OBJECT lo_msg
            EXPORTING
              textid   = zcm_2463_travel=>cancel_success
              severity = if_abap_behv_message=>severity-success.

          ls_reported_travel-%tky = <ls_travel>-%tky.
          ls_reported_travel-%msg = lo_msg.
          APPEND ls_reported_travel TO reported-travel.

          INSERT VALUE #( %tky = <ls_travel>-%tky %param = <ls_travel> ) INTO TABLE rs.
        ENDIF.

      ENDIF.
    ENDLOOP.


  ENDMETHOD.



  METHOD allAc.

    SELECT FROM z2463_dtravel
    FIELDS * INTO TABLE @DATA(lt_result).

    LOOP AT lt_result INTO DATA(ls).
      IF ls-status = 'C'.
        MODIFY ENTITY IN LOCAL MODE z437A_R_TRAVEL
*        UPDATE FIELDS ( status ) WITH VALUE #( ( %tky-AgencyID = ls-agency_id %tky-TravelId = ls-travel_id Status = 'A' ) )
        UPDATE FIELDS ( status ) WITH VALUE #( ( %tky-uuid = ls-uuid Status = 'A' ) )
        FAILED DATA(ls_failed).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validateStatus.

    READ ENTITY IN LOCAL MODE z437A_R_TRAVEL
    FIELDS ( Status ) WITH CORRESPONDING #( keys )
    RESULT DATA(travels)
    FAILED DATA(fal).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<tr>).
      IF <tr>-Status IS INITIAL.
        APPEND VALUE #( %tky = <tr>-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = <tr>-%tky
        %msg = NEW zcm_2463_travel(
          textid = zcm_2463_travel=>field_empty
        )
        %element-status = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validateDelete.

    DATA lt_ri TYPE TABLE FOR READ IMPORT z437A_R_TRAVEL.
    DATA ls_read_in LIKE LINE OF lt_ri.


    FIELD-SYMBOLS <ls_key> LIKE LINE OF keys.

    DATA ls_key LIKE LINE OF keys.

    LOOP AT keys ASSIGNING <ls_key>.
      MOVE-CORRESPONDING <ls_key> TO ls_read_in.
      APPEND ls_read_in TO lt_ri.
    ENDLOOP.
*    READ ENTITY IN LOCAL MODE z437A_R_TRAVEL
*    FIELDS ( AgencyId ) WITH lt_ri
*    RESULT DATA(travels).

    READ ENTITIES OF z437A_R_TRAVEL IN LOCAL MODE
    ENTITY travel
    ALL FIELDS WITH lt_ri

    RESULT DATA(travels)
    FAILED DATA(fal).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<tr>).
      IF <tr>-Status = 'A'.
        APPEND VALUE #( %tky = <tr>-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = <tr>-%tky
        %msg = NEW zcm_2463_travel(
          textid = zcm_2463_travel=>field_empty
        )
        %element-status = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD determineKey.

    DATA lt_travel_upd TYPE TABLE FOR UPDATE z437A_R_TRAVEL.

    lt_travel_upd = CORRESPONDING #( keys ).

    DATA(lv_agencyid) = cl_abap_context_info=>get_user_technical_name( ).
    LOOP AT lt_travel_upd ASSIGNING FIELD-SYMBOL(<ls_travel_upd>).
      <ls_travel_upd>-Description = lv_agencyid.
    ENDLOOP.

    MODIFY ENTITY IN LOCAL MODE z437A_R_TRAVEL
    UPDATE FIELDS ( Description )
    WITH lt_travel_upd
    REPORTED DATA(ls_reported).

    MOVE-CORRESPONDING ls_reported-travel TO reported-travel.
  ENDMETHOD.

  METHOD get_instance_features.
    READ ENTITY IN LOCAL MODE z437A_R_TRAVEL
    FIELDS ( status begindate enddate )
    WITH CORRESPONDING #( keys )
    RESULT DATA(travels).

    DATA(dt) = cl_abap_context_info=>get_system_date( ).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      APPEND CORRESPONDING #( <travel> ) TO result
      ASSIGNING FIELD-SYMBOL(<result>).

      IF <travel>-Status = 'C'.
        <result>-%action-setcancel = if_abap_behv=>fc-o-disabled.
        <result>-%update = if_abap_behv=>fc-o-disabled.
      ENDIF.

      IF <travel>-begindate IS NOT INITIAL AND
      <travel>-begindate < dt.

        <result>-%field-customerid = if_abap_behv=>fc-f-read_only.
        <result>-%field-begindate = if_abap_behv=>fc-f-read_only.

      ELSE.

        <result>-%field-customerid = if_abap_behv=>fc-f-mandatory.
        <result>-%field-begindate = if_abap_behv=>fc-f-mandatory.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD copyLine.
  ENDMETHOD.

  METHOD get_instance_authorizations.


    DATA rs LIKE LINE OF result.

    READ ENTITY IN LOCAL MODE z437A_R_TRAVEL
    FIELDS ( Status ) WITH CORRESPONDING #( keys )
    RESULT DATA(travels)
    FAILED DATA(fal).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<fs>).
      IF requested_authorizations-%update = if_abap_behv=>mk-on
      AND <fs>-Status = 'A'.
        rs-%tky = <fs>-%tky.
*        rs-%update = if_abap_behv=>auth-unauthorized.
        rs-%update = if_abap_behv=>auth-allowed.
        APPEND rs TO result.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_global_authorizations.


    DATA(rec) = is_allowed(  ).

    IF requested_authorizations-%create = if_abap_behv=>mk-on.
      IF ( rec = abap_true ).
        result-%create = if_abap_behv=>auth-allowed.
      ELSE.
        result-%create = if_abap_behv=>auth-unauthorized.
      ENDIF.
    ENDIF.
    IF requested_authorizations-%update = if_abap_behv=>mk-on.
      IF ( rec = abap_true ).
        result-%update = if_abap_behv=>auth-allowed.
      ELSE.
        result-%update = if_abap_behv=>auth-unauthorized.
        result-%action-allAc = if_abap_behv=>auth-unauthorized.
        result-%create = if_abap_behv=>auth-unauthorized.
      ENDIF.
    ENDIF.
    IF requested_authorizations-%delete = if_abap_behv=>mk-on.
      IF ( rec = abap_true ).
        result-%delete = if_abap_behv=>auth-allowed.
      ELSE.
        result-%delete = if_abap_behv=>auth-unauthorized.
      ENDIF.
    ENDIF.
    IF requested_authorizations-%action-setcancel = if_abap_behv=>mk-on.
      IF ( rec = abap_true ).
        result-%action-setcancel = if_abap_behv=>auth-allowed.
      ELSE.
        result-%action-setcancel = if_abap_behv=>auth-unauthorized.
      ENDIF.
    ENDIF.
    IF requested_authorizations-%action-allAc = if_abap_behv=>mk-on.
      IF ( rec = abap_true ).
        result-%action-allAc = if_abap_behv=>auth-allowed.
      ELSE.
        result-%action-allAc = if_abap_behv=>auth-unauthorized.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD is_allowed.
*    rec = abap_false.
    rec = abap_true.
  ENDMETHOD.


ENDCLASS.
