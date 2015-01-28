;+
;
; NAME:
;   allo_list_struct
;
; AUTHOR:
;   Jeff Kolthammer, 20040913
;
; DESCRIPTION:
;   Structure definitions for imagio-formatted list data
;
;-
;

pro allo_compact4_event_struct, a
datum = {allo_compact4_event_struct, $
    eventNumber:0UL, $
    control:1b, $
    d:0b, $
    reserved:0b, $
    zb:0b, $
    za:0b, $
    phi:0b, $
    r:0b}
a = datum
end

pro allo_compact4_control_struct, a
control = {allo_compact4_control_struct, $
    eventNumber:0UL, $
    control:0b, $
    cwt:0b, $
    cwinfo: 0UL}
a = control
end

pro allo_resxtal_event_struct, a
datum = {allo_resxtal_event_struct, $
    eventNumber:0UL, $
        control:2b, $
        control2:1b, $
        d:0b, $
        reserved:0b, $
        reserved2:0b, $
        xa:0, $
        xb:0, $
        zb:0b, $
        za:0b, $
        leb:0b, $
        lea:0b}
a = datum
end

pro allo_resxtal_control_struct, a
control = {allo_resxtal_control_struct, $
    eventNumber:0UL, $
        control:0b, $
        control2:0b, $
        cwt:0b, $
        cwinfo: 0UL, $
        reserved: 0UL}
a = control
end

pro allo_lor8_event_struct, a
datum = {allo_lor8_event_struct, $
    eventNumber:0UL, $
        control:2b, $
        control2:1b, $
        d:0b, $
        reserved:0b, $
        reserved2:0b, $
        r:0b, $
        phi:0b, $
        zb:0b, $
        za:0b, $
        leb:0b, $
        lea:0b}
a = datum
end

pro allo_lor8_control_struct, a
control = {allo_lor8_control_struct, $
    eventNumber:0UL, $
        control:0b, $
        control2:0b, $
        cwt:0b, $
        cwinfo: 0UL, $
        reserved: 0UL}
a = control
end


