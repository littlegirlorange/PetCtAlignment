pro dialog_select_event, sEvent

widget_control, sEvent.top, GET_UVALUE=info
case sEvent.id of
	info.wCancelButton: begin
		*info.pIndices = -1L
		widget_control, sEvent.top, /DESTROY
	end
	info.wSelectButton: begin
		*info.pIndices = widget_info( info.wList, /LIST_SELECT )
		widget_control, sEvent.top, /DESTROY
	end
	else:
endcase

end ; of dialog_select_event


function dialog_select, LIST=list, TITLE=title, MULTIPLE=multiple

if n_elements( list ) eq 0 then return, -1L
if n_elements( title ) eq 0 then title = 'Please make a selection'

wBase = widget_base( TITLE=title, /COLUMN, /ALIGN_CENTER )
if keyword_set( multiple ) then begin
	wSubBase = widget_base( wBase, /COLUMN, /ALIGN_CENTER )
	wLabel = widget_label( wSubBase, VALUE='Use CTRL to make multiple selections' )
endif else begin
	multiple = 0
endelse
wSubBase = widget_base( wBase, /ROW, /ALIGN_CENTER )
wListBase = widget_base( wSubBase, /COLUMN, /ALIGN_CENTER )
wList = widget_list( wListBase, VALUE=list, MULTIPLE=multiple, YSIZE=n_elements(list) )
wSubBase = widget_base( wBase, /ROW, /ALIGN_CENTER )
wSelectButton = widget_button( wSubBase, VALUE='Select', /NO_RELEASE )
wCancelButton = widget_button( wSubBase, VALUE='Cancel', /NO_RELEASE )

pIndices = ptr_new( /ALLOCATE_HEAP )
info = {wSelectButton:		wSelectButton, $
		wCancelButton:		wCancelButton, $
		wList: 				wList, $
		pIndices:			pIndices}
widget_control, wBase, SET_UVALUE=info, /NO_COPY
widget_control, wBase, /REALIZE

xmanager, 'dialog_select', wBase
if ptr_valid( pIndices ) then begin
	selections = *pIndices
	ptr_free, pIndices
endif else begin
	selection = -1L
endelse
return, selections

end ; of dialog_select