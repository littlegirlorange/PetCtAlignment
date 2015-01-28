pro select_contours_event, sEvent

widget_control, sEvent.top, GET_UVALUE=info
if sEvent.id eq info.wLoadButton then begin
    widget_control, sEvent.top, /DESTROY
    return
endif else if sEvent.id eq info.wList then begin
	*info.pContourIndices = widget_info( long(info.wList), /LIST_SELECT )
endif

end ; of select_contours_event


function select_contours, rsFile, ALL_ROIS=allROIs

obj = obj_new( 'IDLffDICOM' )
bOk  = obj->read( rsFile )
nameRefs = obj->getReference( '3006'x, '0026'x )
if nameRefs[0] ne -1 then begin
	values = obj->getValue( REFERENCE=nameRefs )
	nConts = n_elements( values )
	names = strarr( nConts )
	for i=0, nConts-1 do names[i] = *values[i]
endif
obj_destroy, obj
ptr_free, values

if keyword_set( allROIs ) then begin
	return, indgen(nConts)
endif

wBase = widget_base( TITLE='Select contours to save', /COLUMN, /ALIGN_CENTER )
wSubBase = widget_base( wBase, /COLUMN, /ALIGN_CENTER )
wLabel = widget_label( wSubBase,value = 'Use CTRL to make multiple selections' )
wSubBase = widget_base( wBase, /ROW, /ALIGN_CENTER )
wListBase = widget_base( wSubBase, /COLUMN, /ALIGN_CENTER )



wList = widget_list( wListBase, VALUE=names, /MULTIPLE, YSIZE=nConts  )

wSubBase = widget_base( wBase, /COLUMN, /ALIGN_CENTER )
wLoadButton = widget_button( wSubBase, VALUE='Load', /NO_RELEASE )

pContourIndices = ptr_new(/ALLOCATE_HEAP)
info = {wLoadButton:		wLoadButton, $
		wList: 				wList, $
		pContourIndices:	pContourIndices }
widget_control, wBase, SET_UVALUE=info, /NO_COPY
widget_control, wBase, /REALIZE

xmanager, 'select_contours', wBase
selection = *pContourIndices
ptr_free, pContourIndices
return, selection

end ; of select_contours