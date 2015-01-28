;==============================================================================
;
;	User Configuration Settings
;

; The top data directory
   MAIN_DIR  = "C:\Users\makusan\Documents\Research\DynamicPET\Sunnybrook Data\"
; The directory to read from
   READ_DIR  = main_dir
; The directory to write to
   WRITE_DIR   = main_dir + "Coreg\"
; The directory to write temporary files to (will be deleted by program)
   TEMP_DIR = write_dir
; The subfolders in READ_DIR to search through for data
   CT_DIR   = "CTAC\"
   PET_DIR  = "PET\"
   RS_DIR   = "CTAC\"
; File name filters to apply when searching for data
   CT_FILTER  = "CT*.syn"
   PET_FILTER = "*ctac_zm.img"
   RS_FILTER  = "*Claron.art*.dcm" ; E.g., "*.*.img" for Ian Poon's contours, 
                                   ; "*Claron.art.dcm" for Lesion Segmenter arteries,
                                   ; "*Claron.vein.dcm" for veins

; Use nearest neighbour sampling for PET data.
bNN = 1b

; Write images?
bWRITEIMAGES = 0b
bWRITEROIS = 1b

; Display intermediate images for debugging purposes?
bDISPLAY = 0b

; Claron ROI rename name
CLARON_ROI_NAME = "artery" ; E.g., "artery" for arteries and "vein" for veins
                           ; or "vessel" for non-specific vessel